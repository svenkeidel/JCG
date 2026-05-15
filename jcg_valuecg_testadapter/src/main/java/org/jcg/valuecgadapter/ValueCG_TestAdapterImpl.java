package org.jcg.valuecgadapter;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.apache.commons.io.FileUtils;

public class ValueCG_TestAdapterImpl {

	public static final String ALGO_PRECISE = "precise";
	public static final String ALGO_FAST = "fast";
	public static final String ALGO_PRECISE_SERVER_CONF = "valdroidcg-precise.conf"; // relative to config directory
																						// defined in VALUECG_CONFIG_DIR
																						// env variable
	public static final String ALGO_FAST_SERVER_CONF = "valdroidcg-fast.conf";

	// ---- JCG Adapter: TOOL-INDEPENDENT FORMAT ----

	/**
	 * Representation of a method in the JCG format.
	 * Contains method name, declaring class (in JVM format), return type and
	 * parameter types (all JVM-formatted).
	 */
	public static class Method {
		public String name, declaringClass, returnType;
		public List<String> parameterTypes;

		public Method(String name, String declaringClass, String returnType, List<String> parameterTypes) {
			this.name = name;
			this.declaringClass = declaringClass;
			this.returnType = returnType;
			this.parameterTypes = parameterTypes;
		}

		@Override
		public boolean equals(Object o) {
			if (!(o instanceof Method))
				return false;
			Method m = (Method) o;
			return Objects.equals(name, m.name) && Objects.equals(declaringClass, m.declaringClass)
					&& Objects.equals(returnType, m.returnType) && Objects.equals(parameterTypes, m.parameterTypes);
		}

		@Override
		public int hashCode() {
			return Objects.hash(name, declaringClass, returnType, parameterTypes);
		}
	}

	public static class MethodTarget extends Method {
		public String sourceICCStatement;
		public String destICCClass;
		public String reasoning;
		public String kind;

		public MethodTarget(String sourceICCStatement, String destICCClass, String reasoning, String kind, String name,
				String declaringClass, String returnType, List<String> parameterTypes) {
			super(name, declaringClass, returnType, parameterTypes);
			this.sourceICCStatement = sourceICCStatement;
			this.destICCClass = destICCClass;
			this.reasoning = reasoning;
			this.kind = kind;
		}

		public static MethodTarget from(Edge edge, Method m) {
			MethodTarget t = new MethodTarget(edge.sourceICCStatement, edge.destICCClass, edge.reasoning, edge.kind,
					m.name, m.declaringClass, m.returnType, m.parameterTypes);
			return t;
		}
	}

	/**
	 * Representation of a call site in the JCG format.
	 * Contains the declared target, source line, bytecode offset (if available),
	 * and the set of possible targets.
	 */
	public static class CallSite {
		public String sourceStatement;
		public Method declaredTarget;
		public int line;
		public Integer pc;
		public Set<MethodTarget> targets;

		public CallSite(String sourceStatement, Method declaredTarget, int line, Integer pc,
				Set<MethodTarget> targets) {
			this.sourceStatement = sourceStatement;
			this.declaredTarget = declaredTarget;
			this.line = line;
			this.pc = pc;
			this.targets = targets;
		}
	}

	/**
	 * Representation of a reachable method and its outgoing call sites in the JCG
	 * format.
	 */
	public static class ReachableMethod {
		public Method method;
		public Set<CallSite> callSites;

		public ReachableMethod(Method method, Set<CallSite> callSites) {
			this.method = method;
			this.callSites = callSites;
		}
	}

	/**
	 * Container for all reachable methods, as required by the JCG format.
	 */
	public static class ReachableMethods {
		public Set<ReachableMethod> reachableMethods;

		public ReachableMethods(Set<ReachableMethod> reachableMethods) {
			this.reachableMethods = reachableMethods;
		}
	}

	// ---- JCG Adapter Entry Point ----

	/**
	 * Entry point: Converts a ValueCG call graph into the JCG ReachableMethods JSON
	 * format and writes it to the given output Writer.
	 * 
	 * @return The runtime in nanoseconds.
	 */
	public long serializeCG(String algorithm, String inputDirPath, Writer output, String mainClass, String[] classPath,
			String JDKPath, boolean analyzeJDK) throws Exception {

		System.out.println("algorithm=" + algorithm + ", inputDirPath=" + inputDirPath + ", output=" + output
				+ ", mainClass=" + mainClass + ", classPath=" + java.util.Arrays.toString(classPath) + ", JDKPath="
				+ JDKPath + ", analyzeJDK=" + analyzeJDK);

		long start = System.nanoTime();

		// Get ValueCG binary dir
		String runnerDir = System.getenv("VALUECG_RUNNER_DIR");
		if (runnerDir == null) {
			throw new IllegalStateException("VALUECG_RUNNER_DIR env variable not set");
		}
		String configDir = System.getenv("VALUECG_CONFIG_DIR");
		if (configDir == null) {
			throw new IllegalStateException("VALUECG_CONFIG_DIR env variable not set");
		}

		// Generate callgraph
		long processed = 0;
		File inputFile = new File(inputDirPath); // inputDirPath is the single .apk or .jar file that we want to
													// generate the CG for
		String testCaseName = readTestCaseName(inputFile);

		Path outDir = Files.createTempDirectory("output-cgs");
		Path cgDir = outDir.resolve(algorithm, testCaseName);

		// Read and convert generated callgraph files
		try {

			processed += generateCGforFile(inputFile, algorithm, runnerDir, configDir, cgDir, mainClass, classPath, JDKPath,
					analyzeJDK);
			System.out.printf("------ Wrote %d callgraphs ------\n", processed);

			Path cgFile = Files.list(cgDir)
					.filter(p -> p.toString().endsWith(".json") || p.toString().endsWith(".json.gz")).findFirst()
					.orElseThrow(() -> new IOException("CG file not found for " + testCaseName));

			SerializedCallgraph scg = cgFile.toString().endsWith(".gz")
					? SerializedCallgraph.readFromFileCompressed(cgFile.toFile())
					: SerializedCallgraph.readFromFile(cgFile.toFile());

			// Process edges into call site mappings
			Map<Method, Map<CallSiteKey, Set<MethodTarget>>> methodToCallSites = new HashMap<>();
			Set<Method> allMethods = new HashSet<>();

			System.out.println("\n=== PARSING VALIDATION ===");
			for (Edge edge : scg.edges) {
				try {
					Method sourceMethod = parseMethodSignature(edge.sourceMethod);
					MethodTarget targetMethod = MethodTarget.from(edge, parseMethodSignature(edge.targetMethod));

					System.out.printf("Parsed: %s -> %s%n", methodToString(sourceMethod), methodToString(targetMethod));

					allMethods.add(sourceMethod);
					allMethods.add(targetMethod);

					// Extract declared target from source statement
					Method declaredTarget = null;
					try {
						String declaredSig = extractDeclaredSignature(edge.sourceStatement);
						if (declaredSig != null) {
							declaredTarget = parseMethodSignature(declaredSig);
							System.out.println("  Declared target: " + methodToString(declaredTarget));
						} else {
							System.out.println("  No declared target found in: " + edge.sourceStatement);
							// declaredTarget can't be null because JCG's soundness eval will fail silently
							System.out.println("  Creating a dummy declaredTarget.");
							declaredTarget = createDummyDeclaredTarget(edge);
						}
					} catch (Exception e) {
						System.out.println("  Error extracting declared target from statement: " + edge.sourceStatement);
						// declaredTarget can't be null because JCG's soundness eval will fail silently
						System.out.println("  Creating a dummy declaredTarget.");
						declaredTarget = createDummyDeclaredTarget(edge);
					}

					// Add to mapping: sourceMethod -> (key -> actualTargets)
					// where key is combined: lineNumber + declaredTarget
					int lineNumber = edge.lineNumber != null ? edge.lineNumber : -1; // Handle null
					CallSiteKey key = new CallSiteKey(lineNumber, declaredTarget, edge.sourceStatement);

					methodToCallSites.computeIfAbsent(sourceMethod, k -> new HashMap<>())
							.computeIfAbsent(key, k -> new HashSet<>()).add(targetMethod);

				} catch (Exception e) {
					System.err.println("Parsing error for edge: " + edge);
					e.printStackTrace();
				}
			}

			// Convert to ReachableMethods format
			System.out.println("\n=== CONVERTING TO REACHABLE METHODS ===");
			Set<ReachableMethod> reachableMethods = new HashSet<>();

			for (Method method : allMethods) {

				Set<CallSite> callSites = new HashSet<>();

				if (methodToCallSites.containsKey(method)) {
					System.out.println("  Has " + methodToCallSites.get(method).size() + " call sites");

					for (Map.Entry<CallSiteKey, Set<MethodTarget>> entry : methodToCallSites.get(method).entrySet()) {
						CallSiteKey key = entry.getKey();
						Method declaredTarget = key.declaredTarget;
						int lineNum = key.line;
						Set<MethodTarget> targets = entry.getValue();

						if (declaredTarget == null) {
							System.out.println("  Call site to: no declared target");
						} else {
							System.out.println("  Call site to: " + methodToString(declaredTarget));
						}
						System.out.println("    Resolves to " + targets.size() + " targets:");
						for (Method target : targets) {
							System.out.println("      -> " + methodToString(target));
						}

						callSites.add(new CallSite(key.sourceStatement, declaredTarget, lineNum, // line number (not available)
								null, // no statement info
								targets));
					}
				} else {
					System.out.println("  Has no call sites (sink method)");
				}

				ReachableMethod rm = new ReachableMethod(method, callSites);
				reachableMethods.add(rm);
				System.out.println("  Created ReachableMethod with " + callSites.size() + " call sites");
			}

			// Print final reachable methods structure
			System.out.println("\n=== FINAL REACHABLE METHODS ===");
			for (ReachableMethod rm : reachableMethods) {
				System.out.println("Method: " + methodToString(rm.method));
				System.out.println("  Call sites: " + rm.callSites.size());
				for (CallSite cs : rm.callSites) {
					if (cs.declaredTarget == null) {
						System.out.println("    Declared: no declared target");
					} else {
						System.out.println("    Declared: " + methodToString(cs.declaredTarget));
					}
					System.out.println("      Resolves to " + cs.targets.size() + " targets:");
					for (Method tgt : cs.targets) {
						System.out.println("        -> " + methodToString(tgt));
					}
				}
			}

			// Serialize the ReachableMethods object to JSON
			Gson gson = new GsonBuilder().setPrettyPrinting().create();
			output.write(gson.toJson(new ReachableMethods(reachableMethods)));
			output.flush();

		} catch (Exception e) {
			throw new RuntimeException("Failed to process " + inputFile, e);
		} finally {
			FileUtils.deleteDirectory(outDir.toFile());
		}

		return System.nanoTime() - start;
	}

	private String readTestCaseName(File inputFile) {
		String name = inputFile.getName();
		if (name.endsWith(".jar") || name.endsWith(".apk")) {
			name = name.substring(0, name.length() - 4);
		}
		return name;
	}

	private long generateCGforFile(File inputFile, String algorithm, String runnerDir, String configDir, Path outDir,
			String mainClass, String[] classPath, String jdkPath, boolean analyzeJdk) throws Exception {

		// Create output directory for this file
		String testCaseName = readTestCaseName(inputFile);
		outDir = outDir.toAbsolutePath();
		Files.createDirectories(outDir);

		// Generate configuration file from template
		String templateFile = null;
		switch (algorithm) {
		case ALGO_FAST:
			templateFile = ALGO_FAST_SERVER_CONF;
			break;
		case ALGO_PRECISE:
			templateFile = ALGO_PRECISE_SERVER_CONF;
			break;
		default:
			throw new RuntimeException("Invalid algorithm: " + algorithm);
		}
		Path templatePath = Paths.get(configDir, templateFile);
		String configContent = new String(Files.readAllBytes(templatePath)).replace("OUTPUT", outDir.toString());
		if (inputFile.getName().toUpperCase().startsWith("LIB")) {
			configContent += "\n\nJavaAnalyzer.ValueFinder.Static.CG.LibraryMode=true";
		}
		/*        if (mainClass != null && mainClass != "") {
		    configContent += "\n\nJavaAnalyzer.EntryPoint=" + "<Entrypoint: void main(java.lang.String[])>";
		}*/

		Path serverConf = outDir.resolve("server.conf");

		Files.write(serverConf, configContent.getBytes());

		// Execute analysis process
		ProcessBuilder pb = new ProcessBuilder("./AnalysisStandaloneRunner", "--configfile", serverConf.toString(),	inputFile.getAbsolutePath());
		pb.directory(new File(runnerDir));
		pb.redirectErrorStream(true);

		Process process = pb.start();
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
			reader.lines().forEach(System.out::println);
		}

		int exitCode = process.waitFor();
		if (exitCode != 0) {
			throw new RuntimeException("Analysis failed with exit code: " + exitCode);
		}

		System.out.printf("------ Finished generating CG for input file: %s ------\n", testCaseName);
		System.out.printf("------ Files written: ------\n");
		Files.list(outDir).filter(path -> (path.toString().endsWith(".json") || path.toString().endsWith(".json.gz")))
				.forEach(e -> System.out.println(e.toString()));

		// Count generated callgraph files
		return Files.list(outDir)
				.filter(path -> (path.toString().endsWith(".json") || path.toString().endsWith(".json.gz"))).count();
	}

	// ---- Helper methods for converting ValueCG output format to JCG format ----

	private static class CallSiteKey {
		final int line;
		final Method declaredTarget;
		final String sourceStatement;

		CallSiteKey(int line, Method declaredTarget, String sourceStatement) {
			this.line = line;
			this.declaredTarget = declaredTarget;
			this.sourceStatement = sourceStatement;
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			if (o == null || getClass() != o.getClass())
				return false;
			CallSiteKey that = (CallSiteKey) o;
			return line == that.line && Objects.equals(declaredTarget, that.declaredTarget)
					&& sourceStatement == that.sourceStatement;
		}

		@Override
		public int hashCode() {
			return Objects.hash(line, declaredTarget, sourceStatement);
		}
	}

	/**
	 * Convert method signature from ValueCG format to the JVM format used by JCG
	 * 
	 * @param sig ValueCG method signature as string (e.g. `<cfne.Demo: void
	 *            main(java.lang.String[])>`)
	 * @return Method object similar to other JCG adapters
	 */
	private Method parseMethodSignature(String sig) {
		// Remove angle brackets
		sig = sig.substring(1, sig.length() - 1);

		// Split into class and method parts
		int colonIdx = sig.indexOf(':');
		String className = sig.substring(0, colonIdx).trim();
		String methodPart = sig.substring(colonIdx + 1).trim();

		// Extract return type
		int lastSpace = methodPart.lastIndexOf(' ');
		String returnType = methodPart.substring(0, lastSpace).trim();
		String rest = methodPart.substring(lastSpace + 1).trim();

		// Extract method name and parameters
		int parenIdx = rest.indexOf('(');
		String methodName = rest.substring(0, parenIdx).trim();
		String paramsStr = rest.substring(parenIdx + 1, rest.length() - 1).trim();

		// Parse parameter types
		List<String> paramTypes = new ArrayList<>();
		if (!paramsStr.isEmpty()) {
			for (String param : paramsStr.split(",")) {
				paramTypes.add(toJVMType(param.trim()));
			}
		}

		return new Method(methodName, toJVMType(className), toJVMType(returnType), paramTypes);
	}

	/**
	 * Extract the method signature (in ValueCG format) of the declared target from
	 * the edge's sourceStatement
	 * 
	 * @param statement e.g. `r6 = staticinvoke <java.lang.Class: java.lang.Class
	 *                  forName(java.lang.String)>("cfne.CatchMeIfYouCan")`
	 * @return Substring of the statement that contains just the method signature of
	 *         the declared target (e.g. `java.lang.Class: java.lang.Class
	 *         forName(java.lang.String)`)
	 */
	private String extractDeclaredSignature(String statement) {
		// ignore params in statements, e.g. to handle `forName(java.lang.String)>(">")`
		int paramStart = statement.indexOf('"');
		if (paramStart != -1) {
			statement = statement.substring(0, paramStart + 1);
		}

		int start = statement.indexOf('<');
		int end = statement.lastIndexOf('>');
		return (start != -1 && end != -1) ? statement.substring(start, end + 1) : null;
	}

	private Method createDummyDeclaredTarget(Edge edge) {
		String dummySig = String.format("<unknown>@L%d", edge.lineNumber != null ? edge.lineNumber : -1);
		return new Method(dummySig, edge.sourceStatement, "UNKNOWN", Collections.emptyList());
	}

	/**
	 * Convert Type string (e.g. `java.lang.String[]`) to NVM internal format used
	 * by JCG (e.g. `[Ljava.lang.String;`)
	 * Also used for class names (e.g. `cfne.Demo` becomes `Lcfne/Demo;`)
	 * 
	 * @param javaType
	 * @return
	 */
	private String toJVMType(String javaType) {
		int dims = 0;
		while (javaType.endsWith("[]")) {
			dims++;
			javaType = javaType.substring(0, javaType.length() - 2);
		}

		String base;
		switch (javaType) {
		case "byte":
			base = "B";
			break;
		case "char":
			base = "C";
			break;
		case "double":
			base = "D";
			break;
		case "float":
			base = "F";
			break;
		case "int":
			base = "I";
			break;
		case "long":
			base = "J";
			break;
		case "short":
			base = "S";
			break;
		case "boolean":
			base = "Z";
			break;
		case "void":
			base = "V";
			break;
		default:
			base = "L" + javaType.replace('.', '/') + ";";
		}

		return "[".repeat(dims) + base;
	}

	// Helper method for readable method representation, for debugging
	private String methodToString(Method m) {
		if (m == null) {
			throw new RuntimeException("Could not convert method 'null' to string");
		}
		return String.format("%s.%s(%s):%s", m.declaringClass, m.name, String.join(",", m.parameterTypes),
				m.returnType);
	}
}
