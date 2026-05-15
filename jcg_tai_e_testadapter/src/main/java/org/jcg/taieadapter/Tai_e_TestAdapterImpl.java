package org.jcg.taieadapter;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.apache.commons.io.FileUtils;

public class Tai_e_TestAdapterImpl {
    // ---- JCG Adapter: TOOL-INDEPENDENT FORMAT ----

    /**
     * Representation of a method in the JCG format.
     * Contains method name, declaring class (in JVM format), return type and
     * parameter types (all JVM-formatted).
     */
    public static class Method {
        public String name, declaringClass, returnType;
        public List<String> parameterTypes;

        public Method(String name, String declaringClass, String returnType,
                List<String> parameterTypes) {
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
            return Objects.equals(name, m.name) &&
                    Objects.equals(declaringClass, m.declaringClass) &&
                    Objects.equals(returnType, m.returnType) &&
                    Objects.equals(parameterTypes, m.parameterTypes);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name, declaringClass, returnType, parameterTypes);
        }
    }

    /**
     * Representation of a call site in the JCG format.
     * Contains the declared target, source line, bytecode offset (if available),
     * and the set of possible targets.
     */
    public static class CallSite {
        public Method declaredTarget;
        public int line;
        public Integer pc;
        public Set<Method> targets;

        public CallSite(Method declaredTarget, int line, Integer pc, Set<Method> targets) {
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
     * Entry point: Converts a Tai-e call graph into the JCG ReachableMethods JSON
     * format and writes it to the given output Writer.
     * 
     * @return The runtime in nanoseconds.
     */
    public long serializeCG(
            String algorithm,
            String inputDirPath,
            Writer output,
            String mainClass,
            String[] classPath,
            String JDKPath,
            boolean analyzeJDK) throws Exception {

        System.out.println("algorithm=" + algorithm + ", inputDirPath=" + inputDirPath + ", output=" + output
                + ", mainClass=" + mainClass + ", classPath=" + java.util.Arrays.toString(classPath) + ", JDKPath="
                + JDKPath + ", analyzeJDK=" + analyzeJDK);

        long start = System.nanoTime();

        Path runnerDir = Files.createTempDirectory("tai-e");

        try {

            // Generate callgraph
            File inputFile = new File(inputDirPath); // inputDirPath is the single .apk or .jar file that we want to
                                                     // generate the CG for
            String testCaseName = readTestCaseName(inputFile);
            Path cgDir = runnerDir.resolve("output-cgs", algorithm, testCaseName); // where to write the intermediate
                                                                                      // results from Taie before reading
                                                                                      // and parsing them
            generateCGforFile(
                    inputFile,
                    algorithm,
                    cgDir,
                    mainClass,
                    classPath,
                    JDKPath,
                    analyzeJDK);

            // Read reachable methods
            Path reachableMethodsPath = cgDir.resolve("reachable-methods.txt");
            Set<Method> allMethods = Files.readAllLines(reachableMethodsPath).stream()
                    .map(this::parseMethodSignature)
                    .collect(Collectors.toSet());

            // Parse call-graph.dot
            Map<String, String> nodeMap = parseDotNodes(cgDir.resolve("call-graph.dot"));
            Map<Method, Map<CallSiteKey, Set<Method>>> callSitesMap = parseDotEdges(cgDir.resolve("call-graph.dot"),
                    nodeMap);

            // Build ReachableMethods structure
            Set<ReachableMethod> reachableMethods = new HashSet<>();
            for (Method method : allMethods) {
                Set<CallSite> sites = new HashSet<>();
                if (callSitesMap.containsKey(method)) {
                    for (var entry : callSitesMap.get(method).entrySet()) {
                        sites.add(new CallSite(
                                entry.getKey().declaredTarget,
                                entry.getKey().line,
                                null, // pc not required in final format
                                entry.getValue()));
                    }
                }
                reachableMethods.add(new ReachableMethod(method, sites));
            }

            // Serialize the ReachableMethods object to JSON
            Gson gson = new GsonBuilder().setPrettyPrinting().create();
            output.write(gson.toJson(new ReachableMethods(reachableMethods)));
            output.flush();

            return System.nanoTime() - start;

        } finally {
            FileUtils.deleteDirectory(runnerDir.toFile());
        }
    }

    private String readTestCaseName(File inputFile) {
        String name = inputFile.getName();
        if (name.endsWith(".jar") || name.endsWith(".apk")) {
            name = name.substring(0, name.length() - 4);
        }
        return name;
    }

    private long generateCGforFile(
            File inputFile,
            String algorithm,
            Path outDir,
            String mainClass,
            String[] classPath,
            String jdkPath,
            boolean analyzeJdk) throws Exception {

        // Create output directory for this file
        String testCaseName = readTestCaseName(inputFile);
        outDir = outDir.toAbsolutePath();
        Files.createDirectories(outDir);


        // Generate configuration file from template
        String algoTaieName = switch (algorithm.toUpperCase()) {
            case "CHA" -> "cha";
            case "PTA" -> "pta";
            default -> throw new RuntimeException("Invalid algorithm: " + algorithm);
        };

        // Execute analysis process
        ArrayList<String> command = new ArrayList<>(Arrays.asList(
                "--class-path", inputFile.getAbsolutePath(),
                "--main-class", mainClass,
                "-java", "8",
                "-scope", "ALL",
                "-a", "cg=algorithm:" + algoTaieName + ";dump:true;dump-methods:true",
                "--output-dir", outDir.toString()));

        String annotations = lib.annotations.callgraph.IndirectCalls.class.getProtectionDomain().getCodeSource().getLocation().getPath().toString();
        ArrayList<String> classPathArray = new ArrayList<>(Arrays.asList(classPath));
        classPathArray.add(annotations);

        command.add("--class-path");
        command.add(String.join(":", classPathArray));

        System.out.println(command);
        pascal.taie.Main.main(command.toArray(new String[0]));

        System.out.printf("------ Finished generating CG for input file: %s ------\n", testCaseName);
        System.out.printf("------ Files written: ------\n");
        Files.list(outDir)
                .filter(path -> (path.toString().endsWith(".dot")))
                .forEach(e -> System.out.println(e.toString()));

        // Count generated callgraph files
        return Files.list(outDir)
                .filter(path -> (path.toString().endsWith(".dot")))
                .count();
    }

    // ---- Helper methods for converting Tai-e output format to JCG format ----

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
            return line == that.line
                    && Objects.equals(declaredTarget, that.declaredTarget)
                    && Objects.equals(sourceStatement, that.sourceStatement);
        }

        @Override
        public int hashCode() {
            return Objects.hash(line, declaredTarget, sourceStatement);
        }
    }

    /**
     * Convert method signature from Tai-e format to the JVM format used by JCG
     * 
     * @param sig Tai-e method signature as string (e.g. `<cfne.Demo: void
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

        return new Method(
                methodName,
                toJVMType(className),
                toJVMType(returnType),
                paramTypes);
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

        String base = switch (javaType) {
            case "byte" -> "B";
            case "char" -> "C";
            case "double" -> "D";
            case "float" -> "F";
            case "int" -> "I";
            case "long" -> "J";
            case "short" -> "S";
            case "boolean" -> "Z";
            case "void" -> "V";
            default -> "L" + javaType.replace('.', '/') + ";";
        };

        return "[".repeat(dims) + base;
    }

    private Map<String, String> parseDotNodes(Path dotPath) throws IOException {
        Map<String, String> nodeMap = new HashMap<>();
        Pattern nodePattern = Pattern.compile("\"(\\d+)\" \\[label=\"(<[^>]+>)\"");

        for (String line : Files.readAllLines(dotPath)) {
            Matcher m = nodePattern.matcher(line);
            if (m.find())
                nodeMap.put(m.group(1), m.group(2));
        }
        return nodeMap;
    }

    private Map<Method, Map<CallSiteKey, Set<Method>>> parseDotEdges(
            Path dotPath, Map<String, String> nodeMap) throws IOException {

        // example edge from DOT file:
        // "3" -> "20786" [label="[0@L228] $r1 = invokevirtual
        // %this.<java.util.stream.FindOps$FindSink$OfDouble: java.util.OptionalDouble
        // get()>();",];

        Map<Method, Map<CallSiteKey, Set<Method>>> result = new HashMap<>();
        Pattern edgePattern = Pattern.compile(
                "\\s*\"(\\d+)\"\\s*->\\s*\"(\\d+)\"\\s*\\[label=\\\"\\[(\\d+)@L(-?\\d+)\\].*?<([^>]*)>.*?\\\"");
        System.out.println("=== START PARSING DOT EDGES ===");
        System.out.println("Node map size: " + nodeMap.size());
        int total = 0, matched = 0, skipped = 0;

        for (String line : Files.readAllLines(dotPath)) {
            if (!line.contains("->"))
                continue;

            total++;

            Matcher m = edgePattern.matcher(line);
            if (!m.find()) {
                skipped++;
                System.out.println("SKIPPED: " + line);
                continue;
            }

            matched++;
            String srcId = m.group(1);
            String tgtId = m.group(2);
            int pc = Integer.parseInt(m.group(3));
            int lineNum = Integer.parseInt(m.group(4));
            String declaredSig = "<" + m.group(5) + ">";

            // System.out.printf("MATCHED: src=%s, tgt=%s, pc=%d, line=%d, sig=%s%n",
            // srcId, tgtId, pc, lineNum, declaredSig);

            if (!nodeMap.containsKey(srcId) || !nodeMap.containsKey(tgtId))
                continue;

            Method caller = parseMethodSignature(nodeMap.get(srcId));
            Method declared = parseMethodSignature(declaredSig);
            Method target = parseMethodSignature(nodeMap.get(tgtId));

            CallSiteKey key = new CallSiteKey(lineNum, declared, line);
            result
                    .computeIfAbsent(caller, k -> new HashMap<>())
                    .computeIfAbsent(key, k -> new HashSet<>())
                    .add(target);
        }
        System.out.printf("DOT STATS: total=%d, matched=%d, skipped=%d%n", total, matched, skipped);
        return result;
    }
}
