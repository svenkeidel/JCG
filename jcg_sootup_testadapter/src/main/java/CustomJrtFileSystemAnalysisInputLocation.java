
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;
import org.apache.commons.io.FilenameUtils;
import org.jspecify.annotations.NonNull;
import sootup.core.IdentifierFactory;
import sootup.core.frontend.PathbasedClassProvider;
import sootup.core.frontend.ResolveException;
import sootup.core.frontend.SootClassSource;
import sootup.core.inputlocation.AnalysisInputLocation;
import sootup.core.interceptor.BodyInterceptor;
import sootup.core.model.SourceType;
import sootup.core.types.ClassType;
import sootup.core.util.StreamUtils;
import sootup.core.views.View;
import sootup.interceptors.BytecodeBodyInterceptors;
import sootup.java.bytecode.frontend.conversion.AsmJavaClassProvider;
import sootup.java.bytecode.frontend.conversion.AsmModuleSource;
import sootup.java.core.*;
import sootup.java.core.signatures.ModulePackageName;
import sootup.java.core.signatures.ModuleSignature;
import sootup.java.core.types.JavaClassType;

public class CustomJrtFileSystemAnalysisInputLocation implements ModuleInfoAnalysisInputLocation {
    private FileSystem theFileSystem;
    private final Map<ModuleSignature, JavaModuleInfo> moduleInfoMap = new HashMap<>();
    boolean isResolved = false;

    @NonNull private final SourceType sourceType;

    @NonNull private final List<BodyInterceptor> bodyInterceptors;

    public CustomJrtFileSystemAnalysisInputLocation(Path modulePath) throws IOException {
        this(modulePath, SourceType.Library);
    }

    public CustomJrtFileSystemAnalysisInputLocation(Path modulePath, @NonNull SourceType sourceType) throws IOException {
        this(modulePath, sourceType, BytecodeBodyInterceptors.Default.getBodyInterceptors());
    }

    public CustomJrtFileSystemAnalysisInputLocation(
            Path modulePath, @NonNull SourceType sourceType, @NonNull List<BodyInterceptor> bodyInterceptors) throws IOException {
        Path jrtfs = modulePath.getParent().resolve("jrt-fs.jar");
        URLClassLoader loader = new URLClassLoader(new URL[] { jrtfs.toUri().toURL() });
        Path javaHome = modulePath.getParent().getParent();
        theFileSystem = FileSystems.newFileSystem(URI.create("jrt:/"), Map.of("java.home", javaHome.toString()), loader);
        this.sourceType = sourceType;
        this.bodyInterceptors = bodyInterceptors;
    }

    @Override
    @NonNull
    public Optional<JavaSootClassSource> getClassSource(
            @NonNull ClassType classType, @NonNull View view) {
        JavaClassType klassType = (JavaClassType) classType;

        PathbasedClassProvider classProvider = getClassProvider(view);
        Path filepath =
                theFileSystem.getPath(
                        klassType.getFullyQualifiedName().replace('.', '/')
                                + classProvider.getHandledFileType().getExtensionWithDot());

        // parse as module
        if (klassType.getPackageName() instanceof ModulePackageName) {

            ModulePackageName modulePackageSignature = (ModulePackageName) klassType.getPackageName();

            final Path module =
                    theFileSystem.getPath(
                            "modules", modulePackageSignature.getModuleSignature().getModuleName());
            Path foundClass = module.resolve(filepath);
            if (Files.isRegularFile(foundClass)) {
                return classProvider
                        .createClassSource(this, foundClass, klassType)
                        .map(src -> (JavaSootClassSource) src);
            } else {
                return Optional.empty();
            }
        }

        // module information does not exist in Signature -> search for class
        final Path moduleRoot = theFileSystem.getPath("modules");
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(moduleRoot)) {
            {
                for (Path entry : stream) {
                    // check each module folder for the class
                    Path foundfile = entry.resolve(filepath);
                    if (Files.isRegularFile(foundfile)) {
                        return classProvider
                                .createClassSource(this, foundfile, klassType)
                                .map(src -> (JavaSootClassSource) src);
                    }
                }
            }
        } catch (IOException e) {
            throw new ResolveException("Error loading a module", moduleRoot, e);
        }

        return Optional.empty();
    }

    /** Retreive CLassSources of a module specified by methodSignature */
    @Override
    @NonNull
    public Stream<JavaSootClassSource> getModulesClassSources(
            @NonNull ModuleSignature moduleSignature, @NonNull View view) {
        return getClassSourcesInternal(moduleSignature, view.getIdentifierFactory(), view);
    }

    @NonNull
    protected Stream<JavaSootClassSource> getClassSourcesInternal(
            @NonNull ModuleSignature moduleSignature,
            @NonNull IdentifierFactory identifierFactory,
            @NonNull View view) {

        PathbasedClassProvider classProvider = getClassProvider(view);

        String moduleInfoFilename =
                JavaModuleIdentifierFactory.MODULE_INFO_FILE
                        + classProvider.getHandledFileType().getExtensionWithDot();

        final Path archiveRoot = theFileSystem.getPath("modules", moduleSignature.getModuleName());
        try (Stream<Path> paths = Files.walk(archiveRoot)) {
            // collect into a list and then return a stream, so we do not leak the Stream returned by
            // Files.walk
            List<JavaSootClassSource> javaSootClassSources =
                    paths
                            .filter(
                                    filePath -> {
                                        if (!Files.isDirectory(filePath)) {
                                            String pathStr = filePath.toString();
                                            return pathStr.endsWith(
                                                    classProvider.getHandledFileType().getExtensionWithDot())
                                                    && !pathStr.endsWith(moduleInfoFilename);
                                        }
                                        return false;
                                    })
                            .<SootClassSource>flatMap(
                                    p ->
                                            StreamUtils.optionalToStream(
                                                    classProvider.createClassSource(this, p, fromPath(p, identifierFactory))))
                            .map(src -> (JavaSootClassSource) src)
                            .toList();
            return javaSootClassSources.stream();
        } catch (IOException e) {
            throw new ResolveException("Error loading module " + moduleSignature, archiveRoot, e);
        }
    }

    protected PathbasedClassProvider getClassProvider(@NonNull View view) {
        return new AsmJavaClassProvider(view);
    }

    @Override
    public @NonNull Stream<JavaSootClassSource> getClassSources(@NonNull View view) {

        Collection<ModuleSignature> moduleSignatures = discoverModules();
        return moduleSignatures.stream()
                .flatMap(sig -> getClassSourcesInternal(sig, view.getIdentifierFactory(), view));
    }

    /**
     * Discover and return all modules contained in the jrt filesystem.
     *
     * @return Collection of found module names.
     */
    @NonNull
    public Collection<ModuleSignature> discoverModules() {
        if (!isResolved) {
            final Path moduleRoot = theFileSystem.getPath("modules");
            final String moduleInfoFilename = JavaModuleIdentifierFactory.MODULE_INFO_FILE + ".class";
            try (DirectoryStream<Path> stream = Files.newDirectoryStream(moduleRoot)) {
                {
                    for (Path entry : stream) {
                        if (Files.isDirectory(entry)) {
                            ModuleSignature moduleSignature =
                                    JavaModuleIdentifierFactory.getModuleSignature(entry.subpath(1, 2).toString());
                            Path moduleInfo = entry.resolve(moduleInfoFilename);
                            if (Files.exists(moduleInfo)) {
                                moduleInfoMap.put(moduleSignature, new AsmModuleSource(moduleInfo));
                            } else {
                                moduleInfoMap.put(
                                        moduleSignature, JavaModuleInfo.createAutomaticModuleInfo(moduleSignature));
                            }
                        }
                    }
                }
            } catch (IOException e) {
                throw new ResolveException("Error while discovering modules", moduleRoot, e);
            }
            isResolved = true;
        }
        return moduleInfoMap.keySet();
    }

    @NonNull
    private JavaClassType fromPath(
            @NonNull Path p, @NonNull final IdentifierFactory identifierFactory) {

        final Path moduleDir = p.subpath(1, 2);
        final Path filename = p.subpath(2, p.getNameCount());

        final String fullyQualifiedName =
                FilenameUtils.removeExtension(
                        filename.toString().replace(filename.getFileSystem().getSeparator(), "."));

        JavaClassType sig = (JavaClassType) identifierFactory.getClassType(fullyQualifiedName);

        // TODO: move to Module version
        if (identifierFactory instanceof JavaModuleIdentifierFactory) {
            return ((JavaModuleIdentifierFactory) identifierFactory)
                    .getClassType(sig.getClassName(), sig.getPackageName().getName(), moduleDir.toString());
        }

        // if we are using the normal signature factory, then trim the module from the path
        return sig;
    }

    @NonNull
    @Override
    public Optional<JavaModuleInfo> getModuleInfo(ModuleSignature sig, View view) {
        if (!isResolved) {
            discoverModules();
        }
        return Optional.ofNullable(moduleInfoMap.get(sig));
    }

    @NonNull
    @Override
    public Set<ModuleSignature> getModules(View view) {
        if (!isResolved) {
            discoverModules();
        }
        return Collections.unmodifiableSet(moduleInfoMap.keySet());
    }

    @NonNull
    @Override
    public SourceType getSourceType() {
        return sourceType;
    }

    @Override
    @NonNull
    public List<BodyInterceptor> getBodyInterceptors() {
        return bodyInterceptors;
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof CustomJrtFileSystemAnalysisInputLocation;
    }

    @Override
    public int hashCode() {
        return 31;
    }
}