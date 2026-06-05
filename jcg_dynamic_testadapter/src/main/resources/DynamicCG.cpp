
#include <jvmti.h>
#include <map>
#include <unordered_set>
#include <iostream>
#include <fstream>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>

static jvmtiEnv *jvmti = NULL;
static char *call_graph_file_name = NULL;
static const std::hash<std::string> stringHash = std::hash<std::string>{};
static const std::hash<int> intHash = std::hash<int>{};
static const std::hash<long> longHash = std::hash<long>{};

struct Method {
    jvmtiEnv *jvmti;
    std::string name;
    std::string declaringClass;
    std::string returnType;
    std::vector<std::string> parameterTypes;

    Method(jvmtiEnv *jvmti, jmethodID mid) {
        this->jvmti = jvmti;

        jclass cls;
        int err;
        if ((err = jvmti->GetMethodDeclaringClass(mid, &cls)) != JVMTI_ERROR_NONE)
            throw std::runtime_error("cannot get declaring class: error "+std::to_string(err));

        char* className;
        char* generic;
        if ((err = jvmti->GetClassSignature(cls, &className, NULL)) != JVMTI_ERROR_NONE)
            throw std::runtime_error("cannot get class: error "+std::to_string(err));
        declaringClass = std::string(className);
        jvmti->Deallocate((unsigned char*) className);

        char *methodName;
        char *sig;
        jvmti->GetMethodName(mid, &methodName, &sig, NULL);
        name = std::string(methodName);
        std::string signature = std::string(sig);
        jvmti->Deallocate((unsigned char*) methodName);
        jvmti->Deallocate((unsigned char*) sig);

        returnType = signature.substr(signature.find(")") + 1);
        std::string parameterString = signature.substr(1, signature.find(")") - 1);

        std::string currentParameter = "";
        while (! parameterString.empty()) {
            if (parameterString.starts_with("L")) {
                int pos = parameterString.find(";");
                currentParameter += parameterString.substr(0, pos + 1);
                parameterTypes.push_back(currentParameter);
                currentParameter = "";
                parameterString.erase(0,pos+1);
            } else if (parameterString.starts_with("[")) {
                currentParameter += "[";
                parameterString.erase(0,1);
            } else {
                currentParameter += parameterString.substr(0, 1);
                parameterTypes.push_back(currentParameter);
                currentParameter = "";
                parameterString.erase(0,1);
            }
        }
    }

    bool operator==(const Method &o) const {
        return name == o.name && declaringClass == o.declaringClass && returnType == o.returnType && parameterTypes == o.parameterTypes;
    }

    bool operator<(const Method &o) const {
        return name < o.name || declaringClass == o.declaringClass && returnType < o.returnType && parameterTypes < o.parameterTypes;
    }

    void toJson(boost::iostreams::filtering_ostream& out) const {
        out << "{";
        out << "\"declaringClass\": \"" << declaringClass << "\",";
        out << "\"name\": \"" << name << "\",";
        out << "\"returnType\": \"" << returnType << "\",";
        out << "\"parameterTypes\": [";
        bool first = true;
        for (const auto& parameter : parameterTypes) {
            if (!first) out << ", ";
            out << "\"" << parameter << "\"";
            first = false;
        }
        out << "]";
        out << "}";
    }
};

namespace std {
    template<>
    struct hash<Method> {
        size_t operator()(const Method& method) const noexcept {
            size_t seed = stringHash(method.name);
            seed ^= stringHash(method.declaringClass) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
            seed ^= stringHash(method.returnType) + 0x9e3779b9 + (seed << 6) + (seed >> 2);

            for (const auto parameterType: method.parameterTypes) {
                seed ^= stringHash(parameterType) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
            }

            return seed;
        }
    };
}

std::unordered_set<Method> methodPool;

struct CallSite {
    const Method* method;
    jint lineNumber;
    jlong pc;

    CallSite(jvmtiEnv *jvmti, jmethodID m, jlocation loc) {
        auto [it, _inserted] = methodPool.insert(Method(jvmti, m));
        method = &(*it);

        jint lntEntries;
        jvmtiLineNumberEntry* lineNumbers = NULL;
        if (jvmti->GetLineNumberTable(m, &lntEntries, &lineNumbers) == JVMTI_ERROR_NONE) {
            lineNumber = lineNumbers[0].line_number;
            for (int i = 1; i < lntEntries; i++) {
                if (loc < lineNumbers[i].start_location) {
                    break;
                }
                lineNumber = lineNumbers[i].line_number;
            }
        } else {
            lineNumber = -1;
        }

        pc = loc;
    }

    bool operator==(const CallSite &other) const {
        return method == other.method && lineNumber == other.lineNumber && pc == other.pc;
    }

    void toJson(boost::iostreams::filtering_ostream& out) const {
        out << "{";
        out << "\"method\": \"" << method << "\",";
        out << "\"line\": " << lineNumber << ",";
        out << "\"pc\": " << pc;
        out << "}";
    }
};

namespace std {
    template<>
    struct hash<CallSite> {
        size_t operator()(const CallSite& callSite) const noexcept {
            size_t seed = reinterpret_cast<size_t>(callSite.method);
            seed ^= callSite.lineNumber + 0x9e3779b9 + (seed << 6) + (seed >> 2);
            seed ^= callSite.pc + 0x9e3779b9 + (seed << 6) + (seed >> 2);

            return seed;
        }
    };

}

struct CompareCallSitePointer {
    bool operator()(const CallSite* lhs, const CallSite* rhs) const {
        return lhs < rhs;
    }
};

std::unordered_set<CallSite> callSitePool;

struct CallTree {
    std::map<const CallSite*, std::unique_ptr<CallTree>, CompareCallSitePointer> children;

    void addStackTrace(jvmtiEnv *jvmti, jvmtiFrameInfo* stack_frames, jint stack_size) {
        if(stack_size > 0) {
            auto [it, _inserted] = callSitePool.insert(CallSite (jvmti, stack_frames[stack_size - 1].method, stack_frames[stack_size - 1].location ));

            const CallSite* topmost = &(*it);

            if(! children.contains(topmost)) {
                children.emplace(topmost, std::make_unique<CallTree>());
            }

            children[topmost]->addStackTrace(jvmti, stack_frames, stack_size - 1);
        }
    }

    void toJson(boost::iostreams::filtering_ostream& out) const {
        out << "{";

        bool first = true;
        for (const auto& [callSite, subTree] : children) {

            if (! first) {
                out << ",";
            }

            out << "\"" << callSite << "\": ";
            subTree->toJson(out);

            first = false;
        }

        out << "}";
    }
};
static CallTree callTree;


void JNICALL MethodEntry(jvmtiEnv *jvmti, JNIEnv* jni, jthread thread, jmethodID method) {

    static const jint start_depth = 0;
    static const jint max_stack_depth = 10000;
    static jvmtiFrameInfo stack_frames[max_stack_depth];
    static jint stack_size;

    static size_t lastCallSitePoolSize = 0;
    static size_t lastMethodPoolSize = 0;
    if (callSitePool.size() >= lastCallSitePoolSize + 1000) {
        lastCallSitePoolSize = callSitePool.size();
        std::cout << "callSitePool.size = " << lastCallSitePoolSize << "\n";
    }
    if (methodPool.size() >= lastMethodPoolSize + 1000) {
        lastMethodPoolSize = methodPool.size();
        std::cout << "methodPool.size = " << lastMethodPoolSize << "\n";
    }

    jvmtiError err;
    if ((err = jvmti->GetStackTrace(thread, start_depth, max_stack_depth, stack_frames, &stack_size)) != JVMTI_ERROR_NONE)
        throw std::runtime_error("cannot get stack trace: error "+std::to_string(err));

    callTree.addStackTrace(jvmti, stack_frames, stack_size);
}

void return_cg() {
    // Create an ordinary output file stream in binary mode
    std::ofstream call_graph_file(call_graph_file_name, std::ios::binary);

    // Create a filtering stream and push the Gzip compressor
    boost::iostreams::filtering_ostream out;
    out.push(boost::iostreams::gzip_compressor());
    out.push(call_graph_file); // Pipe the compressed data directly to your file

    out << "{";

        out << "\"callTree\": ";
        callTree.toJson(out);

        out << ", \"callSites\": {";
        bool first = true;
        for (const auto& callSite : callSitePool) {
            if (!first) out << ",\n";

            out << "\"" << std::addressof(callSite) << "\": ";
            callSite.toJson(out);

            first = false;
        }
        out << "}";

        out << ", \"methods\": {";
        first = true;
        for (const auto& method : methodPool) {
            if (!first) out << ",\n";

            out << "\"" << std::addressof(method) << "\": ";
            method.toJson(out);

            first = false;
        }
        out << "}";

    out << "}";
}

JNIEXPORT void JNICALL VMDeath(jvmtiEnv *jvmti_env, JNIEnv* jni_env) {
    return_cg();
}

JNIEXPORT jint JNICALL Agent_OnLoad(JavaVM *vm, char *options, void *reserved) {
    call_graph_file_name = options;

    vm->GetEnv((void**)&jvmti, JVMTI_VERSION_1_0);

    jvmtiCapabilities capabilities = {0};
    capabilities.can_generate_method_entry_events = 1;
    capabilities.can_get_line_numbers = 1;
    jvmti->AddCapabilities(&capabilities);

    jvmtiEventCallbacks callbacks = {0};
    callbacks.MethodEntry = MethodEntry;
    callbacks.VMDeath = VMDeath;
    jvmti->SetEventCallbacks(&callbacks, sizeof(callbacks));

    jvmti->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_METHOD_ENTRY, NULL);
    jvmti->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_VM_DEATH, NULL);

    return 0;
}
