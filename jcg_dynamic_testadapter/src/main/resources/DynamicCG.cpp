
#include <jvmti.h>

#include <map>
#include <unordered_set>

#include <iostream>
#include <fstream>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <filesystem>

#include <string_view>

using namespace std::string_view_literals;

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
        out << "{"sv;
        out << "\"declaringClass\": \""sv << declaringClass << "\","sv;
        out << "\"name\": \""sv << name << "\","sv;
        out << "\"returnType\": \""sv << returnType << "\","sv;
        out << "\"parameterTypes\": ["sv;
        bool first = true;
        for (const auto& parameter : parameterTypes) {
            if (!first) out << ", "sv;
            out << "\""sv << parameter << "\""sv;
            first = false;
        }
        out << "]"sv;
        out << "}"sv;
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
        if (lineNumbers != NULL) jvmti->Deallocate((unsigned char*)lineNumbers);

        pc = loc;
    }

    bool operator==(const CallSite &other) const noexcept {
        return method == other.method && lineNumber == other.lineNumber && pc == other.pc;
    }

    void toJson(boost::iostreams::filtering_ostream& out) const {
        out << "{"sv;
        out << "\"method\": \""sv << method << "\","sv;
        out << "\"line\": "sv << lineNumber << ","sv;
        out << "\"pc\": "sv << pc;
        out << "}"sv;
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

struct CallSitePointerHash {
    size_t operator()(const CallSite* callSite) const noexcept {
        return reinterpret_cast<size_t>(callSite);
    }
};

struct CallSitePointerEquals {
    bool operator()(const CallSite* lhs, const CallSite* rhs) const {
        return lhs == rhs;
    }
};

std::unordered_set<CallSite> callSitePool;

struct CallTree {
    std::unordered_map<const CallSite*, std::unique_ptr<CallTree>, CallSitePointerHash, CallSitePointerEquals> children;

    CallTree() {
        children.reserve(1);
    }

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
        out << "{"sv;

        bool first = true;
        for (const auto& [callSite, subTree] : children) {

            if (! first) {
                out << ","sv;
            }

            out << "\""sv << callSite << "\": "sv;
            subTree->toJson(out);

            first = false;
        }

        out << "}"sv;
    }

    unsigned int size() const {
        unsigned int s = 1;
        for (const auto& [callSite, subTree] : children) {
            s += subTree->size();
        }
        return s;
    }

    unsigned int bucketSum() const {
        unsigned int sum = children.bucket_count();
        for (const auto& [callSite, subTree] : children) {
            sum += subTree->bucketSum();
        }
        return sum;
    }
};
static CallTree callTree;


static unsigned long methodCalls = 0;

static std::mutex methodEntryMutex;

void JNICALL MethodEntry(jvmtiEnv *jvmti, JNIEnv* jni, jthread thread, jmethodID method) {

    std::lock_guard<std::mutex> lock(methodEntryMutex);

    if (methodCalls % 100000 == 0) {
        std::cout << "callTree.size = "sv << callTree.size() << ", callTree.bucketSum = " << callTree.bucketSum() << "\n"sv;
        std::cout << "callSitePool.size = "sv << callSitePool.size() << "\n"sv;
        std::cout << "methodPool.size = "sv << methodPool.size() << "\n"sv;
        std::cout.flush();
    }
    methodCalls += 1;

    static const jint start_depth = 0;
    static const jint max_stack_depth = 10000;
    static jvmtiFrameInfo stack_frames[max_stack_depth];
    static jint stack_size;

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

    std::streamsize buffer_size = 64 * 1024;
    boost::iostreams::gzip_params params;
    params.level = boost::iostreams::gzip::default_compression;
    out.push(boost::iostreams::gzip_compressor(params, buffer_size));
    out.push(call_graph_file); // Pipe the compressed data directly to your file

    out << "{"sv;

    {
        std::cout << "Write call tree ...\n" << std::flush;

        out << "\"callTree\": "sv;
        callTree.toJson(out);

        boost::iostreams::flush(out);

        std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;

        std::cout << "Write call-sites ...\n" << std::flush;

        out << ", \"callSites\": {"sv;
        bool first = true;
        for (const auto& callSite : callSitePool) {
            if (!first) out << ",\n"sv;

            out << "\""sv << std::addressof(callSite) << "\": "sv;
            callSite.toJson(out);

            first = false;
        }
        out << "}"sv;

        boost::iostreams::flush(out);

        std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;

        std::cout << "Write methods ...\n" << std::flush;

        out << ", \"methods\": {"sv;
        first = true;
        for (const auto& method : methodPool) {
            if (!first) out << ",\n"sv;

            out << "\""sv << std::addressof(method) << "\": "sv;
            method.toJson(out);

            first = false;
        }
        out << "}"sv;

        boost::iostreams::flush(out);
        std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;
    }

    out << "}"sv;

    boost::iostreams::close(out);

    std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;
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
