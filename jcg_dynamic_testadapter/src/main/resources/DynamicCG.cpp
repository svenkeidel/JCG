
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
    jmethodID methodId;

    Method(jmethodID methodId) {
        this->methodId = methodId;
    }

    bool operator==(const Method &o) const {
        return methodId == o.methodId;
    }

    bool operator<(const Method &o) const {
        return reinterpret_cast<size_t>(methodId) < reinterpret_cast<size_t>(o.methodId);
    }

    void toJson(jvmtiEnv *jvmti, boost::iostreams::filtering_ostream& out) const {

        std::string name;
        std::string declaringClass;
        std::string returnType;
        std::vector<std::string> parameterTypes;

        jclass cls;
        int err;
        if ((err = jvmti->GetMethodDeclaringClass(methodId, &cls)) != JVMTI_ERROR_NONE)
            throw std::runtime_error("cannot get declaring class: error "+std::to_string(err));

        char* className;
        char* generic;
        if ((err = jvmti->GetClassSignature(cls, &className, NULL)) != JVMTI_ERROR_NONE)
            throw std::runtime_error("cannot get class: error "+std::to_string(err));
        declaringClass = std::string(className);
        jvmti->Deallocate((unsigned char*) className);

        char *methodName;
        char *sig;
        jvmti->GetMethodName(methodId, &methodName, &sig, NULL);
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
            return reinterpret_cast<size_t>(method.methodId);
        }
    };
}

std::unordered_set<Method> methodPool;

struct CallSite {
    const Method* method;
    const jlocation location;

    CallSite(jmethodID m, jlocation location)
        : method(&(*methodPool.insert(Method(m)).first)),
          location(location)
    {
    }

    bool operator==(const CallSite &other) const noexcept {
        return location == other.location && method == other.method;
    }

    void toJson(jvmtiEnv *jvmti, boost::iostreams::filtering_ostream& out) const {
        jint lntEntries;
        jint lineNumber;
        jvmtiLineNumberEntry* lineNumbers = NULL;
        if (jvmti->GetLineNumberTable(method->methodId, &lntEntries, &lineNumbers) == JVMTI_ERROR_NONE) {
            lineNumber = lineNumbers[0].line_number;
            for (int i = 1; i < lntEntries; i++) {
                if (location < lineNumbers[i].start_location) {
                    break;
                }
                lineNumber = lineNumbers[i].line_number;
            }
        } else {
            lineNumber = -1;
        }
        if (lineNumbers != NULL) jvmti->Deallocate((unsigned char*)lineNumbers);

        jlocation pc = location;

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
            seed ^= callSite.location + 0x9e3779b9 + (seed << 6) + (seed >> 2);

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
            auto [it, _inserted] = callSitePool.insert(CallSite (stack_frames[stack_size - 1].method, stack_frames[stack_size - 1].location ));

            const CallSite* topmost = &(*it);

            if(! children.contains(topmost)) {
                children.emplace(topmost, std::make_unique<CallTree>());
            }

            children[topmost]->addStackTrace(jvmti, stack_frames, stack_size - 1);
        }
    }

    void toJson(jvmtiEnv *_jvmti, boost::iostreams::filtering_ostream& out) const {
        out << "{"sv;

        bool first = true;
        for (const auto& [callSite, subTree] : children) {

            if (! first) {
                out << ","sv;
            }

            out << "\""sv << callSite << "\": "sv;
            subTree->toJson(jvmti, out);

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


static std::mutex methodEntryMutex;

bool isSuffixOf(jvmtiFrameInfo* stackA, jint sizeA, jvmtiFrameInfo* stackB, jint sizeB) {
    if (sizeA > sizeB)
        return false;

    // Stacks are ordered in reverse, i.e., the main method is always the last method on the stack.
    // This means we need to compare stacks from the end to the front.
    for (int i = 0; i < sizeA; i++) {
        int frameA = i;
        int frameB = (sizeB - sizeA) + i;
        if (stackA[frameA].method != stackB[frameB].method || stackA[frameA].location != stackB[frameB].location)
            return false;
    }

    return true;
}

static unsigned long long methodCalls = 0;
static unsigned long long suffixes = 0;
static const jint max_stack_depth = 10000;
static jvmtiFrameInfo stack_traces[2][max_stack_depth];
static jint stack_sizes[2];

void JNICALL MethodEntry(jvmtiEnv *jvmti, JNIEnv* jni, jthread thread, jmethodID method) {

    std::lock_guard<std::mutex> lock(methodEntryMutex);

    if (methodCalls % 1000000 == 0) {
        std::cout << "method calls = "sv << methodCalls << ", suffixes = "sv << suffixes << ", "sv
                  << "callTree.size = "sv << callTree.size() << ", callTree.bucketSum = " << callTree.bucketSum() << ", "sv
                  << "callSitePool.size = "sv << callSitePool.size() << ", "sv
                  << "methodPool.size = "sv << methodPool.size() << "\n"sv;
        std::cout.flush();
    }

    static const jint start_depth = 0;

    unsigned int currentStackTrace = methodCalls % 2;
    unsigned int previousStackTrace = (methodCalls - 1) % 2;

    jvmtiError err;
    if ((err = jvmti->GetStackTrace(thread, start_depth, max_stack_depth, stack_traces[currentStackTrace], &stack_sizes[currentStackTrace])) != JVMTI_ERROR_NONE)
        throw std::runtime_error("cannot get stack trace: error "+std::to_string(err));

    if (methodCalls != 0 && !isSuffixOf(stack_traces[previousStackTrace], stack_sizes[previousStackTrace], stack_traces[currentStackTrace], stack_sizes[currentStackTrace])) {
        callTree.addStackTrace(jvmti, stack_traces[previousStackTrace], stack_sizes[previousStackTrace]);
    } else {
        suffixes += 1;
    }

    methodCalls += 1;
}

void return_cg(jvmtiEnv *jvmti) {
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
        callTree.toJson(jvmti, out);

        boost::iostreams::flush(out);

        // std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;

        std::cout << "Write call-sites ...\n" << std::flush;

        out << ", \"callSites\": {"sv;
        bool first = true;
        for (const auto& callSite : callSitePool) {
            if (!first) out << ",\n"sv;

            out << "\""sv << std::addressof(callSite) << "\": "sv;
            callSite.toJson(jvmti, out);

            first = false;
        }
        out << "}"sv;

        boost::iostreams::flush(out);

        // std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;

        std::cout << "Write methods ...\n" << std::flush;

        out << ", \"methods\": {"sv;
        first = true;
        for (const auto& method : methodPool) {
            if (!first) out << ",\n"sv;

            out << "\""sv << std::addressof(method) << "\": "sv;
            method.toJson(jvmti, out);

            first = false;
        }
        out << "}"sv;

        boost::iostreams::flush(out);
        // std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;
    }

    out << "}"sv;

    boost::iostreams::close(out);

    // std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;
}

JNIEXPORT void JNICALL VMDeath(jvmtiEnv *jvmti, JNIEnv* jni_env) {

    // Ensure that all remaining stack traces are added to the call tree.
    callTree.addStackTrace(jvmti, stack_traces[0], stack_sizes[0]);
    callTree.addStackTrace(jvmti, stack_traces[1], stack_sizes[1]);

    return_cg(jvmti);
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
