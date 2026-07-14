#include <jvmti.h>

#include <csignal>
#include <execinfo.h>
#include <unistd.h>

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

struct Method {
    const jmethodID method_id;

    Method(jmethodID method_id): method_id(method_id) {
    }

    void to_json(jvmtiEnv *jvmti, boost::iostreams::filtering_ostream& out) const {

        std::string name;
        std::string declaringClass;
        std::string returnType;
        std::vector<std::string> parameterTypes;

        jclass cls;
        int err;
        if ((err = jvmti->GetMethodDeclaringClass(method_id, &cls)) != JVMTI_ERROR_NONE)
            throw std::runtime_error("cannot get declaring class: error "+std::to_string(err));

        char* className;
        char* generic;
        if ((err = jvmti->GetClassSignature(cls, &className, NULL)) != JVMTI_ERROR_NONE)
            throw std::runtime_error("cannot get class: error "+std::to_string(err));
        declaringClass = std::string(className);
        jvmti->Deallocate((unsigned char*) className);

        char *methodName;
        char *sig;
        jvmti->GetMethodName(method_id, &methodName, &sig, NULL);
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

struct method_hash {
    size_t operator()(const Method& method) const noexcept {
        return reinterpret_cast<size_t>(method.method_id);
    }
};

struct method_equals {
    bool operator()(const Method& lhs, const Method& rhs) const {
        return lhs.method_id == rhs.method_id;
    }
};

std::unordered_set<Method, method_hash, method_equals> method_pool;

struct Call_Site {
    const Method* method;
    const jlocation location;

    Call_Site(jmethodID m, jlocation location)
        : method(&(*method_pool.insert(Method(m)).first)),
          location(location)
    {
    }

    void to_json(jvmtiEnv *jvmti, boost::iostreams::filtering_ostream& out) const {
        jint lntEntries;
        jint lineNumber;
        jvmtiLineNumberEntry* lineNumbers = NULL;
        if (jvmti->GetLineNumberTable(method->method_id, &lntEntries, &lineNumbers) == JVMTI_ERROR_NONE) {
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

struct Call_Site_Hash {
    size_t operator()(const Call_Site& callSite) const noexcept {
        size_t seed = reinterpret_cast<size_t>(callSite.method);
        seed ^= callSite.location + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        return seed;
    }
};

struct Call_Site_Equals {
    bool operator()(const Call_Site& lhs, const Call_Site& rhs) const {
        return lhs.method->method_id == rhs.method->method_id && lhs.location == rhs.location;
    }
};

struct Call_Site_Pointer_Hash {
    size_t operator()(const Call_Site* callSite) const noexcept {
        return reinterpret_cast<size_t>(callSite);
    }
};

struct Call_Site_Pointer_Equals {
    bool operator()(const Call_Site* lhs, const Call_Site* rhs) const {
        return lhs == rhs;
    }
};

std::unordered_set<Call_Site, Call_Site_Hash, Call_Site_Equals> call_site_pool;

struct Call_Tree {
    std::unordered_map<const Call_Site*, std::unique_ptr<Call_Tree>, Call_Site_Pointer_Hash, Call_Site_Pointer_Equals> children;

    Call_Tree() {
        children.reserve(1);
    }

    void add_stack_trace(jvmtiEnv *jvmti, jvmtiFrameInfo* stack_frames, jint stack_size) {
        if(stack_size > 0) {
            auto [it, _inserted] = call_site_pool.insert(Call_Site (stack_frames[stack_size - 1].method, stack_frames[stack_size - 1].location ));

            const Call_Site* topmost = &(*it);

            if(! children.contains(topmost)) {
                children.emplace(topmost, std::make_unique<Call_Tree>());
            }

            children[topmost]->add_stack_trace(jvmti, stack_frames, stack_size - 1);
        }
    }

    void to_json(jvmtiEnv *_jvmti, boost::iostreams::filtering_ostream& out) const {
        out << "{"sv;

        bool first = true;
        for (const auto& [callSite, subTree] : children) {

            if (! first) {
                out << ","sv;
            }

            out << "\""sv << callSite << "\": "sv;
            subTree->to_json(jvmti, out);

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

    unsigned int bucket_sum() const {
        unsigned int sum = children.bucket_count();
        for (const auto& [callSite, subTree] : children) {
            sum += subTree->bucket_sum();
        }
        return sum;
    }
};
static Call_Tree call_tree;

static std::mutex method_entry_mutex;
static unsigned long long method_calls = 0;
static const jint max_stack_depth = 1000000;
static jvmtiFrameInfo stack_trace[max_stack_depth];
static jint stack_size;

void write_cg_to_file(jvmtiEnv *jvmti) {
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
        // std::cout << "Write call tree ...\n" << std::flush;

        out << "\"callTree\": "sv;
        call_tree.to_json(jvmti, out);

        boost::iostreams::flush(out);

        // std::cout << "Write call-sites ...\n" << std::flush;

        out << ", \"callSites\": {"sv;
        bool first = true;
        for (const auto& callSite : call_site_pool) {
            if (!first) out << ",\n"sv;

            out << "\""sv << std::addressof(callSite) << "\": "sv;
            callSite.to_json(jvmti, out);

            first = false;
        }
        out << "}"sv;

        boost::iostreams::flush(out);

        out << ", \"methods\": {"sv;
        first = true;
        for (const auto& method : method_pool) {
            if (!first) out << ",\n"sv;

            out << "\""sv << std::addressof(method) << "\": "sv;
            method.to_json(jvmti, out);

            first = false;
        }
        out << "}"sv;

        boost::iostreams::flush(out);
    }

    out << "}"sv;

    boost::iostreams::close(out);

    // std::cout << call_graph_file_name << " size: " << std::filesystem::file_size(call_graph_file_name) << "\n" << std::flush;
}

void JNICALL MethodEntry(jvmtiEnv *jvmti, JNIEnv* jni, jthread thread, jmethodID method) {

    std::lock_guard<std::mutex> lock(method_entry_mutex);
    try {
        if (method_calls % 1000000 == 0) {
            std::cout << "method calls = "sv << method_calls << ", "sv
                      << "callTree.size = "sv << call_tree.size() << ", callTree.bucketSum = " << call_tree.bucket_sum() << ", "sv
                      << "callSitePool.size = "sv << call_site_pool.size() << ", "sv
                      << "methodPool.size = "sv << method_pool.size();
            if (method_calls % 10000000 == 0) {
                std::cout << ", serialize callgraph\n"sv;
                write_cg_to_file(jvmti);
            } else {
                std::cout << "\n"sv;
            }
            std::cout.flush();
        }
        method_calls += 1;

        static const jint start_depth = 0;

        jvmtiError err;
        if ((err = jvmti->GetStackTrace(thread, start_depth, max_stack_depth, stack_trace, &stack_size)) != JVMTI_ERROR_NONE) {
            throw std::runtime_error("cannot get stacktrace.");
        }

        call_tree.add_stack_trace(jvmti, stack_trace, stack_size);

    } catch (const std::runtime_error& e) {
        std::cout << "JVMTI Agent: Caught runtime error: " << e.what() << '\n';
    } catch (...) {
        std::cerr << "JVMTI Agent: unknown exception\n";
    }

}

JNIEXPORT void JNICALL VMDeath(jvmtiEnv *jvmti, JNIEnv* jni_env) {
    std::lock_guard<std::mutex> lock(method_entry_mutex);
    std::cout << "JVMTI Agent: VMDeath. Start final serialization.";
    write_cg_to_file(jvmti);
    std::cout << "JVMTI Agent: Final serialization finished.";
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
