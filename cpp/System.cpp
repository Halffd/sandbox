#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <cstdio>
#include <cstdarg>
#include <set>
#include <memory>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

// Global "garbage collector" - still cursed but won't infinite loop
class GarbageCollector {
private:
    void** allocated_ptrs;  // Use raw array to avoid std::set recursion
    size_t capacity;
    size_t count;
    bool in_allocation;     // Prevent recursion
    
public:
    static GarbageCollector& getInstance() {
        static GarbageCollector instance;
        return instance;
    }
    
    GarbageCollector() : capacity(1000), count(0), in_allocation(false) {
        allocated_ptrs = (void**)malloc(capacity * sizeof(void*));
    }
    
    void registerPtr(void* ptr) {
        if (in_allocation || !ptr) return;  // Prevent recursion and null ptrs
        
        if (count >= capacity) {
            // Don't resize - just ignore (peak garbage collection logic)
            return;
        }
        allocated_ptrs[count++] = ptr;
    }
    
    void unregisterPtr(void* ptr) {
        if (in_allocation || !ptr) return;
        
        for (size_t i = 0; i < count; ++i) {
            if (allocated_ptrs[i] == ptr) {
                // Shift everything down
                for (size_t j = i; j < count - 1; ++j) {
                    allocated_ptrs[j] = allocated_ptrs[j + 1];
                }
                count--;
                break;
            }
        }
    }
    
    void collect() {
        std::cout << "[GC] Collecting " << count << " objects..." << std::endl;
        in_allocation = true;  // Prevent tracking during cleanup
        
        for (size_t i = 0; i < count; ++i) {
            std::cout << "[GC] Freeing object at " << allocated_ptrs[i] << std::endl;
            free(allocated_ptrs[i]);  // Still mixing malloc/free with new/delete
        }
        count = 0;
        in_allocation = false;
        std::cout << "[GC] Collection complete. Still probably broke everything." << std::endl;
    }
};

// Overload new to track allocations (still insane but won't crash immediately)
void* operator new(size_t size) noexcept(false) {
    void* ptr = malloc(size);
    if (!ptr) throw std::bad_alloc();
    
    GarbageCollector::getInstance().registerPtr(ptr);
    std::cout << "[GC] Allocated " << size << " bytes at " << ptr << std::endl;
    return ptr;
}

void operator delete(void* ptr) noexcept {
    if (!ptr) return;
    GarbageCollector::getInstance().unregisterPtr(ptr);
    std::cout << "[GC] Manually freed " << ptr << std::endl;
    free(ptr);
}

template <typename T>
void print(T value) {
    std::cout << value << std::endl;
}

#define puts(x) std::cout << x << std::endl

class SystemCS {
public:
    void Beep() {
        #ifdef _WIN32
        Beep(800, 200);
        #else
        std::cout << "\a";
        #endif
    }
};

class ConsoleCS {
public:
    void WriteLine(const std::string& str) {
        std::cout << str << std::endl;
    }
    
    void Write(const char* format, const char* str) {
        printf(format, str);
    }
};

class ConsoleJS {
public:
    void log(const std::string& str) {
        std::cout << "[LOG] " << str << std::endl;
    }
    
    void debug(const std::string& str) {
        std::cout << "[DEBUG] " << str << std::endl;
    }
    
    void dir(const std::vector<std::string>& obj) {
        std::cout << "[DIR] {";
        for (size_t i = 0; i < obj.size(); ++i) {
            std::cout << "\"" << obj[i] << "\"";
            if (i < obj.size() - 1) std::cout << ", ";
        }
        std::cout << "}" << std::endl;
    }
};

class SystemJava {
public:
    class Out {
    public:
        void print(const std::string& str) {
            std::cout << str;
        }
        
        void println(const std::string& str) {
            std::cout << str << std::endl;
        }
        
        void printf(const char* format, const char* str) {
            ::printf(format, str);
            std::cout << std::endl;
        }
    };
    
    Out out;
    
    void gc() {
        std::cout << "[JAVA GC] Triggering garbage collection..." << std::endl;
        GarbageCollector::getInstance().collect();
        std::cout << "[JAVA GC] If your program crashes now, that's working as intended" << std::endl;
    }
};

int main() {
    print("Hello");
    puts(" World");
    
    SystemCS* System = new SystemCS();
    System->Beep();
    
    ConsoleCS* Console = new ConsoleCS();
    Console->WriteLine("Hello");
    Console->Write("Hello %s\n", "World");
    
    ConsoleJS* console = new ConsoleJS();
    console->log("Hello");
    console->debug("Hello");
    console->dir({"Hello", "World"});
    
    SystemJava* SystemJ = new SystemJava();
    SystemJ->out.print("Hello");
    SystemJ->out.println(" World");
    SystemJ->out.printf("Hello %s", "World");
    
    // The moment of truth - trigger "garbage collection"
    SystemJ->gc();
    
    std::cout << "If you see this message, the GC didn't crash (yet)" << std::endl;
    
    return 0;
}