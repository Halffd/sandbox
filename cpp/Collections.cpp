#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <functional>
#include <memory>
#include <stdexcept>
#include <cstdint>    // For uintptr_t and SIZE_MAX
#include <random>     // For proper shuffling
#include <climits>    // Just in case

// Base Object class (like java.lang.Object)
class Object {
public:
    virtual ~Object() = default;
    
    virtual std::string toString() const {
        return "Object@" + std::to_string(reinterpret_cast<uintptr_t>(this));
    }
    
    virtual bool equals(const Object& other) const {
        return this == &other;
    }
    
    virtual size_t hashCode() const {
        return reinterpret_cast<size_t>(this);
    }
    
    virtual Object* clone() const {
        throw std::runtime_error("CloneNotSupportedException");
    }
};

// Generic List interface
template<typename T>
class List : public Object {
public:
    virtual ~List() = default;
    virtual void add(const T& element) = 0;
    virtual void add(size_t index, const T& element) = 0;
    virtual T& get(size_t index) = 0;
    virtual const T& get(size_t index) const = 0;
    virtual T remove(size_t index) = 0;
    virtual bool remove(const T& element) = 0;
    virtual size_t size() const = 0;
    virtual bool isEmpty() const = 0;
    virtual void clear() = 0;
    virtual bool contains(const T& element) const = 0;
    virtual size_t indexOf(const T& element) const = 0;
};

// Helper template to convert things to string
template<typename T>
std::string toStringHelper(const T& value) {
    if constexpr (std::is_same_v<T, std::string>) {
        return "\"" + value + "\"";
    } else if constexpr (std::is_arithmetic_v<T>) {
        return std::to_string(value);
    } else {
        return "Object";  // Give up for complex types
    }
}

// ArrayList implementation
template<typename T>
class ArrayList : public List<T> {
private:
    std::vector<T> data;
    
public:
    ArrayList() = default;
    ArrayList(size_t initialCapacity) { data.reserve(initialCapacity); }
    
    void add(const T& element) override {
        data.push_back(element);
    }
    
    void add(size_t index, const T& element) override {
        if (index > data.size()) {
            throw std::out_of_range("IndexOutOfBoundsException");
        }
        data.insert(data.begin() + index, element);
    }
    
    T& get(size_t index) override {
        if (index >= data.size()) {
            throw std::out_of_range("IndexOutOfBoundsException");
        }
        return data[index];
    }
    
    const T& get(size_t index) const override {
        if (index >= data.size()) {
            throw std::out_of_range("IndexOutOfBoundsException");
        }
        return data[index];
    }
    
    T remove(size_t index) override {
        if (index >= data.size()) {
            throw std::out_of_range("IndexOutOfBoundsException");
        }
        T element = data[index];
        data.erase(data.begin() + index);
        return element;
    }
    
    bool remove(const T& element) override {
        auto it = std::find(data.begin(), data.end(), element);
        if (it != data.end()) {
            data.erase(it);
            return true;
        }
        return false;
    }
    
    size_t size() const override { return data.size(); }
    bool isEmpty() const override { return data.empty(); }
    void clear() override { data.clear(); }
    
    bool contains(const T& element) const override {
        return std::find(data.begin(), data.end(), element) != data.end();
    }
    
    size_t indexOf(const T& element) const override {
        auto it = std::find(data.begin(), data.end(), element);
        return it != data.end() ? std::distance(data.begin(), it) : SIZE_MAX;
    }
    
    std::string toString() const override {
        std::string result = "[";
        for (size_t i = 0; i < data.size(); ++i) {
            result += toStringHelper(data[i]);
            if (i < data.size() - 1) result += ", ";
        }
        result += "]";
        return result;
    }
};

// Set interface
template<typename T>
class SetInterface : public Object {
public:
    virtual ~SetInterface() = default;
    virtual bool add(const T& element) = 0;
    virtual bool remove(const T& element) = 0;
    virtual bool contains(const T& element) const = 0;
    virtual size_t size() const = 0;
    virtual bool isEmpty() const = 0;
    virtual void clear() = 0;
};

// HashSet implementation
template<typename T>
class HashSet : public SetInterface<T> {
private:
    std::set<T> data;  // Using std::set for simplicity
    
public:
    bool add(const T& element) override {
        return data.insert(element).second;
    }
    
    bool remove(const T& element) override {
        return data.erase(element) > 0;
    }
    
    bool contains(const T& element) const override {
        return data.find(element) != data.end();
    }
    
    size_t size() const override { return data.size(); }
    bool isEmpty() const override { return data.empty(); }
    void clear() override { data.clear(); }
    
    std::string toString() const override {
        std::string result = "{";
        bool first = true;
        for (const auto& element : data) {
            if (!first) result += ", ";
            result += toStringHelper(element);
            first = false;
        }
        result += "}";
        return result;
    }
};

// Map interface
template<typename K, typename V>
class MapInterface : public Object {
public:
    virtual ~MapInterface() = default;
    virtual V& put(const K& key, const V& value) = 0;
    virtual V& get(const K& key) = 0;
    virtual const V& get(const K& key) const = 0;
    virtual bool remove(const K& key) = 0;
    virtual bool containsKey(const K& key) const = 0;
    virtual bool containsValue(const V& value) const = 0;
    virtual size_t size() const = 0;
    virtual bool isEmpty() const = 0;
    virtual void clear() = 0;
};

// HashMap implementation
template<typename K, typename V>
class HashMap : public MapInterface<K, V> {
private:
    std::map<K, V> data;
    
public:
    V& put(const K& key, const V& value) override {
        data[key] = value;
        return data[key];
    }
    
    V& get(const K& key) override {
        auto it = data.find(key);
        if (it == data.end()) {
            throw std::runtime_error("Key not found");
        }
        return it->second;
    }
    
    const V& get(const K& key) const override {
        auto it = data.find(key);
        if (it == data.end()) {
            throw std::runtime_error("Key not found");
        }
        return it->second;
    }
    
    bool remove(const K& key) override {
        return data.erase(key) > 0;
    }
    
    bool containsKey(const K& key) const override {
        return data.find(key) != data.end();
    }
    
    bool containsValue(const V& value) const override {
        for (const auto& pair : data) {
            if (pair.second == value) return true;
        }
        return false;
    }
    
    size_t size() const override { return data.size(); }
    bool isEmpty() const override { return data.empty(); }
    void clear() override { data.clear(); }
    
    std::string toString() const override {
        std::string result = "{";
        bool first = true;
        for (const auto& pair : data) {
            if (!first) result += ", ";
            result += toStringHelper(pair.first) + "=" + toStringHelper(pair.second);
            first = false;
        }
        result += "}";
        return result;
    }
};

// Collections utility class
class Collections : public Object {
public:
    template<typename T>
    static void sort(ArrayList<T>& list) {
        std::vector<T> temp;
        for (size_t i = 0; i < list.size(); ++i) {
            temp.push_back(list.get(i));
        }
        std::sort(temp.begin(), temp.end());
        list.clear();
        for (const auto& item : temp) {
            list.add(item);
        }
    }
    
    template<typename T>
    static void reverse(ArrayList<T>& list) {
        std::vector<T> temp;
        for (size_t i = 0; i < list.size(); ++i) {
            temp.push_back(list.get(i));
        }
        std::reverse(temp.begin(), temp.end());
        list.clear();
        for (const auto& item : temp) {
            list.add(item);
        }
    }
    
    template<typename T>
    static void shuffle(ArrayList<T>& list) {
        std::vector<T> temp;
        for (size_t i = 0; i < list.size(); ++i) {
            temp.push_back(list.get(i));
        }
        std::random_device rd;
        std::mt19937 g(rd());
        std::shuffle(temp.begin(), temp.end(), g);
        list.clear();
        for (const auto& item : temp) {
            list.add(item);
        }
    }
    
    template<typename T>
    static T max(const ArrayList<T>& list) {
        if (list.isEmpty()) {
            throw std::runtime_error("NoSuchElementException");
        }
        T maximum = list.get(0);
        for (size_t i = 1; i < list.size(); ++i) {
            if (list.get(i) > maximum) {
                maximum = list.get(i);
            }
        }
        return maximum;
    }
    
    template<typename T>
    static T min(const ArrayList<T>& list) {
        if (list.isEmpty()) {
            throw std::runtime_error("NoSuchElementException");
        }
        T minimum = list.get(0);
        for (size_t i = 1; i < list.size(); ++i) {
            if (list.get(i) < minimum) {
                minimum = list.get(i);
            }
        }
        return minimum;
    }
};

// Test the whole cursed system
int main() {
    std::cout << "=== Testing ArrayList ===" << std::endl;
    ArrayList<int> numbers;
    numbers.add(42);
    numbers.add(13);
    numbers.add(37);
    std::cout << "ArrayList: " << numbers.toString() << std::endl;
    std::cout << "Size: " << numbers.size() << std::endl;
    std::cout << "Get(1): " << numbers.get(1) << std::endl;
    
    std::cout << "\n=== Testing HashSet ===" << std::endl;
    HashSet<std::string> words;
    words.add("Hello");
    words.add("World");
    words.add("Hello");  // Duplicate
    std::cout << "HashSet: " << words.toString() << std::endl;
    std::cout << "Size: " << words.size() << std::endl;
    
    std::cout << "\n=== Testing HashMap ===" << std::endl;
    HashMap<int, std::string> map;
    map.put(1, "One");
    map.put(2, "Two");
    map.put(3, "Three");
    std::cout << "HashMap: " << map.toString() << std::endl;
    std::cout << "Get(2): " << map.get(2) << std::endl;
    
    std::cout << "\n=== Testing Collections ===" << std::endl;
    ArrayList<int> nums;
    nums.add(5);
    nums.add(2);
    nums.add(8);
    nums.add(1);
    std::cout << "Before sort: " << nums.toString() << std::endl;
    Collections::sort(nums);
    std::cout << "After sort: " << nums.toString() << std::endl;
    std::cout << "Max: " << Collections::max(nums) << std::endl;
    std::cout << "Min: " << Collections::min(nums) << std::endl;
    
    return 0;
}