#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <variant>
#include <stdexcept>
#include <memory>
#include <optional>  // Add this line!

class Stack {
private:
    std::vector<int> data;
    static constexpr size_t MAX_SIZE = 256;

public:
    void push(int value) {
        if (data.size() >= MAX_SIZE) {
            throw std::runtime_error("Stack overflow");
        }
        data.push_back(value);
    }
    
    int pop() {
        if (data.empty()) {
            throw std::runtime_error("Stack underflow");
        }
        int value = data.back();
        data.pop_back();
        return value;
    }
    
    int top() const {
        if (data.empty()) {
            throw std::runtime_error("Stack is empty");
        }
        return data.back();
    }
    
    bool empty() const { return data.empty(); }
    size_t size() const { return data.size(); }
};

// Using variant for type-safe instruction arguments
using InstructionArg = std::variant<int, std::string>;

struct Instruction {
    std::string opcode;
    std::optional<InstructionArg> arg;
    
    Instruction(std::string op) : opcode(std::move(op)) {}
    Instruction(std::string op, InstructionArg argument) 
        : opcode(std::move(op)), arg(std::move(argument)) {}
};

class Interpreter {
private:
    Stack stack;
    std::vector<Instruction> program;
    std::unordered_map<std::string, size_t> labels;
    size_t pc = 0;

    std::vector<std::string> tokenize(const std::string& line) {
        std::vector<std::string> tokens;
        std::istringstream iss(line);
        std::string token;
        
        while (iss >> token) {
            tokens.push_back(token);
        }
        return tokens;
    }
    
    std::string extractStringLiteral(const std::vector<std::string>& tokens, size_t start) {
        if (start >= tokens.size()) return "";
        
        std::string result = tokens[start];
        // Remove quotes if present
        if (result.front() == '"' && result.back() == '"') {
            result = result.substr(1, result.length() - 2);
        }
        
        // Handle multi-word strings
        for (size_t i = start + 1; i < tokens.size(); ++i) {
            result += " " + tokens[i];
        }
        
        return result;
    }

public:
    void parseFile(const std::string& filename) {
        std::ifstream file(filename);
        if (!file.is_open()) {
            throw std::runtime_error("Cannot open file: " + filename);
        }
        
        std::string line;
        size_t instruction_counter = 0;
        
        while (std::getline(file, line)) {
            auto tokens = tokenize(line);
            if (tokens.empty()) continue;
            
            const std::string& opcode = tokens[0];
            
            // Handle labels
            if (opcode.back() == ':') {
                std::string label = opcode.substr(0, opcode.length() - 1);
                labels[label] = instruction_counter;
                continue;
            }
            
            // Handle instructions with arguments
            if (opcode == "push") {
                if (tokens.size() < 2) {
                    throw std::runtime_error("Push instruction requires a number");
                }
                int value = std::stoi(tokens[1]);
                program.emplace_back(opcode, value);
            }
            else if (opcode == "print") {
                if (tokens.size() < 2) {
                    throw std::runtime_error("Print instruction requires a string");
                }
                std::string str = extractStringLiteral(tokens, 1);
                program.emplace_back(opcode, str);
            }
            else if (opcode == "jez" || opcode == "jgz") {
                if (tokens.size() < 2) {
                    throw std::runtime_error(opcode + " instruction requires a label");
                }
                program.emplace_back(opcode, tokens[1]);
            }
            else {
                // Instructions without arguments
                program.emplace_back(opcode);
            }
            
            instruction_counter++;
        }
    }
    
    void run() {
        while (pc < program.size()) {
            const auto& instruction = program[pc];
            const std::string& opcode = instruction.opcode;
            
            try {
                if (opcode == "halt") {
                    break;
                }
                else if (opcode == "push") {
                    int value = std::get<int>(instruction.arg.value());
                    stack.push(value);
                }
                else if (opcode == "pop") {
                    stack.pop();
                }
                else if (opcode == "add") {
                    int b = stack.pop();
                    int a = stack.pop();
                    stack.push(a + b);
                }
                else if (opcode == "sub") {
                    int b = stack.pop();
                    int a = stack.pop();
                    stack.push(a - b);
                }
                else if (opcode == "print") {
                    std::string str = std::get<std::string>(instruction.arg.value());
                    std::cout << str << std::endl;
                }
                else if (opcode == "read") {
                    int value;
                    std::cout << "Enter a number: ";
                    if (!(std::cin >> value)) {
                        throw std::runtime_error("Invalid input");
                    }
                    stack.push(value);
                }
                else if (opcode == "jez") {
                    std::string label = std::get<std::string>(instruction.arg.value());
                    if (stack.top() == 0) {
                        auto it = labels.find(label);
                        if (it == labels.end()) {
                            throw std::runtime_error("Label not found: " + label);
                        }
                        pc = it->second;
                        continue;
                    }
                }
                else if (opcode == "jgz") {
                    std::string label = std::get<std::string>(instruction.arg.value());
                    if (stack.top() > 0) {
                        auto it = labels.find(label);
                        if (it == labels.end()) {
                            throw std::runtime_error("Label not found: " + label);
                        }
                        pc = it->second;
                        continue;
                    }
                }
                else {
                    throw std::runtime_error("Unknown instruction: " + opcode);
                }
            }
            catch (const std::exception& e) {
                std::cerr << "Runtime error at instruction " << pc << ": " << e.what() << std::endl;
                return;
            }
            
            pc++;
        }
    }
    
    void debugPrint() const {
        std::cout << "=== Program Debug Info ===" << std::endl;
        std::cout << "Instructions:" << std::endl;
        for (size_t i = 0; i < program.size(); ++i) {
            std::cout << i << ": " << program[i].opcode;
            if (program[i].arg.has_value()) {
                std::visit([](const auto& arg) { std::cout << " " << arg; }, 
                          program[i].arg.value());
            }
            std::cout << std::endl;
        }
        
        std::cout << "\nLabels:" << std::endl;
        for (const auto& [label, pos] : labels) {
            std::cout << label << " -> " << pos << std::endl;
        }
        std::cout << "======================" << std::endl;
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <program_file>" << std::endl;
        return 1;
    }
    
    try {
        Interpreter interpreter;
        interpreter.parseFile(argv[1]);
        interpreter.run();
    }
    catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}