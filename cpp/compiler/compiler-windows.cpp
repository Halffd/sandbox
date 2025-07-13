#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>
#include <filesystem>
#include <cstdlib>

class Compiler {
private:
    std::vector<std::string> tokens;
    std::vector<std::string> string_literals;
    std::string assembly_file;
    std::string object_file;
    std::string exe_file;
    
    void tokenize(const std::string& filename) {
        std::ifstream file(filename);
        std::string line;
        
        while (std::getline(file, line)) {
            std::istringstream iss(line);
            std::string token;
            
            // Get first token (opcode)
            if (!(iss >> token) || token.empty()) continue;
            
            tokens.push_back(token);
            
            // Handle instructions with arguments
            if (token == "push") {
                int value;
                iss >> value;
                tokens.push_back(std::to_string(value));
            }
            else if (token == "print") {
                std::string str_literal;
                std::getline(iss, str_literal);
                // Remove quotes and leading space
                str_literal = str_literal.substr(str_literal.find('"') + 1);
                str_literal = str_literal.substr(0, str_literal.rfind('"'));
                tokens.push_back(str_literal);
            }
            else if (token == "jez" || token == "jgz") {
                std::string label;
                iss >> label;
                tokens.push_back(label);
            }
        }
    }
    
    void preprocessStringLiterals() {
        for (size_t i = 0; i < tokens.size(); ++i) {
            if (i > 0 && tokens[i-1] == "print") {
                string_literals.push_back(tokens[i]);
                tokens[i] = std::to_string(string_literals.size() - 1);
            }
        }
    }
    
    void generateAssembly() {
        std::ofstream asm_file(assembly_file);
        
        // NASM directives
        asm_file << "bits 64\n";
        asm_file << "default rel\n\n";
        
        // Sections
        asm_file << "section .bss\n";
        asm_file << "    read_number resq 1\n\n";
        
        asm_file << "section .data\n";
        asm_file << "    read_format db \"%d\", 0\n";
        
        // String literals
        for (size_t i = 0; i < string_literals.size(); ++i) {
            asm_file << "    string_literal_" << i << " db \"" 
                     << string_literals[i] << "\", 0\n";
        }
        asm_file << "\n";
        
        asm_file << "section .text\n";
        asm_file << "    global main\n";
        asm_file << "    extern ExitProcess\n";
        asm_file << "    extern printf\n";
        asm_file << "    extern scanf\n\n";
        
        asm_file << "main:\n";
        asm_file << "    ; Setup stack frame\n";
        asm_file << "    push rbp\n";
        asm_file << "    mov rbp, rsp\n";
        asm_file << "    sub rsp, 32  ; Shadow space\n\n";
        
        // Process tokens
        for (size_t ip = 0; ip < tokens.size(); ++ip) {
            const std::string& opcode = tokens[ip];
            
            if (opcode.back() == ':') {
                asm_file << "    ; Label: " << opcode << "\n";
                asm_file << opcode << "\n";
            }
            else if (opcode == "push") {
                int value = std::stoi(tokens[++ip]);
                asm_file << "    ; Push " << value << "\n";
                asm_file << "    push " << value << "\n";
            }
            else if (opcode == "pop") {
                asm_file << "    ; Pop\n";
                asm_file << "    pop rax\n";
            }
            else if (opcode == "add") {
                asm_file << "    ; Add\n";
                asm_file << "    pop rax\n";
                asm_file << "    add qword [rsp], rax\n";
            }
            else if (opcode == "sub") {
                asm_file << "    ; Subtract\n";
                asm_file << "    pop rax\n";
                asm_file << "    sub qword [rsp], rax\n";
            }
            else if (opcode == "print") {
                int str_index = std::stoi(tokens[++ip]);
                asm_file << "    ; Print string\n";
                asm_file << "    lea rcx, [string_literal_" << str_index << "]\n";
                asm_file << "    xor eax, eax\n";
                asm_file << "    call printf\n";
            }
            else if (opcode == "read") {
                asm_file << "    ; Read number\n";
                asm_file << "    lea rcx, [read_format]\n";
                asm_file << "    lea rdx, [read_number]\n";
                asm_file << "    xor eax, eax\n";
                asm_file << "    call scanf\n";
                asm_file << "    push qword [read_number]\n";
            }
            else if (opcode == "jez") {
                std::string label = tokens[++ip];
                asm_file << "    ; Jump if equal to zero\n";
                asm_file << "    cmp qword [rsp], 0\n";
                asm_file << "    je " << label << "\n";
            }
            else if (opcode == "jgz") {
                std::string label = tokens[++ip];
                asm_file << "    ; Jump if greater than zero\n";
                asm_file << "    cmp qword [rsp], 0\n";
                asm_file << "    jg " << label << "\n";
            }
            else if (opcode == "halt") {
                asm_file << "    ; Halt - jump to exit\n";
                asm_file << "    jmp exit\n";
            }
        }
        
        asm_file << "\nexit:\n";
        asm_file << "    xor eax, eax\n";
        asm_file << "    call ExitProcess\n";
        
        asm_file.close();
    }
    
    void assemble() {
        std::string cmd = "nasm -f win64 " + assembly_file;
        if (std::system(cmd.c_str()) != 0) {
            throw std::runtime_error("Assembly failed");
        }
    }
    
    void link() {
        std::string cmd = "gcc -o " + exe_file + " " + object_file;
        if (std::system(cmd.c_str()) != 0) {
            throw std::runtime_error("Linking failed");
        }
    }
    
    void run() {
        std::string cmd = "./" + exe_file;
        std::system(cmd.c_str());
    }

public:
    void compile(const std::string& source_file) {
        // Setup file names
        std::filesystem::path p(source_file);
        std::string base_name = p.stem().string();
        assembly_file = base_name + ".asm";
        object_file = base_name + ".obj";
        exe_file = base_name + ".exe";
        
        std::cout << "Tokenizing..." << std::endl;
        tokenize(source_file);
        
        std::cout << "Preprocessing..." << std::endl;
        preprocessStringLiterals();
        
        std::cout << "Generating assembly..." << std::endl;
        generateAssembly();
        
        std::cout << "Assembling..." << std::endl;
        assemble();
        
        std::cout << "Linking..." << std::endl;
        link();
        
        std::cout << "Running..." << std::endl;
        run();
    }
    
    void debugTokens() const {
        std::cout << "Tokens: ";
        for (const auto& token : tokens) {
            std::cout << token << " ";
        }
        std::cout << std::endl;
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <source_file>" << std::endl;
        return 1;
    }
    
    try {
        Compiler compiler;
        compiler.compile(argv[1]);
    }
    catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}