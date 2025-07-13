#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <filesystem>
#include <cstdlib>
#include <stdexcept>

class LinuxCompiler {
private:
    std::vector<std::string> tokens;
    std::vector<std::string> string_literals;
    std::string assembly_file;
    std::string object_file;
    std::string exe_file;
    
    void tokenize(const std::string& filename) {
        std::ifstream file(filename);
        if (!file.is_open()) {
            throw std::runtime_error("Cannot open source file: " + filename);
        }
        
        std::string line;
        while (std::getline(file, line)) {
            std::istringstream iss(line);
            std::string token;
            
            if (!(iss >> token) || token.empty()) continue;
            
            tokens.push_back(token);
            
            if (token == "push") {
                int value;
                iss >> value;
                tokens.push_back(std::to_string(value));
            }
            else if (token == "print") {
                std::string str_literal;
                std::getline(iss, str_literal);
                size_t start = str_literal.find('"');
                size_t end = str_literal.rfind('"');
                if (start != std::string::npos && end != std::string::npos && start < end) {
                    str_literal = str_literal.substr(start + 1, end - start - 1);
                } else {
                    str_literal = str_literal.substr(1);
                }
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
        if (!asm_file.is_open()) {
            throw std::runtime_error("Cannot create assembly file: " + assembly_file);
        }
        
        asm_file << "bits 64\n";
        asm_file << "default rel\n\n";
        
        asm_file << "section .bss\n";
        asm_file << "    read_number resq 1\n";
        asm_file << "    stack_memory resq 256  ; Our virtual stack\n";
        asm_file << "    stack_ptr resq 1       ; Stack pointer\n\n";
        
        asm_file << "section .data\n";
        asm_file << "    read_format db \"%ld\", 0\n";
        asm_file << "    newline db 10, 0\n";
        asm_file << "    prompt db \"Enter number: \", 0\n";
        
        for (size_t i = 0; i < string_literals.size(); ++i) {
            asm_file << "    string_literal_" << i << " db \"" 
                     << string_literals[i] << "\", 0\n";
        }
        asm_file << "\n";
        
        asm_file << "section .text\n";
        asm_file << "    global main\n";
        asm_file << "    extern printf\n";
        asm_file << "    extern scanf\n\n";
        
        asm_file << "main:\n";
        asm_file << "    push rbp\n";
        asm_file << "    mov rbp, rsp\n";
        asm_file << "    and rsp, -16  ; Align stack\n";
        asm_file << "    sub rsp, 64   ; Reserve space for calls\n";
        asm_file << "    ; Initialize our virtual stack\n";
        asm_file << "    lea rax, [rel stack_memory]\n";
        asm_file << "    mov [rel stack_ptr], rax\n\n";
        
        for (size_t ip = 0; ip < tokens.size(); ++ip) {
            const std::string& opcode = tokens[ip];
            
            if (opcode.back() == ':') {
                asm_file << opcode << "\n";
            }
            else if (opcode == "push") {
                int value = std::stoi(tokens[++ip]);
                asm_file << "    ; Push " << value << "\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    mov qword [rax], " << value << "\n";
                asm_file << "    add qword [rel stack_ptr], 8\n";
            }
            else if (opcode == "pop") {
                asm_file << "    ; Pop\n";
                asm_file << "    sub qword [rel stack_ptr], 8\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    mov rax, [rax]\n";
            }
            else if (opcode == "add") {
                asm_file << "    ; Add\n";
                asm_file << "    sub qword [rel stack_ptr], 8\n";  // Pop first\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    mov rbx, [rax]\n";
                asm_file << "    sub qword [rel stack_ptr], 8\n";  // Pop second\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    add [rax], rbx\n";               // Add and store\n";
                asm_file << "    add qword [rel stack_ptr], 8\n";  // Push result\n";
            }
            else if (opcode == "sub") {
                asm_file << "    ; Subtract\n";
                asm_file << "    sub qword [rel stack_ptr], 8\n";  // Pop first (top)\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    mov rbx, [rax]\n";
                asm_file << "    sub qword [rel stack_ptr], 8\n";  // Pop second\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    sub [rax], rbx\n";               // second - first\n";
                asm_file << "    add qword [rel stack_ptr], 8\n";  // Push result\n";
            }
            else if (opcode == "print") {
                int str_index = std::stoi(tokens[++ip]);
                asm_file << "    ; Print string\n";
                asm_file << "    lea rdi, [rel string_literal_" << str_index << "]\n";
                asm_file << "    xor eax, eax\n";
                asm_file << "    call printf\n";
                asm_file << "    lea rdi, [rel newline]\n";
                asm_file << "    xor eax, eax\n";
                asm_file << "    call printf\n";
            }
            else if (opcode == "read") {
                asm_file << "    ; Read number\n";
                asm_file << "    lea rdi, [rel prompt]\n";
                asm_file << "    xor eax, eax\n";
                asm_file << "    call printf\n";
                asm_file << "    lea rdi, [rel read_format]\n";
                asm_file << "    lea rsi, [rel read_number]\n";
                asm_file << "    xor eax, eax\n";
                asm_file << "    call scanf\n";
                asm_file << "    ; Push the read value\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    mov rbx, [rel read_number]\n";
                asm_file << "    mov [rax], rbx\n";
                asm_file << "    add qword [rel stack_ptr], 8\n";
            }
            else if (opcode == "jez") {
                std::string label = tokens[++ip];
                asm_file << "    ; Jump if equal to zero\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    sub rax, 8\n";
                asm_file << "    cmp qword [rax], 0\n";
                asm_file << "    je " << label << "\n";
            }
            else if (opcode == "jgz") {
                std::string label = tokens[++ip];
                asm_file << "    ; Jump if greater than zero\n";
                asm_file << "    mov rax, [rel stack_ptr]\n";
                asm_file << "    sub rax, 8\n";
                asm_file << "    cmp qword [rax], 0\n";
                asm_file << "    jg " << label << "\n";
            }
            else if (opcode == "halt") {
                asm_file << "    ; Halt program\n";
                asm_file << "    jmp program_exit\n";
            }
        }
        
        asm_file << "\nprogram_exit:\n";
        asm_file << "    mov rsp, rbp\n";
        asm_file << "    pop rbp\n";
        asm_file << "    mov eax, 0\n";
        asm_file << "    ret\n";
        
        asm_file.close();
    }
    
    void assemble() {
        std::string cmd = "nasm -f elf64 " + assembly_file + " -o " + object_file;
        if (std::system(cmd.c_str()) != 0) {
            throw std::runtime_error("Assembly failed");
        }
    }
    
    void link() {
        std::string cmd = "gcc -no-pie " + object_file + " -o " + exe_file;
        std::cout << "Running: " << cmd << std::endl;
        if (std::system(cmd.c_str()) != 0) {
            throw std::runtime_error("Linking failed");
        }
    }
    
    void run() {
        std::string cmd = "./" + exe_file;
        std::cout << "Running: " << cmd << std::endl;
        std::system(cmd.c_str());
    }

public:
    void compile(const std::string& source_file) {
        std::filesystem::path p(source_file);
        std::string base_name = p.stem().string();
        assembly_file = base_name + ".asm";
        object_file = base_name + ".o";
        exe_file = base_name;
        
        std::cout << "🔥 Compiling " << source_file << " for Linux x86-64" << std::endl;
        
        tokenize(source_file);
        preprocessStringLiterals();
        generateAssembly();
        assemble();
        link();
        run();
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <source_file.lang>" << std::endl;
        return 1;
    }
    
    try {
        LinuxCompiler compiler;
        compiler.compile(argv[1]);
    }
    catch (const std::exception& e) {
        std::cerr << "💥 Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}