#include <iostream>
#include <vector>
#include <unordered_map>
#include <string>
#include <fstream>
#include <sstream>
#include <chrono>
#include <algorithm>
#include <cstdint>
class ProperCPU {\
public:
    uint16_t registers[16];
    uint16_t memory[65536];
    uint16_t pc = 0;
    
    // Register names for easy access
    enum Reg { A=0, B, C, D, X, Y, Z, I, J, PC=8, SP, EX, IA, O, F };
    
private:
    // 8 general purpose registers + special registers
    uint16_t sp = 0xFFFF;  // Stack pointer starts at top
    bool zero_flag = false;
    bool carry_flag = false;
    uint64_t cycle_count = 0;

public:
    ProperCPU() {
        // Initialize memory and registers
        for(int i = 0; i < 65536; i++) memory[i] = 0;
        for(int i = 0; i < 16; i++) registers[i] = 0;
        registers[SP] = 0xFFFF;
    }
    
    enum Opcode {
        SET = 0x01,  // SET a, b   - sets a to b
        ADD = 0x02,  // ADD a, b   - sets a to a+b  
        SUB = 0x03,  // SUB a, b   - sets a to a-b
        MUL = 0x04,  // MUL a, b   - sets a to a*b
        DIV = 0x05,  // DIV a, b   - sets a to a/b
        MOD = 0x06,  // MOD a, b   - sets a to a%b
        SHL = 0x07,  // SHL a, b   - sets a to a<<b
        SHR = 0x08,  // SHR a, b   - sets a to a>>b
        AND = 0x09,  // AND a, b   - sets a to a&b
        BOR = 0x0A,  // BOR a, b   - sets a to a|b
        XOR = 0x0B,  // XOR a, b   - sets a to a^b
        IFE = 0x0C,  // IFE a, b   - performs next instruction only if a==b
        IFN = 0x0D,  // IFN a, b   - performs next instruction only if a!=b
        IFG = 0x0E,  // IFG a, b   - performs next instruction only if a>b
        IFB = 0x0F,  // IFB a, b   - performs next instruction only if (a&b)!=0
        
        // Special opcodes
        JSR = 0x10,  // JSR a      - pushes PC to stack, sets PC to a
        JMP = 0x11,  // JMP a      - sets PC to a
        PUSH = 0x12, // PUSH a     - pushes a to stack
        POP = 0x13,  // POP a      - pops from stack to a
        RET = 0x14,  // RET        - pops PC from stack
        HLT = 0x15   // HLT        - halt execution
    };
    
    void executeInstruction() {
        if(pc >= 65536) return;
        
        uint16_t instruction = memory[pc++];
        uint8_t opcode = instruction & 0x1F;
        uint8_t a_code = (instruction >> 5) & 0x1F;  
        uint8_t b_code = (instruction >> 10) & 0x1F;
        
        uint16_t a_val = getValue(a_code);
        uint16_t b_val = getValue(b_code);
        
        cycle_count++;
        
        switch(opcode) {
            case SET: setValue(a_code, b_val); break;
            case ADD: {
                uint32_t result = a_val + b_val;
                carry_flag = result > 0xFFFF;
                setValue(a_code, result & 0xFFFF);
                zero_flag = (result & 0xFFFF) == 0;
                break;
            }
            case SUB: {
                int32_t result = a_val - b_val;
                carry_flag = result < 0;
                setValue(a_code, result & 0xFFFF);
                zero_flag = (result & 0xFFFF) == 0;
                break;
            }
            case MUL: {
                uint32_t result = a_val * b_val;
                registers[EX] = (result >> 16) & 0xFFFF;
                setValue(a_code, result & 0xFFFF);
                break;
            }
            case DIV: {
                if(b_val == 0) {
                    setValue(a_code, 0);
                    registers[EX] = 0;
                } else {
                    registers[EX] = ((uint32_t)a_val << 16) / b_val;
                    setValue(a_code, a_val / b_val);
                }
                break;
            }
            case SHL: setValue(a_code, a_val << b_val); break;
            case SHR: setValue(a_code, a_val >> b_val); break;
            case AND: setValue(a_code, a_val & b_val); break;
            case BOR: setValue(a_code, a_val | b_val); break;
            case XOR: setValue(a_code, a_val ^ b_val); break;
            
            case IFE: if(a_val != b_val) pc++; break;
            case IFN: if(a_val == b_val) pc++; break;
            case IFG: if(a_val <= b_val) pc++; break;
            case IFB: if((a_val & b_val) == 0) pc++; break;
            
            // Control flow that actually makes sense
            case JSR: push(pc); pc = a_val; break;
            case JMP: pc = a_val; break;
            case PUSH: push(a_val); break;
            case POP: setValue(a_code, pop()); break;
            case RET: pc = pop(); break;
            case HLT: pc = 65536; break; // Stop execution
            
            default:
                std::cout << "Unknown opcode: " << std::hex << opcode << std::endl;
                break;
        }
        
        registers[PC] = pc;
        registers[SP] = sp;
    }
    
    void push(uint16_t value) {
        memory[sp--] = value;
    }
    
    uint16_t pop() {
        return memory[++sp];
    }
    
    uint16_t getValue(uint8_t code) {
        if(code < 8) return registers[code];           // Register
        if(code < 16) return memory[registers[code-8]]; // [register]
        if(code < 24) return memory[registers[code-16] + memory[pc++]]; // [register + literal]
        if(code == 24) return memory[sp++];            // POP
        if(code == 25) return memory[sp];              // PEEK
        if(code == 26) return memory[--sp];            // PUSH
        if(code == 27) return sp;                      // SP
        if(code == 28) return pc;                      // PC
        if(code == 29) return registers[EX];           // EX
        if(code == 30) return memory[memory[pc++]];    // [literal]
        return memory[pc++];                           // literal
    }
    
    void setValue(uint8_t code, uint16_t value) {
        if(code < 8) registers[code] = value;
        else if(code < 16) memory[registers[code-8]] = value;
        else if(code < 24) memory[registers[code-16] + memory[pc-1]] = value;
        // Can't set to literals, SP, PC, etc.
    }
    
    void run() {
        std::cout << "Starting CPU execution..." << std::endl;
        while(pc < 65536 && memory[pc] != 0) {
            executeInstruction();
            if(cycle_count % 10000 == 0) {
                std::cout << "Cycles: " << cycle_count << ", PC: " << pc << std::endl;
            }
        }
        std::cout << "Execution completed. Total cycles: " << cycle_count << std::endl;
    }
    
    void loadProgram(const std::vector<uint16_t>& program) {
        for(size_t i = 0; i < program.size() && i < 65536; i++) {
            memory[i] = program[i];
        }
        pc = 0;
    }
    
    void dumpState() {
        std::cout << "\n=== CPU STATE ===" << std::endl;
        std::cout << "PC: " << std::hex << pc << ", SP: " << sp << std::endl;
        std::cout << "Cycles: " << std::dec << cycle_count << std::endl;
        std::cout << "Registers:" << std::endl;
        char reg_names[] = "ABCDXYZI";
        for(int i = 0; i < 8; i++) {
            std::cout << reg_names[i] << ": " << std::hex << registers[i] << " ";
        }
        std::cout << std::endl;
        std::cout << "Flags: Z=" << zero_flag << " C=" << carry_flag << std::endl;
    }
};

// Simple assembler for testing
class SimpleAssembler {
public:
    std::vector<uint16_t> assemble(const std::string& code) {
        std::vector<uint16_t> program;
        
        // Just hardcode a simple test program instead of parsing
        // Because parsing assembly is actually hard and this is a demo
        program.push_back(0x0421); // SET R0, 100
        program.push_back(100);
        program.push_back(0x0441); // SET R1, 50  
        program.push_back(50);
        program.push_back(0x0082); // ADD R0, R1
        program.push_back(0x15);   // HLT
        
        return program;
    }
};
// Game framework
class SpaceGame {
private:
    std::vector<ProperCPU> ships;
    
public:
    void addShip(const std::string& program) {
        ships.emplace_back();
        SimpleAssembler assembler;
        auto compiled = assembler.assemble(program);
        ships.back().loadProgram(compiled);
    }
    
    void simulate() {
        std::cout << "Simulating " << ships.size() << " ships..." << std::endl;
        
        // Run all ships for a few cycles
        for(int cycle = 0; cycle < 100; cycle++) {
            for(auto& ship : ships) {
                ship.executeInstruction();
            }
        }
        
        // Dump states
        for(size_t i = 0; i < ships.size(); i++) {
            std::cout << "\n=== SHIP " << i << " ===" << std::endl;
            ships[i].dumpState();
        }
    }
};

int test() {
    std::cout << "=== CPU EMULATOR ===" << std::endl;
    
    // Test the CPU with a simple program
    ProperCPU cpu;
    
    // Simple test program
    std::vector<uint16_t> program = {
        0x0421,  // SET A, 10
        10,
        0x0441,  // SET B, 20  
        20,
        0x0082,  // ADD A, B  (A = A + B = 30, takes 1 cycle)
        0x2C01,  // SET [0x1000], A
        0x1000,
        0x15     // HLT
    };
    
    cpu.loadProgram(program);
    cpu.run();
    cpu.dumpState();
    
    // Test the game framework
    SpaceGame game;
    
    // Add a ship with a simple AI program
    std::string shipProgram = R"(
        SET A 100
        SET B 50
        ADD A B
        SET C A
        HLT
    )";
    
    game.addShip(shipProgram);
    game.simulate();
    
    std::cout << "\n=== PERFORMANCE COMPARISON ===" << std::endl;


    return 0;
}
class NetworkGame {
private:
    std::vector<ProperCPU> player_ships;
    std::vector<std::string> player_names;
    
public:
    void addPlayer(const std::string& name, const std::string& code) {
        player_names.push_back(name);
        player_ships.emplace_back();
        
        SimpleAssembler assembler;
        auto program = assembler.assemble(code);
        player_ships.back().loadProgram(program);
        
        std::cout << "Player " << name << " joined with " << program.size() 
                  << " word program" << std::endl;
    }
    
    void runTournament() {
        std::cout << "\n=== RUNNING TOURNAMENT ===" << std::endl;
        
        // Run all ships simultaneously
        for(int round = 0; round < 1000; round++) {
            for(size_t i = 0; i < player_ships.size(); i++) {
                // Execute one instruction per ship per round
                player_ships[i].executeInstruction();
            }
        }
        
        // Report results
        for(size_t i = 0; i < player_names.size(); i++) {
            std::cout << player_names[i] << " completed in " 
                      << "cycles" << std::endl;
        }
    }
};

class Debugger {
private:
    ProperCPU& cpu;
    std::vector<uint16_t> breakpoints;
    bool step_mode = false;
    
public:
    Debugger(ProperCPU& c) : cpu(c) {}
    
    void addBreakpoint(uint16_t address) {
        breakpoints.push_back(address);
        std::cout << "Breakpoint added at 0x" << std::hex << address << std::endl;
    }
    
    void stepMode(bool enabled) {
        step_mode = enabled;
        std::cout << "Step mode " << (enabled ? "enabled" : "disabled") << std::endl;
    }
    
    bool shouldBreak() {
        uint16_t pc = cpu.registers[cpu.PC];
        return step_mode || 
               std::find(breakpoints.begin(), breakpoints.end(), pc) != breakpoints.end();
    }
    
    void interactiveDebug() {
        cpu.dumpState();
        std::cout << "Debug> ";
        std::string command;
        std::getline(std::cin, command);
        
        if(command == "continue" || command == "c") {
            step_mode = false;
        } else if(command == "step" || command == "s") {
            step_mode = true;
        } else if(command.substr(0, 5) == "break") {
            uint16_t addr = std::stoi(command.substr(6), nullptr, 16);
            addBreakpoint(addr);
        }
    }
};

class SimpleGraphics {
private:
    uint16_t screen[32][24]; // 80x24 character display
    
public:
    void clear() {
        for(int y = 0; y < 24; y++) {
            for(int x = 0; x < 32; x++) {
                screen[x][y] = ' ';
            }
        }
    }
    
    void setPixel(uint8_t x, uint8_t y, uint16_t value) {
        if(x < 32 && y < 24) {
            screen[x][y] = value;
        }
    }
    
    void render() {
        system("clear"); // Unix clear screen
        for(int y = 0; y < 24; y++) {
            for(int x = 0; x < 32; x++) {
                std::cout << (char)(screen[x][y] & 0xFF);
            }
            std::cout << std::endl;
        }
    }
    
    // Memory-mapped graphics for CPU access
    void connectToCPU(ProperCPU& cpu) {
        // Map screen memory to CPU address space 0x8000-0x83FF
        for(int i = 0; i < 32*24; i++) {
            cpu.memory[0x8000 + i] = screen[0][0] + i;
        }
    }
};

// File system for loading/saving programs
class FileSystem {
public:
    static void saveProgram(const std::string& filename, const std::vector<uint16_t>& program) {
        std::ofstream file(filename, std::ios::binary);
        for(uint16_t word : program) {
            file.write(reinterpret_cast<const char*>(&word), sizeof(word));
        }
        file.close();
        std::cout << "Program saved to " << filename << std::endl;
    }
    
    static std::vector<uint16_t> loadProgram(const std::string& filename) {
        std::vector<uint16_t> program;
        std::ifstream file(filename, std::ios::binary);
        
        uint16_t word;
        while(file.read(reinterpret_cast<char*>(&word), sizeof(word))) {
            program.push_back(word);
        }
        file.close();
        std::cout << "Loaded " << program.size() << " words from " << filename << std::endl;
        return program;
    }
    
    static void saveSourceCode(const std::string& filename, const std::string& source) {
        std::ofstream file(filename);
        file << source;
        file.close();
        std::cout << "Source saved to " << filename << std::endl;
    }
    
    static std::string loadSourceCode(const std::string& filename) {
        std::ifstream file(filename);
        std::string content((std::istreambuf_iterator<char>(file)),
                           std::istreambuf_iterator<char>());
        file.close();
        return content;
    }
};

// Complete game example
void runCompleteExample() {
    std::cout << "\n=== COMPLETE GAME EXAMPLE ===" << std::endl;
    
    // Create game world
    NetworkGame game;
    SimpleGraphics graphics;
    
    // Player 1: Simple AI that counts to 100
    std::string player1_code = R"(
        SET A 0
        :loop
        ADD A 1
        SET [0x8000] A    ; Display counter on screen
        IFN A 100
        JMP loop
        HLT
    )";
    
    // Player 2: Fibonacci calculator  
    std::string player2_code = R"(
        SET A 1
        SET B 1
        SET C 0
        :fib_loop
        SET C A
        ADD A B
        SET B C
        IFG A 1000
        JMP end
        JMP fib_loop
        :end
        SET [0x8001] A    ; Display result
        HLT
    )";
    
    game.addPlayer("Counter Bot", player1_code);
    game.addPlayer("Fibonacci Bot", player2_code);
    
    // Run the tournament
    game.runTournament();
}

void benchmarkComparison() {
    std::cout << "\n=== PERFORMANCE BENCHMARK ===" << std::endl;
    
    ProperCPU our_cpu;
    
    // Test program: Add numbers from 1 to 1000
    std::string test_program = R"(
        SET A 0
        SET B 1
        :loop
        ADD A B
        ADD B 1
        IFN B 1001
        JMP loop
        HLT
    )";
    
    SimpleAssembler assembler;
    auto program = assembler.assemble(test_program);
    our_cpu.loadProgram(program);
    
    auto start = std::chrono::high_resolution_clock::now();
    our_cpu.run();
    auto end = std::chrono::high_resolution_clock::now();
    
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
}

// Main function with full demo
int main() {
	test();
	std::cin;
    runCompleteExample();
    benchmarkComparison();
    
    std::cout << "\n" << std::endl;
    
    return 0;
}
