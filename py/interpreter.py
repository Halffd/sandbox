class Stack:
    def __init__(self, size):
        self.data = [0] * size
        self.pointer = -1
    
    def push(self, value):
        self.pointer += 1
        self.data[self.pointer] = value
    
    def pop(self):
        value = self.data[self.pointer]
        self.pointer -= 1
        return value
    
    def top(self):
        return self.data[self.pointer]

class Interpreter:
    def __init__(self):
        self.stack = Stack(256)
        self.program = []
        self.labels = {}
        self.pc = 0
    
    def parse(self, filename):
        with open(filename, 'r') as f:
            lines = f.readlines()
        
        token_counter = 0
        for line in lines:
            parts = line.strip().split()
            if not parts:
                continue
                
            opcode = parts[0]
            
            if opcode.endswith(':'):
                # It's a label
                self.labels[opcode[:-1]] = token_counter
                continue
            
            self.program.append(opcode)
            token_counter += 1
            
            # Handle instructions with arguments
            if opcode == 'push':
                self.program.append(int(parts[1]))
                token_counter += 1
            elif opcode == 'print':
                string_literal = ' '.join(parts[1:]).strip('"')
                self.program.append(string_literal)
                token_counter += 1
            elif opcode in ['jez', 'jgz']:  # jump if equal/greater than zero
                self.program.append(parts[1])
                token_counter += 1
    
    def run(self):
        while self.pc < len(self.program):
            opcode = self.program[self.pc]
            self.pc += 1
            
            if opcode == 'halt':
                break
            elif opcode == 'push':
                value = self.program[self.pc]
                self.pc += 1
                self.stack.push(value)
            elif opcode == 'pop':
                self.stack.pop()
            elif opcode == 'add':
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.push(a + b)
            elif opcode == 'sub':
                b = self.stack.pop()
                a = self.stack.pop()
                self.stack.push(a - b)
            elif opcode == 'print':
                string_literal = self.program[self.pc]
                self.pc += 1
                print(string_literal)
            elif opcode == 'read':
                value = int(input())
                self.stack.push(value)
            elif opcode == 'jez':
                label = self.program[self.pc]
                if self.stack.top() == 0:
                    self.pc = self.labels[label]
                else:
                    self.pc += 1
            elif opcode == 'jgz':
                label = self.program[self.pc]
                if self.stack.top() > 0:
                    self.pc = self.labels[label]
                else:
                    self.pc += 1

# Usage
import sys
interpreter = Interpreter()
interpreter.parse(sys.argv[1])
interpreter.run()