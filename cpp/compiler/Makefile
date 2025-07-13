CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2 -g
TARGET = compiler
SOURCES = compiler.cpp

# Build the compiler
$(TARGET): $(SOURCES)
    $(CXX) $(CXXFLAGS) -o $(TARGET) $(SOURCES)

# Test with a sample program
test: $(TARGET) hello.ol
    ./$(TARGET) hello.ol

# Create a sample program if it doesn't exist
hello.ol:
    @echo 'Creating sample program...'
    @echo 'print "Hello from our language!"' > hello.ol
    @echo 'push 42' >> hello.ol
    @echo 'push 13' >> hello.ol
    @echo 'add' >> hello.ol
    @echo 'halt' >> hello.ol

# Install dependencies (Ubuntu/Debian)
deps:
    sudo apt update
    sudo apt install -y nasm gcc g++ build-essential

# Clean build artifacts
clean:
    rm -f $(TARGET) *.o *.asm hello hello.ol

# Clean everything including test programs
cleanall: clean
    rm -f *.ol

.PHONY: test deps clean cleanall