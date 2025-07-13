#include <string>
#include <cstdio>
#include <iostream>
#include <vector>
#include <sstream>
#ifdef _WIN32
#include <windows.h>  // For Beep(), SetConsoleTextAttribute, etc.
#include <conio.h>    // For _getch()
#else
#include <termios.h>
#endif
class StreamWriter {
private:
    FILE *stream;
public:
    constexpr StreamWriter(FILE *fp) : stream(fp) {}
    
    int WriteLine(const char *str) {
        if (stream == nullptr) return -1;
        if (fputs(str, stream) == EOF) return -1;
        if (fputc('\n', stream) == EOF) return -1;
        return fflush(stream);
    }
    
    int WriteLine(const std::string &str) {
        return WriteLine(str.c_str());
    }
    
    int WriteLine() {
        return fputc('\n', stream) != EOF ? 0 : -1;
    }
    
    int Write(const char *str) {
        if (stream == nullptr) return -1;
        return fputs(str, stream) != EOF ? 0 : -1;
    }
    
    int Write(const std::string &str) {
        return Write(str.c_str());
    }
    
    int Write(char c) {
        return fputc(c, stream) != EOF ? 0 : -1;
    }
};

class StreamReader {
private:
    FILE *stream;
public:
    constexpr StreamReader(FILE *fp) : stream(fp) {}
    
    std::string ReadLine() {
        if (stream == nullptr) return "";
        char buffer[4096];
        if (fgets(buffer, sizeof(buffer), stream) != nullptr) {
            std::string result(buffer);
            // Remove trailing newline
            if (!result.empty() && result.back() == '\n') {
                result.pop_back();
            }
            return result;
        }
        return "";
    }
    
    int Read() {
        return stream ? fgetc(stream) : EOF;
    }
    
    char ReadKey() {
        #ifdef _WIN32
        return _getch();
        #else
        return getchar();  // Simplified for cross-platform
        #endif
    }
};

class _Console {
public:
    StreamWriter Out { stdout };
    StreamWriter Error { stderr };
    StreamReader In { stdin };
    
    // WriteLine overloads
    int WriteLine(const char *str) { return Out.WriteLine(str); }
    int WriteLine(const std::string &str) { return Out.WriteLine(str); }
    int WriteLine() { return Out.WriteLine(); }
    
    template<typename T>
    int WriteLine(const T &value) {
        std::ostringstream oss;
        oss << value;
        return WriteLine(oss.str());
    }
    
    // Write overloads
    int Write(const char *str) { return Out.Write(str); }
    int Write(const std::string &str) { return Out.Write(str); }
    int Write(char c) { return Out.Write(c); }
    
    template<typename T>
    int Write(const T &value) {
        std::ostringstream oss;
        oss << value;
        return Write(oss.str());
    }
    
    // Read methods
    std::string ReadLine() { return In.ReadLine(); }
    int Read() { return In.Read(); }
    char ReadKey() { return In.ReadKey(); }
    
    // Utility methods
    void Clear() {
        #ifdef _WIN32
        system("cls");
        #else
        system("clear");
        #endif
    }
    
    void Beep() {
        #ifdef _WIN32
        Beep(800, 200);
        #else
        std::cout << "\a";
        #endif
    }
    
    void SetCursorPosition(int left, int top) {
        #ifdef _WIN32
        COORD coord;
        coord.X = left;
        coord.Y = top;
        SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), coord);
        #else
        printf("\033[%d;%dH", top + 1, left + 1);
        #endif
    }
    
    void ResetColor() {
        #ifdef _WIN32
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
        #else
        printf("\033[0m");
        #endif
    }
    
    void SetForegroundColor(int color) {
        #ifdef _WIN32
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);
        #else
        printf("\033[%dm", 30 + color);
        #endif
    }
    
    void SetBackgroundColor(int color) {
        #ifdef _WIN32
        CONSOLE_SCREEN_BUFFER_INFO info;
        GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info);
        SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 
                              (info.wAttributes & 0x0F) | (color << 4));
        #else
        printf("\033[%dm", 40 + color);
        #endif
    }
};

class _System {
public:
    _Console Console;
};

static _System System;

int main() {
    System.Console.WriteLine("Hello, world!");
    System.Console.Write("Enter your name: ");
    std::string name = System.Console.ReadLine();
    System.Console.WriteLine("Hello, " + name + "!");
    
    System.Console.WriteLine("Press any key to continue...");
    System.Console.ReadKey();
    
    System.Console.Clear();
    System.Console.SetForegroundColor(2); // Green
    System.Console.WriteLine("This is green text!");
    System.Console.ResetColor();
    
    System.Console.Beep();
    System.Console.SetCursorPosition(10, 5);
    System.Console.WriteLine("Positioned text!");
    
    return 0;
}
