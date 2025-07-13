#include <iostream>
#include <stdexcept>
#include <fstream>
#include <vector>        // YOU FORGOT THIS! 🤦‍♂️
#include <string>
#include <exception>
#include <memory>
// Custom exception hierarchy
class DatabaseException : public std::runtime_error {
public:
    explicit DatabaseException(const std::string& msg) 
        : std::runtime_error("Database Error: " + msg) {}
};

class ConnectionTimeoutException : public DatabaseException {
public:
    explicit ConnectionTimeoutException(const std::string& msg)
        : DatabaseException("Connection Timeout - " + msg) {}
};

// Composite exception for multiple errors
class CompositeException : public std::runtime_error {
private:
    std::vector<std::exception_ptr> causes_;
    
public:
    CompositeException(const std::string& msg, const std::vector<std::exception_ptr>& causes)
        : std::runtime_error(msg), causes_(causes) {}
    
    void printAllErrors() const {
        std::cerr << what() << ":\n";
        for (const auto& cause : causes_) {
            try {
                std::rethrow_exception(cause);
            } catch (const std::exception& e) {
                std::cerr << "  - " << e.what() << "\n";
            }
        }
    }
};

class ValidationException : public std::logic_error {
private:
    std::string field_;
    
public:
    ValidationException(const std::string& field, const std::string& msg)
        : std::logic_error("Validation failed for '" + field + "': " + msg)
        , field_(field) {}
    
    const std::string& getField() const { return field_; }
};

class ChainedException : public std::runtime_error {
private:
    std::exception_ptr cause_;
    
public:
    ChainedException(const std::string& msg, std::exception_ptr cause = nullptr)
        : std::runtime_error(msg), cause_(cause) {}
    
    std::exception_ptr getCause() const { return cause_; }
    
    void printStackTrace() const {
        std::cerr << "Exception: " << what() << std::endl;
        
        if (cause_) {
            std::cerr << "Caused by: ";
            try {
                std::rethrow_exception(cause_);
            } catch (const ChainedException& e) {
                e.printStackTrace();  // Recursive for chained exceptions
            } catch (const std::exception& e) {
                std::cerr << e.what() << std::endl;
            }
        }
    }
};

class DatabaseManager {
public:
    void connect(const std::string& connectionString) {
        try {
            // Simulate network operation
            if (connectionString.empty()) {
                throw std::invalid_argument("Connection string cannot be empty");
            }
            
            // Simulate timeout
            if (connectionString.find("timeout") != std::string::npos) {
                throw ConnectionTimeoutException("Failed to connect within 30 seconds");
            }
            
            std::cout << "Connected successfully!" << std::endl;
            
        } catch (const std::exception& e) {
            // Chain the original exception
            throw ChainedException(
                "Failed to establish database connection", 
                std::current_exception()
            );
        }
    }
    
    void executeQuery(const std::string& sql) {
        if (sql.empty()) {
            throw ValidationException("sql", "Query cannot be empty");
        }
        
        if (sql.find("DROP") != std::string::npos) {
            throw DatabaseException("DROP operations are not allowed");
        }
        
        std::cout << "Query executed: " << sql << std::endl;
    }
};

class UserService {
public:
    void registerUser(const std::string& email, const std::string& password) {
        std::vector<std::exception_ptr> errors;
        
        // Collect all validation errors
        try {
            validateEmail(email);
        } catch (...) {
            errors.push_back(std::current_exception());
        }
        
        try {
            validatePassword(password);
        } catch (...) {
            errors.push_back(std::current_exception());
        }
        
        // If we have errors, throw a composite exception
        if (!errors.empty()) {
            throw CompositeException("User registration failed", errors);
        }
        
        std::cout << "User registered successfully!" << std::endl;
    }
    
private:
    void validateEmail(const std::string& email) {
        if (email.empty()) {
            throw ValidationException("email", "Email is required");
        }
        if (email.find("@") == std::string::npos) {
            throw ValidationException("email", "Invalid email format");
        }
    }
    
    void validatePassword(const std::string& password) {
        if (password.length() < 8) {
            throw ValidationException("password", "Password must be at least 8 characters");
        }
        if (password.find_first_of("0123456789") == std::string::npos) {
            throw ValidationException("password", "Password must contain at least one digit");
        }
    }
};

class FileProcessor {
private:
    std::string filename_;
    
public:
    explicit FileProcessor(const std::string& filename) : filename_(filename) {}
    
    void processFile() {
        std::ifstream file;
        
        try {
            file.open(filename_);
            if (!file.is_open()) {
                throw std::runtime_error("Cannot open file: " + filename_);
            }
            
            std::string line;
            int lineNumber = 0;
            
            while (std::getline(file, line)) {
                lineNumber++;
                try {
                    processLine(line, lineNumber);
                } catch (const std::exception& e) {
                    // Chain with context
                    throw ChainedException(
                        "Error processing line " + std::to_string(lineNumber),
                        std::current_exception()
                    );
                }
            }
            
        } catch (const ChainedException&) {
            // Re-throw chained exceptions as-is
            throw;
        } catch (const std::exception& e) {
            // Wrap other exceptions
            throw ChainedException(
                "Failed to process file: " + filename_,
                std::current_exception()
            );
        }
        // RAII ensures file is closed automatically
    }
    
private:
    void processLine(const std::string& line, int lineNumber) {
        if (line.empty()) {
            throw ValidationException("line", "Empty line not allowed");
        }
        
        // Simulate processing error
        if (line.find("ERROR") != std::string::npos) {
            throw std::runtime_error("Invalid content in line");
        }
        
        std::cout << "Processed line " << lineNumber << ": " << line << std::endl;
    }
};

int main() {
    // Example 1: Database operations
    try {
        DatabaseManager db;
        db.connect("server=localhost;timeout=true");
        db.executeQuery("SELECT * FROM users");
        
    } catch (const ChainedException& e) {
        std::cerr << "=== CHAINED EXCEPTION ===" << std::endl;
        e.printStackTrace();
    } catch (const ValidationException& e) {
        std::cerr << "Validation Error in field '" << e.getField() << "': " 
                  << e.what() << std::endl;
    } catch (const DatabaseException& e) {
        std::cerr << "Database Error: " << e.what() << std::endl;
    }
    
    std::cout << "\n" << std::string(50, '=') << "\n" << std::endl;
    
    // Example 2: User registration
    try {
        UserService userService;
        userService.registerUser("invalid-email", "weak");
        
    } catch (const CompositeException& e) {
        std::cerr << "=== MULTIPLE VALIDATION ERRORS ===" << std::endl;
        e.printAllErrors();
    }
    
    std::cout << "\n" << std::string(50, '=') << "\n" << std::endl;
    
    // Example 3: File processing
    try {
        FileProcessor processor("test.txt");
        processor.processFile();
        
    } catch (const ChainedException& e) {
        std::cerr << "=== FILE PROCESSING ERROR ===" << std::endl;
        e.printStackTrace();
    }
    
    return 0;
}
