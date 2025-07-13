#include <iostream>
#include <string>
#include <vector>
#include <filesystem>
#include <fstream>
#include <map>

class InstallerWizard {
private:
    std::string appName;
    std::string installPath;
    std::string version;
    std::vector<std::string> files;
    std::map<std::string, std::string> registryEntries;
    bool createDesktopShortcut;
    bool createStartMenuShortcut;
    
public:
    InstallerWizard(const std::string& name, const std::string& ver) 
        : appName(name), version(ver), createDesktopShortcut(false), createStartMenuShortcut(false) {
        installPath = "C:\\Program Files\\" + appName;
    }
    
    void welcome() {
        std::cout << "=== " << appName << " Installation Wizard ===" << std::endl;
        std::cout << "Version: " << version << std::endl;
        std::cout << "This wizard will install " << appName << " on your computer." << std::endl;
        std::cout << "\nPress Enter to continue...";
        std::cin.get();
    }
    
    void selectInstallPath() {
        std::cout << "\n=== Installation Directory ===" << std::endl;
        std::cout << "Default: " << installPath << std::endl;
        std::cout << "Enter custom path (or press Enter for default): ";
        
        std::string customPath;
        std::getline(std::cin, customPath);
        
        if (!customPath.empty()) {
            installPath = customPath;
        }
        
        std::cout << "Installing to: " << installPath << std::endl;
    }
    
    void selectComponents() {
        std::cout << "\n=== Select Components ===" << std::endl;
        
        char choice;
        std::cout << "Create desktop shortcut? (y/n): ";
        std::cin >> choice;
        createDesktopShortcut = (choice == 'y' || choice == 'Y');
        
        std::cout << "Create start menu shortcut? (y/n): ";
        std::cin >> choice;
        createStartMenuShortcut = (choice == 'y' || choice == 'Y');
        
        std::cin.ignore(); // Clear input buffer
    }
    
    void addFile(const std::string& filename) {
        files.push_back(filename);
    }
    
    void addRegistryEntry(const std::string& key, const std::string& value) {
        registryEntries[key] = value;
    }
    
    bool install() {
        std::cout << "\n=== Installing ===" << std::endl;
        
        // Create installation directory
        try {
            std::filesystem::create_directories(installPath);
            std::cout << "Created directory: " << installPath << std::endl;
        } catch (const std::exception& e) {
            std::cerr << "Failed to create directory: " << e.what() << std::endl;
            return false;
        }
        
        // Copy files
        for (const auto& file : files) {
            std::cout << "Installing: " << file << std::endl;
            // In real implementation, you'd actually copy files here
            std::string destPath = installPath + "\\" + file;
            // Simulate file copying
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
        }
        
        // Write registry entries (simulation)
        if (!registryEntries.empty()) {
            std::cout << "Writing registry entries..." << std::endl;
            for (const auto& entry : registryEntries) {
                std::cout << "  " << entry.first << " = " << entry.second << std::endl;
            }
        }
        
        // Create shortcuts
        if (createDesktopShortcut) {
            std::cout << "Creating desktop shortcut..." << std::endl;
        }
        
        if (createStartMenuShortcut) {
            std::cout << "Creating start menu shortcut..." << std::endl;
        }
        
        // Create uninstaller info
        createUninstallInfo();
        
        return true;
    }
    
    void createUninstallInfo() {
        std::string uninstallPath = installPath + "\\uninstall.txt";
        std::ofstream uninstallFile(uninstallPath);
        
        if (uninstallFile.is_open()) {
            uninstallFile << "Uninstall Information for " << appName << std::endl;
            uninstallFile << "Version: " << version << std::endl;
            uninstallFile << "Install Path: " << installPath << std::endl;
            uninstallFile << "Files:" << std::endl;
            
            for (const auto& file : files) {
                uninstallFile << "  " << file << std::endl;
            }
            
            uninstallFile << "Registry Entries:" << std::endl;
            for (const auto& entry : registryEntries) {
                uninstallFile << "  " << entry.first << std::endl;
            }
            
            uninstallFile.close();
            std::cout << "Created uninstall information." << std::endl;
        }
    }
    
    void finish() {
        std::cout << "\n=== Installation Complete ===" << std::endl;
        std::cout << appName << " has been successfully installed to:" << std::endl;
        std::cout << installPath << std::endl;
        std::cout << "\nPress Enter to exit...";
        std::cin.get();
    }
    
    void runWizard() {
        welcome();
        selectInstallPath();
        selectComponents();
        
        std::cout << "\n=== Ready to Install ===" << std::endl;
        std::cout << "Application: " << appName << std::endl;
        std::cout << "Location: " << installPath << std::endl;
        std::cout << "Desktop shortcut: " << (createDesktopShortcut ? "Yes" : "No") << std::endl;
        std::cout << "Start menu shortcut: " << (createStartMenuShortcut ? "Yes" : "No") << std::endl;
        
        std::cout << "\nPress Enter to begin installation...";
        std::cin.get();
        
        if (install()) {
            finish();
        } else {
            std::cout << "Installation failed!" << std::endl;
        }
    }
};

// Example usage
int main() {
    InstallerWizard wizard("MyAwesomeApp", "1.0.0");
    
    // Add some files to install
    wizard.addFile("myapp.exe");
    wizard.addFile("config.ini");
    wizard.addFile("readme.txt");
    
    // Add registry entries
    wizard.addRegistryEntry("HKLM\\Software\\MyAwesomeApp\\InstallPath", "C:\\Program Files\\MyAwesomeApp");
    wizard.addRegistryEntry("HKLM\\Software\\MyAwesomeApp\\Version", "1.0.0");
    
    // Run the wizard
    wizard.runWizard();
    
    return 0;
}