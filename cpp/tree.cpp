#include <iostream>
#include <stack>
#include <queue>
#include <vector>
#include <algorithm>
#include <iomanip>
#include <string>
#include <utility>
#include <memory> // for smart pointers, but we'll stick with raw pointers for tree connections
#include <math.h>

class BinarySearchTree {
protected:
    struct Node {
        int key =  0;
        Node* left = nullptr;
        Node* right = nullptr;
        int count = 0; // To track duplicates
        unsigned int height = 1;
		Node(int value) : key(value), left(nullptr), right(nullptr), count(1) {}
    };
   Node* root;
 private: 
    // Helper search function that returns the node and its parent
    Node* search(int key, Node* root, Node*& parent, bool& found) {
        if (root == nullptr) {
            found = false;
            return nullptr;
        }
        else if (root->key == key) {
            found = true;
            return root;
        }
        else if (key < root->key) {
            if (root->left != nullptr) {
                parent = root;
                return search(key, root->left, parent, found);
            }
            else {
                found = false;
                return root;
            }
        }
        else {
            if (root->right != nullptr) {
                parent = root;
                return search(key, root->right, parent, found);
            }
            else {
                found = false;
                return root;
            }
        }
    }
    
    // Helper function to delete the tree recursively
    void deleteTree(Node* node) {
        if (node == nullptr) return;
        
        // Post-order deletion (delete children before parent)
        deleteTree(node->left);
        deleteTree(node->right);
        delete node;
    }
    
    // Helper function to collect all keys in the tree
    void collectKeys(Node* node, std::vector<int>& keys) {
        if (node == nullptr) return;
        
        // In-order traversal to collect keys
        collectKeys(node->left, keys);
        
        // Add the key count times for duplicates
        for (int i = 0; i < node->count; i++) {
            keys.push_back(node->key);
        }
        
        collectKeys(node->right, keys);
    }
    
    // Helper function to build a balanced tree from sorted keys
    Node* buildBalancedTree(const std::vector<int>& keys, int start, int end) {
        if (start > end) return nullptr;
        
        int mid = start + (end - start) / 2;
        Node* node = new Node(keys[mid]);
        
        node->left = buildBalancedTree(keys, start, mid - 1);
        node->right = buildBalancedTree(keys, mid + 1, end);
        
        return node;
    }
    
    // Helper for tree visualization
    void printTree(const std::string& prefix, const Node* node, bool isLeft) {
        if (node == nullptr) return;
        
        std::cout << prefix;
        std::cout << (isLeft ? "├──" : "└──");
        
        // Print the key and count if > 1
        std::cout << node->key;
        if (node->count > 1) {
            std::cout << " (x" << node->count << ")";
        }
        std::cout << std::endl;
        
        // Enter the next tree level - left and right branch
        printTree(prefix + (isLeft ? "│   " : "    "), node->left, true);
        printTree(prefix + (isLeft ? "│   " : "    "), node->right, false);
    }

public:
    BinarySearchTree() : root(nullptr) {}
    
    // Proper destructor that cleans up all allocated memory
    ~BinarySearchTree() {
        deleteTree(root);
        root = nullptr; // Not necessary, but good practice
    }
    
    // Insert with option to allow duplicates
    bool insert(int key, bool allowDuplicates = false) {
        Node* parent = nullptr;
        bool found = false;
        
        Node* aux = search(key, root, parent, found);
        
        if (found) {
            if (allowDuplicates) {
                aux->count++; // Increment count for duplicate
                return true;
            }
            return false; // Key already exists and duplicates not allowed
        }
        
        Node* newNode = new Node(key);
        
        if (aux == nullptr) {
            root = newNode;
        }
        else if (key < aux->key) {
            aux->left = newNode;
        }
        else {
            aux->right = newNode;
        }
        return true;
    }
    
    // Replace a node with a new key
    bool replaceNode(int oldKey, int newKey) {
        // First check if oldKey exists and newKey doesn't
        Node* parent = nullptr;
        bool found = false;
        
        Node* nodeToReplace = search(oldKey, root, parent, found);
        if (!found) {
            return false; // Old key not found
        }
        
        // Store the count
        int count = nodeToReplace->count;
        
        // Remove the old node (just one instance if duplicates)
        remove(oldKey);
        
        // Insert the new node with the same count
        bool inserted = insert(newKey);
        if (inserted) {
            Node* parent = nullptr;
            bool found = false;
            Node* newNode = search(newKey, root, parent, found);
            if (found) {
                newNode->count = count;
            }
        }
        
        return inserted;
    }
    
    bool remove(int key, bool exact = true) {
        Node* parent = nullptr;
        bool found = false;
        
        Node* aux = search(key, root, parent, found);
        
        if (aux == nullptr) {
            return false; // Empty tree
        }
        else if (!found && exact) {
            return false; // Key not found and exact match required
        }
        
        // If there are multiple instances, just decrement the count
        if (aux->count > 1) {
            aux->count--;
            return true;
        }
        
        // Case 1: Node is a leaf (no children)
        if (aux->left == nullptr && aux->right == nullptr) {
            if (parent == nullptr) {
                delete root;
                root = nullptr; // Tree becomes empty
            }
            else {
                if (parent->right == aux) {
                    parent->right = nullptr;
                }
                else {
                    parent->left = nullptr;
                }
                delete aux;
            }
        }
        // Case 2: Node has only one child
        else if (aux->left == nullptr || aux->right == nullptr) {
            Node* child = (aux->left != nullptr) ? aux->left : aux->right;
            
            if (parent == nullptr) {
                root = child;
            }
            else {
                if (parent->right == aux) {
                    parent->right = child;
                }
                else {
                    parent->left = child;
                }
            }
            delete aux;
        }
        // Case 3: Node has two children
        else {
            // Find the inorder successor (smallest node in right subtree)
            Node* successor_parent = aux;
            Node* successor = aux->right;
            
            while (successor->left != nullptr) {
                successor_parent = successor;
                successor = successor->left;
            }
            
            // Save successor's key and count
            int successorKey = successor->key;
            int successorCount = successor->count;
            
            // Remove the successor (it has at most one child)
            if (successor_parent == aux) {
                successor_parent->right = successor->right;
            }
            else {
                successor_parent->left = successor->right;
            }
            
            // Replace aux's data with successor's data
            aux->key = successorKey;
            aux->count = successorCount;
            
            delete successor;
        }
        return true;
    }
    
    // Balance the tree (sort and rebuild)
    void balance() {
        std::vector<int> keys;
        collectKeys(root, keys);
        
        // Remove duplicates for building the structure
        std::vector<int> uniqueKeys;
        for (size_t i = 0; i < keys.size(); i++) {
            if (i == 0 || keys[i] != keys[i-1]) {
                uniqueKeys.push_back(keys[i]);
            }
        }
        
        // Clear the current tree
        deleteTree(root);
        root = nullptr;
        
        // Build a balanced tree
        if (!uniqueKeys.empty()) {
            root = buildBalancedTree(uniqueKeys, 0, uniqueKeys.size() - 1);
            
            // Restore counts for duplicate keys
            for (int key : keys) {
                Node* parent = nullptr;
                bool found = false;
                Node* node = search(key, root, parent, found);
                if (found && node->count == 1) {
                    node->count++;
                }
            }
        }
    }
void rightRotate(Node*& node) {
    if (!node || !node->left) return;
    
    Node* leftChild = node->left;
    node->left = leftChild->right;
    leftChild->right = node;
    node = leftChild;
}

void leftRotate(Node*& node) {
    if (!node || !node->right) return;
    
    Node* rightChild = node->right;
    node->right = rightChild->left;
    rightChild->left = node;
    node = rightChild;
}

// Step 1 of DSW: Create the backbone (a right-leaning vine)
void createBackbone() {
    if (!root) return;
    
    Node* current = root;
    Node* prev = nullptr;
    
    // Step 1: Flatten the tree into a right-leaning vine
    while (current) {
        if (current->left) {
            // Right rotate at current
            Node* left = current->left;
            current->left = left->right;
            left->right = current;
            
            if (prev) {
                prev->right = left;
            } else {
                root = left;
            }
            current = left;
        } else {
            // Move to the right child
            prev = current;
            current = current->right;
        }
    }
}

// Fix for balanceBackbone() - we're doing LEFT rotations here, not right!
void balanceBackbone() {
    // Count nodes in the backbone
    int n = 0;
    Node* current = root;
    while (current) {
        n++;
        current = current->right;
    }
    
    // Calculate number of leaves in the perfect tree portion
    int m = (1 << (int)log2(n + 1)) - 1;
    
    // First rotation pass - compress the backbone
    int rotations = n - m;
    
    // Fix: use a pointer instead of trying to create a Node with no constructor
    Node* fake_root = new Node(0); // Temporary node with dummy value
    fake_root->right = root;
    Node* scanner = fake_root;
    
    for (int i = 0; i < rotations; i++) {
        // LEFT rotation
        Node* child = scanner->right;
        scanner->right = child->right;
        child->right = scanner->right->left;
        scanner->right->left = child;
        scanner = scanner->right;
    }
    
    root = fake_root->right;
    
    // Complete remaining passes
    while (m > 1) {
        m = m / 2;
        // Reset for next pass
        fake_root->right = root;
        scanner = fake_root;
        
        for (int i = 0; i < m; i++) {
            // LEFT rotation again
            Node* child = scanner->right;
            scanner->right = child->right;
            child->right = scanner->right->left;
            scanner->right->left = child;
            scanner = scanner->right;
        }
        
        root = fake_root->right;
    }
    
    // Clean up our temporary node
    delete fake_root;
}
// DSW balancing method
void balanceDSW() {
    if (!root) return;
    
    // Clear out any of the existing structure
    createBackbone();
    balanceBackbone();
    
    // Verify the tree is still a valid BST
    // This is just for debugging - can be removed in production
    bool isValid = true;
    validateBST(root, isValid);
    if (!isValid) {
        std::cout << "WARNING: Tree is not a valid BST after DSW balancing!" << std::endl;
    }
}

// Helper method to validate BST property (for debugging)
void validateBST(Node* node, bool& isValid, int* minVal = nullptr, int* maxVal = nullptr) {
    if (!node) return;
    
    // Check current node's constraints
    if (minVal && node->key <= *minVal) isValid = false;
    if (maxVal && node->key >= *maxVal) isValid = false;
    
    // Recursively check children with updated constraints
    int currentKey = node->key;
    validateBST(node->left, isValid, minVal, &currentKey);
    validateBST(node->right, isValid, &currentKey, maxVal);
}
    // Print the tree in ASCII art format
    void printTree() {
        if (root == nullptr) {
            std::cout << "Empty tree" << std::endl;
            return;
        }
        printTree("", root, false);
    }
    
    void postOrderTraversal() {
        if (root == nullptr) return;
        
        std::stack<std::pair<Node*, int>> stack;
        stack.push({root, 1}); // Insert root at moment 1
        
        while (!stack.empty()) {
            auto [node, moment] = stack.top();
            stack.pop();
            
            if (moment == 1) {
                stack.push({node, 2}); // Push moment 2
                if (node->left != nullptr) {
                    stack.push({node->left, 1});
                }
            }
            else if (moment == 2) {
                stack.push({node, 3}); // Push moment 3
                if (node->right != nullptr) {
                    stack.push({node->right, 1});
                }
            }
            else if (moment == 3) {
                // Visit node (print its value and count)
                std::cout << node->key;
                if (node->count > 1) {
                    std::cout << " (x" << node->count << ")";
                }
                std::cout << " ";
            }
        }
        std::cout << std::endl;
    }
    
    void inOrderTraversal() {
        inOrderHelper(root);
        std::cout << std::endl;
    }
    
    void preOrderTraversal() {
        preOrderHelper(root);
        std::cout << std::endl;
    }
    
    // Check if a key exists
    bool contains(int key) {
        Node* parent = nullptr;
        bool found = false;
        search(key, root, parent, found);
        return found;
    }
    
    // Get the count of occurrences of a key
    int getCount(int key) {
        Node* parent = nullptr;
        bool found = false;
        Node* node = search(key, root, parent, found);
        return found ? node->count : 0;
    }
    
private:
    void inOrderHelper(Node* node) {
        if (node == nullptr) return;
        inOrderHelper(node->left);
        
        std::cout << node->key;
        if (node->count > 1) {
            std::cout << " (x" << node->count << ")";
        }
        std::cout << " ";
        
        inOrderHelper(node->right);
    }
    
    void preOrderHelper(Node* node) {
        if (node == nullptr) return;
        
        std::cout << node->key;
        if (node->count > 1) {
            std::cout << " (x" << node->count << ")";
        }
        std::cout << " ";
        
        preOrderHelper(node->left);
        preOrderHelper(node->right);
    }
};
// AVL Tree class that inherits from BinarySearchTree
class AVLTree : public BinarySearchTree {
private:
    // Get height of a node (handles nullptr)
    int getHeight(Node* node) {
        return node ? node->height : 0;
    }
    
    // Calculate balance factor of a node
    int getBalanceFactor(Node* node) {
        if (!node) return 0;
        return getHeight(node->left) - getHeight(node->right);
    }
    
    // Update height of a node based on children
    void updateHeight(Node* node) {
        if (!node) return;
        node->height = 1 + std::max(getHeight(node->left), getHeight(node->right));
        std::cout << "Updated height: " << (node ? std::to_string(node->height) : "null") << "\n";
    }
    
    // Rebalance a node if needed and update height
    Node* rebalance(Node* node) {
        if (!node) return nullptr;
        
        // Update height of current node
        updateHeight(node);
        
        // Get balance factor to check if node is unbalanced
        int balance = getBalanceFactor(node);
        if (balance)
    		std::cout << "Balance: " << std::to_string(balance) << "\n";
        // Left Heavy (Case 1 and 2)
        if (balance > 1) {
            // Case 2b: Left-Right case
            if (getBalanceFactor(node->left) < 0) {
                // Double rotation needed - first left rotate the left child
                node->left = leftRotateSingle(node->left);
            }
            // Case 2a: Left-Left case (or continuation of 2b)
            return rightRotateSingle(node);
        }
        
        // Right Heavy (Case 1 and 2)
        if (balance < -1) {
            // Case 1b: Right-Left case
            if (getBalanceFactor(node->right) > 0) {
                // Double rotation needed - first right rotate the right child
                node->right = rightRotateSingle(node->right);
            }
            // Case 1a: Right-Right case (or continuation of 1b)
            return leftRotateSingle(node);
        }
        
        // Node is balanced
        return node;
    }
    
    // Single right rotation - enhanced for AVL to update heights
    Node* rightRotateSingle(Node* y) {
        if (!y || !y->left) return y;
        
        Node* x = y->left;
        Node* T2 = x->right;
        // Debug print with null checks
        std::cout << "Right Rotate: y=" << y->key
                  << " x=" << (x ? std::to_string(x->key) : "null")
                  << " T2=" << (T2 ? std::to_string(T2->key) : "null") << std::endl;
        // Perform rotation
        x->right = y;
        y->left = T2;
        // Debug print with null checks
        std::cout << "After Rotate: y=" << y->key
                  << " x=" << (x ? std::to_string(x->key) : "null")
                  << " T2=" << (T2 ? std::to_string(T2->key) : "null") << std::endl;
        // Update heights
        updateHeight(y);
        updateHeight(x);
        
        return x;
    }
    
    // Single left rotation - enhanced for AVL to update heights
    Node* leftRotateSingle(Node* x) {
        if (!x || !x->right) return x;
        
        Node* y = x->right;
        Node* T2 = y->left;
        // Debug print with null checks
        std::cout << "Left Rotate: y=" << y->key
                  << " x=" << (x ? std::to_string(x->key) : "null")
                  << " T2=" << (T2 ? std::to_string(T2->key) : "null") << std::endl;
        // Perform rotation
        y->left = x;
        x->right = T2;
        // Debug print with null checks
        std::cout << "After Rotate: y=" << y->key
                  << " x=" << (x ? std::to_string(x->key) : "null")
                  << " T2=" << (T2 ? std::to_string(T2->key) : "null") << std::endl;
        
        // Update heights
        updateHeight(x);
        updateHeight(y);
        
        return y;
    }
    
    // Recursive insert that maintains AVL property
    Node* insertNode(Node* node, int key, bool allowDuplicates, bool& success) {
        // Standard BST insert
        if (node == nullptr) {
            success = true;
            return new Node(key);
        }
        std::cout << "Insert Node: " << node->key << " Key: " << key << "\n";

        if (key < node->key) {
            node->left = insertNode(node->left, key, allowDuplicates, success);
        } 
        else if (key > node->key) {
            node->right = insertNode(node->right, key, allowDuplicates, success);
        } 
        else {
            // Duplicate value
            if (allowDuplicates) {
                node->count++;
                success = true;
            } else {
                success = false;
            }
            return node;
        }
        
        // Update height and rebalance
        return rebalance(node);
    }
    
    // Find the node with minimum value in a subtree
    Node* findMin(Node* node) {
        if (!node) return nullptr;
        while (node->left) {
            node = node->left;
        }
        std::cout << "Found Min Node: " << node->key << "\n";
        return node;
    }
    
    // Recursive delete that maintains AVL property
    Node* deleteNode(Node* node, int key, bool& success) {
        if (!node) {
            success = false;
            return nullptr;
        }
        std::cout << "Delete Node: " << node->key << " Key: " << key << "\n";

        // Standard BST delete
        if (key < node->key) {
            node->left = deleteNode(node->left, key, success);
        }
        else if (key > node->key) {
            node->right = deleteNode(node->right, key, success);
        }
        else {
            // Found the node to delete
            success = true;
            
            // If there are multiple instances, just decrement the count
            if (node->count > 1) {
                node->count--;
                return node;
            }
            
            // Node with one child or no child
            if (!node->left || !node->right) {
                Node* temp = node->left ? node->left : node->right;
                
                // No child case
                if (!temp) {
                    temp = node;
                    node = nullptr;
                }
                // One child case
                else {
                    // Copy contents of non-empty child
                    *node = *temp;
                }
                
                delete temp;
            }
            // Node with two children
            else {
                // Get inorder successor (smallest in right subtree)
                Node* successor = findMin(node->right);
                
                // Copy successor data to this node
                node->key = successor->key;
                node->count = successor->count;
                
                // Reset successor count to 1 to ensure it gets deleted
                successor->count = 1;
                
                // Delete the successor
                node->right = deleteNode(node->right, successor->key, success);
            }
        }
        
        // If tree had only one node, return
        if (!node) return nullptr;
        
        // Update height and rebalance
        return rebalance(node);
    }

public:
    AVLTree() : BinarySearchTree() {}
    
    // Override insert to maintain AVL property
    bool insert(int key, bool allowDuplicates = false) {
        bool success = false;
        root = insertNode(root, key, allowDuplicates, success);
        return success;
    }
    
    // Override remove to maintain AVL property
    bool remove(int key, bool exact = true) {
        bool success = false;
        root = deleteNode(root, key, success);
        return success;
    }
    
    // Method to check if tree is AVL balanced (for testing)
    bool isBalanced() {
        return checkBalance(root);
    }
    
private:
    // Helper to check if tree is balanced
    bool checkBalance(Node* node) {
        if (!node) return true;
        
        int balanceFactor = getBalanceFactor(node);
        if (balanceFactor > 1 || balanceFactor < -1) {
            return false;
        }
        
        return checkBalance(node->left) && checkBalance(node->right);
    }
};

// Test function
void testAVLTree() {
    AVLTree avl;
    
    std::cout << "--- Testing AVL Tree ---\n";
    
    // Insert some values that would cause imbalance in regular BST
    std::cout << "Inserting values: 10, 20, 30, 40, 50, 25\n";
    avl.insert(10);
    avl.insert(20);
    avl.insert(30);
    avl.insert(40);
    avl.insert(50);
    avl.insert(25);
    
    // Print the tree structure
    std::cout << "\nAVL Tree structure after insertions:\n";
    avl.printTree();
    
    // Verify it's balanced
    std::cout << "\nIs tree balanced? " << (avl.isBalanced() ? "Yes" : "No") << "\n";
    
    // Test traversals
    std::cout << "\nIn-order traversal: ";
    avl.inOrderTraversal();
    
    std::cout << "Pre-order traversal: ";
    avl.preOrderTraversal();
    
    std::cout << "Post-order traversal: ";
    avl.postOrderTraversal();
    
    // Test deletion
    std::cout << "\nRemoving value 30\n";
    avl.remove(30);
    
    std::cout << "AVL Tree structure after removal:\n";
    avl.printTree();
    
    std::cout << "Is tree still balanced? " << (avl.isBalanced() ? "Yes" : "No") << "\n";
    
    // Test more complex scenario - create a tree that requires multiple rotations
    std::cout << "\n--- Testing more complex AVL scenario ---\n";
    AVLTree avl2;
    
    // Insert values in a way that triggers different rotation types
    std::cout << "Inserting values: 9, 5, 10, 0, 6, 11, -1, 1, 2\n";
    avl2.insert(9);
    avl2.insert(5);
    avl2.insert(10);
    avl2.insert(0);
    avl2.insert(6);
    avl2.insert(11);
    avl2.insert(-1);
    avl2.insert(1);
    avl2.insert(2);
    
    std::cout << "\nComplex AVL Tree structure:\n";
    avl2.printTree();
    
    std::cout << "Is complex tree balanced? " << (avl2.isBalanced() ? "Yes" : "No") << "\n";
    
    // Test removing a node that causes rebalancing
    std::cout << "\nRemoving values that trigger rebalancing: 10, 11\n";
    avl2.remove(10);
    avl2.remove(11);
    
    std::cout << "Tree after complex removals:\n";
    avl2.printTree();
    
    std::cout << "Is tree still balanced? " << (avl2.isBalanced() ? "Yes" : "No") << "\n";
}
// Example usage with comprehensive tests
int main() {
    BinarySearchTree bst;
    
    std::cout << "=== TESTING BASIC INSERTION ===" << std::endl;
    bst.insert(10);
    bst.insert(5);
    bst.insert(15);
    bst.insert(3);
    bst.insert(7);
    bst.insert(12);
    bst.insert(18);
    
    std::cout << "Initial tree:" << std::endl;
    bst.printTree();
    
    std::cout << "\n=== TESTING TRAVERSALS ===" << std::endl;
    std::cout << "In-order traversal: ";
    bst.inOrderTraversal();  // Should print: 3 5 7 10 12 15 18
    
    std::cout << "Pre-order traversal: ";
    bst.preOrderTraversal(); // Should print: 10 5 3 7 15 12 18
    
    std::cout << "Post-order traversal: ";
    bst.postOrderTraversal(); // Should print: 3 7 5 12 18 15 10
    
    std::cout << "\n=== TESTING DUPLICATE INSERTION ===" << std::endl;
    // Try to insert duplicate without allowing it
    bool inserted = bst.insert(7);
    std::cout << "Inserted duplicate 7 (not allowed): " << (inserted ? "Yes" : "No") << std::endl;
    
    // Insert duplicate with allowDuplicates=true
    inserted = bst.insert(7, true);
    std::cout << "Inserted duplicate 7 (allowed): " << (inserted ? "Yes" : "No") << std::endl;
    
    bst.insert(7, true); // Add one more 7
    
    std::cout << "Tree after adding duplicates:" << std::endl;
    bst.printTree();
    
    std::cout << "In-order traversal with duplicates: ";
    bst.inOrderTraversal();
    
    std::cout << "\n=== TESTING REMOVAL ===" << std::endl;
    // Remove one instance of 7 (should decrement count, not remove node)
    bst.remove(7);
    std::cout << "After removing one instance of 7:" << std::endl;
    bst.printTree();
    
    // Remove another instance of 7 (should decrement count again)
    bst.remove(7);
    std::cout << "After removing second instance of 7:" << std::endl;
    bst.printTree();
    
    // Remove last instance of 7 (should remove node)
    bst.remove(7);
    std::cout << "After removing last instance of 7:" << std::endl;
    bst.printTree();
    
    std::cout << "\n=== TESTING NODE REPLACEMENT ===" << std::endl;
    // Replace 5 with 6
    bool replaced = bst.replaceNode(5, 6);
    std::cout << "Replaced 5 with 6: " << (replaced ? "Yes" : "No") << std::endl;
    std::cout << "Tree after replacement:" << std::endl;
    bst.printTree();
    
    std::cout << "\n=== TESTING BALANCING ===" << std::endl;
    // Create an unbalanced tree
    BinarySearchTree unbalancedBST;
    unbalancedBST.insert(1);
    std::cout << "\n=== TESTING BALANCING ===" << std::endl;
	{
    // Create an unbalanced tree
    BinarySearchTree unbalancedBST;
    unbalancedBST.insert(1);
    unbalancedBST.insert(2);
    unbalancedBST.insert(3);
    unbalancedBST.insert(4);
    unbalancedBST.insert(5);
    unbalancedBST.insert(6);
    unbalancedBST.insert(7);
    
    std::cout << "Unbalanced tree (linear):" << std::endl;
    unbalancedBST.printTree();
    
    // Balance the tree
    unbalancedBST.balance();
    
    std::cout << "After balancing:" << std::endl;
    unbalancedBST.printTree();
    }
    std::cout << "\n=== TESTING COMPLEX REMOVAL ===" << std::endl;
    // Let's go back to our original tree and test removal of nodes with two children
    std::cout << "Current tree:" << std::endl;
    bst.printTree();
    
    // Remove a node with two children (15)
    bst.remove(15);
    std::cout << "After removing 15 (node with two children):" << std::endl;
    bst.printTree();
    
    // Remove root node
    bst.remove(10);
    std::cout << "After removing root node (10):" << std::endl;
    bst.printTree();
    
    std::cout << "\n=== TESTING DUPLICATE COUNTING ===" << std::endl;
    // Create a new tree with duplicates
    BinarySearchTree countBST;
    countBST.insert(10);
    countBST.insert(5, true);
    countBST.insert(5, true); // Add 5 twice more
    countBST.insert(5, true);
    countBST.insert(15);
    
    std::cout << "Tree with duplicates:" << std::endl;
    countBST.printTree();
    std::cout << "Count of 5: " << countBST.getCount(5) << std::endl;
    std::cout << "Count of 10: " << countBST.getCount(10) << std::endl;
    std::cout << "Count of 99 (not in tree): " << countBST.getCount(99) << std::endl;
    
    std::cout << "\n=== TESTING CONTAINS METHOD ===" << std::endl;
    std::cout << "Contains 5? " << (countBST.contains(5) ? "Yes" : "No") << std::endl;
    std::cout << "Contains 99? " << (countBST.contains(99) ? "Yes" : "No") << std::endl;
    
    std::cout << "\n=== TESTING BALANCING WITH DUPLICATES ===" << std::endl;
    countBST.balance();
    std::cout << "Balanced tree with preserved duplicates:" << std::endl;
    countBST.printTree();
    std::cout << "Count of 5 after balancing: " << countBST.getCount(5) << std::endl;
    
    std::cout << "\n=== STRESS TEST ===" << std::endl;
    // Create a larger tree with random values
    BinarySearchTree stressBST;
    // Seed the random number generator for reproducible results
    srand(42);
    
    // Insert 20 random numbers between 1 and 100
    std::cout << "Inserting 20 random values..." << std::endl;
    for (int i = 0; i < 20; i++) {
        int randomValue = rand() % 100 + 1;
        stressBST.insert(randomValue, true); // Allow duplicates
    }
    
    std::cout << "Random tree before balancing:" << std::endl;
    stressBST.printTree();
    
    // Balance the tree
    stressBST.balance();
    
    std::cout << "Random tree after balancing:" << std::endl;
    stressBST.printTree();
    
    std::cout << "\n=== TESTING EDGE CASES ===" << std::endl;
    
    // Create an empty tree
    BinarySearchTree emptyBST;
    std::cout << "Empty tree:" << std::endl;
    emptyBST.printTree();
    
    // Try to remove from empty tree
    bool removed = emptyBST.remove(10);
    std::cout << "Removed from empty tree: " << (removed ? "Yes" : "No") << std::endl;
    
    // Insert a single node and remove it
    emptyBST.insert(42);
    std::cout << "Tree with one node:" << std::endl;
    emptyBST.printTree();
    
    // Remove the only node
    emptyBST.remove(42);
    std::cout << "After removing the only node:" << std::endl;
    emptyBST.printTree();
    
    // Test balancing an empty tree
    emptyBST.balance();
    std::cout << "After balancing an empty tree:" << std::endl;
    emptyBST.printTree();
    
    std::cout << "\n=== TESTING REPLACEMENT EDGE CASES ===" << std::endl;
    // Replace a node that doesn't exist
    replaced = bst.replaceNode(99, 100);
    std::cout << "Replaced non-existent node: " << (replaced ? "Yes" : "No") << std::endl;
    
    // Replace with an existing value
    BinarySearchTree replaceBST;
    replaceBST.insert(10);
    replaceBST.insert(5);
    replaceBST.insert(15);
    
    replaced = replaceBST.replaceNode(5, 15); // 15 already exists
    std::cout << "Replace with existing value:" << std::endl;
    replaceBST.printTree();
    
    // Replace with duplicates
    BinarySearchTree dupReplaceBST;
    dupReplaceBST.insert(10);
    dupReplaceBST.insert(5, true);
    dupReplaceBST.insert(5, true); // 5 appears 3 times
    dupReplaceBST.insert(5, true);
    
    std::cout << "Before replacing duplicates:" << std::endl;
    dupReplaceBST.printTree();
    
    replaced = dupReplaceBST.replaceNode(5, 7);
    std::cout << "After replacing 5 with 7:" << std::endl;
    dupReplaceBST.printTree();
    
	std::cout << "\n=== TESTING DSW BALANCING ALGORITHM ===" << std::endl;
	BinarySearchTree dswTree;

// Create an unbalanced right-leaning tree
	dswTree.insert(1);
	dswTree.insert(2);
	dswTree.insert(3);
	dswTree.insert(4);
	dswTree.insert(5);
	dswTree.insert(6);
	dswTree.insert(7);

	std::cout << "Unbalanced tree before DSW:" << std::endl;
	dswTree.printTree();

// Balance using DSW
	dswTree.balanceDSW();

	std::cout << "After DSW balancing:" << std::endl;
	dswTree.printTree();

// Try with a more complex tree
	BinarySearchTree dswTree2;
	for (int i = 0; i < 15; i++) {
			dswTree2.insert(rand() % 100);
	}

	std::cout << "Random tree before DSW:" << std::endl;
	dswTree2.printTree();

// Balance using DSW
	dswTree2.balanceDSW();

	std::cout << "After DSW balancing:" << std::endl;
	dswTree2.printTree();
    std::cout << "\n=== ALL TESTS COMPLETE ===" << std::endl;
    
	testAVLTree();
    return 0;
}
