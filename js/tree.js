// Create a binary search tree node class
class Node {
    constructor(value) {
        this.value = value;
        this.left = null;
        this.right = null;
    }
}

// Create a binary search tree class
class BinarySearchTree {
    constructor() {
        this.root = null;
    }
    
    // Insert a value into the BST
    insert(value) {
        const newNode = new Node(value);
        
        if (!this.root) {
            this.root = newNode;
            return this;
        }
        
        let current = this.root;
        
        while (true) {
            // Skip if value already exists
            if (value === current.value) return this;
            
            if (value < current.value) {
                if (current.left === null) {
                    current.left = newNode;
                    return this;
                }
                current = current.left;
            } else {
                if (current.right === null) {
                    current.right = newNode;
                    return this;
                }
                current = current.right;
            }
        }
    }
    
    // Search for a value in the BST and return visited nodes
    search(value) {
        if (!this.root) return [];
        
        let current = this.root;
        let visitedNodes = [];
        
        while (current) {
            visitedNodes.push(current.value);
            
            if (value === current.value) {
                return visitedNodes; // Found the value
            }
            
            if (value < current.value) {
                current = current.left;
            } else {
                current = current.right;
            }
        }
        
        return visitedNodes; // Value not found, but return visited nodes
    }
}

// Create a specific tree structure that matches Option A: 42 - 60 - 20 - 48 - 50
function createTreeStructureA() {
    const bst = new BinarySearchTree();
    bst.root = new Node(42);
    bst.root.right = new Node(60);
    bst.root.left = new Node(20);
    bst.root.left.right = new Node(48);
    bst.root.left.right.right = new Node(50);
    return bst;
}

// Test all the search paths from the options
console.log("Option A:", createTreeStructureA().search(50)); // Should output [42, 20, 48, 50]

// You can implement the other options similarly
// For example, to test option C (40 - 60 - 45 - 48 - 50):
function createTreeStructureC() {
    const bst = new BinarySearchTree();
    bst.root = new Node(40);
    bst.root.right = new Node(60);
    bst.root.right.left = new Node(45);
    bst.root.right.left.right = new Node(48);
    bst.root.right.left.right.right = new Node(50);
    return bst;
}

console.log("Option C:", createTreeStructureC().search(50)); // Should output [40, 60, 45, 48, 50]