interface TreeNode {
  value: number;
  left: TreeNode | null;
  right: TreeNode | null;
}

interface StackItem {
  node: TreeNode;
  moment: number;
}

function inOrderTraversalNonRecursive(root: TreeNode | null): void {
  if (!root) {
    console.log("🌱 Tree is empty. Nothing to traverse!");
    return;
  }
  
  const stack: StackItem[] = [];
  let traversalResult: number[] = [];
  
  console.log("🚀 Starting in-order traversal");
  // Push root node with moment 1
  stack.push({ node: root, moment: 1 });
  console.log(`📥 PUSH: Node ${root.value}, Moment 1`);
  
  while (stack.length > 0) {
    console.log(`\n📊 STACK: ${stack.map(item => `(${item.node.value},${item.moment})`).join(', ')}`);
    
    const { node, moment } = stack.pop()!;
    console.log(`📤 POP: Node ${node.value}, Moment ${moment}`);
    
    if (moment === 1) {
      // Push the same node with moment 2
      stack.push({ node, moment: 2 });
      console.log(`📥 PUSH: Node ${node.value}, Moment 2`);
      
      // If there's a left child, push it with moment 1
      if (node.left) {
        stack.push({ node: node.left, moment: 1 });
        console.log(`📥 PUSH: Node ${node.left.value}, Moment 1`);
      } else {
        console.log(`🍃 No left child for node ${node.value}`);
      }
    } else if (moment === 2) {
      // Visit the node in moment 2 (in-order traversal)
      traversalResult.push(node.value);
      console.log(`🎯 VISIT: Node ${node.value}`);
      
      // If there's a right child, push it with moment 1
      if (node.right) {
        stack.push({ node: node.right, moment: 1 });
        console.log(`📥 PUSH: Node ${node.right.value}, Moment 1`);
      } else {
        console.log(`🍃 No right child for node ${node.value}`);
      }
    }
    // No action for moment 3 in in-order traversal
  }
  
  console.log(`\n✅ In-order traversal complete!`);
  console.log(`🔢 Result: ${traversalResult.join(', ')}`);
}

// Example usage
const tree: TreeNode = {
  value: 10,
  left: {
    value: 8,
    left: null,
    right: {
      value: 12,
      left: null,
      right: null
    }
  },
  right: {
    value: 7,
    left: null,
    right: null
  }
};

inOrderTraversalNonRecursive(tree);