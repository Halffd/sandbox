function inOrderTraversalNonRecursive(root) {
    if (!root) {
        console.log("🌱 Tree is empty. Nothing to traverse!");
        return;
    }
    var stack = [];
    var traversalResult = [];
    console.log("🚀 Starting in-order traversal");
    // Push root node with moment 1
    stack.push({ node: root, moment: 1 });
    console.log("\uD83D\uDCE5 PUSH: Node ".concat(root.value, ", Moment 1"));
    while (stack.length > 0) {
        console.log("\n\uD83D\uDCCA STACK: ".concat(stack.map(function (item) { return "(".concat(item.node.value, ",").concat(item.moment, ")"); }).join(', ')));
        var _a = stack.pop(), node = _a.node, moment = _a.moment;
        console.log("\uD83D\uDCE4 POP: Node ".concat(node.value, ", Moment ").concat(moment));
        if (moment === 1) {
            // Push the same node with moment 2
            stack.push({ node: node, moment: 2 });
            console.log("\uD83D\uDCE5 PUSH: Node ".concat(node.value, ", Moment 2"));
            // If there's a left child, push it with moment 1
            if (node.left) {
                stack.push({ node: node.left, moment: 1 });
                console.log("\uD83D\uDCE5 PUSH: Node ".concat(node.left.value, ", Moment 1"));
            }
            else {
                console.log("\uD83C\uDF43 No left child for node ".concat(node.value));
            }
        }
        else if (moment === 2) {
            // Visit the node in moment 2 (in-order traversal)
            traversalResult.push(node.value);
            console.log("\uD83C\uDFAF VISIT: Node ".concat(node.value));
            // If there's a right child, push it with moment 1
            if (node.right) {
                stack.push({ node: node.right, moment: 1 });
                console.log("\uD83D\uDCE5 PUSH: Node ".concat(node.right.value, ", Moment 1"));
            }
            else {
                console.log("\uD83C\uDF43 No right child for node ".concat(node.value));
            }
        }
        // No action for moment 3 in in-order traversal
    }
    console.log("\n\u2705 In-order traversal complete!");
    console.log("\uD83D\uDD22 Result: ".concat(traversalResult.join(', ')));
}
// Example usage
var tree = {
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
