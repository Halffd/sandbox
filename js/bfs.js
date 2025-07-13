/**
 * Breadth-First Search with detailed emoji logging
 * @param {Object} graph - Adjacency list representing the graph
 * @param {string} startNode - The node to start the search from
 * @returns {Array} - The order of nodes visited
 */
function bfsWithEmojiLogging(graph, startNode) {
  // Initialize the queue with the start node
  const queue = [startNode];
  // Keep track of visited nodes
  const visited = new Set([startNode]);
  // Track visit order
  const visitOrder = [];
  
  console.log(`🚀 Starting BFS from node ${startNode}`);
  console.log(`📋 Initial queue: [${queue}]`);
  console.log(`🔍 Initial visited: ${Array.from(visited)}\n`);
  
  let step = 1;
  
  // Continue until the queue is empty
  while (queue.length > 0) {
    // Dequeue the first node
    const currentNode = queue.shift();
    visitOrder.push(currentNode);
    
    console.log(`🔄 Step ${step}:`);
    console.log(`  ⬅️ Dequeued: ${currentNode}`);
    console.log(`  📝 Current visit order: [${visitOrder}]`);
    
    // Check if the current node has neighbors
    const neighbors = graph[currentNode] || [];
    console.log(`  👥 Examining neighbors of ${currentNode}: [${neighbors}]`);
    
    // Process all neighbors
    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        // Mark as visited and enqueue
        visited.add(neighbor);
        queue.push(neighbor);
        console.log(`    ✅ Added ${neighbor} to queue (new discovery! 🎉)`);
      } else {
        console.log(`    ❌ Skipped ${neighbor} (already visited 🔄)`);
      }
    }
    
    console.log(`  📋 Updated queue: [${queue}]`);
    console.log(`  🔍 Updated visited: [${Array.from(visited)}]\n`);
    
    step++;
  }
  
  console.log(`🏁 BFS complete! Final visit order: [${visitOrder}]`);
  return visitOrder;
}

// Example usage
const graph = {
  'A': ['B', 'C'],
  'B': ['A', 'D', 'E'],
  'C': ['A', 'F'],
  'D': ['B'],
  'E': ['B', 'F'],
  'F': ['C', 'E']
};

console.log('🌐 Graph structure:');
for (const [node, neighbors] of Object.entries(graph)) {
  console.log(`  📍 ${node} → ${neighbors.join(', ')}`);
}
console.log();

const result = bfsWithEmojiLogging(graph, 'A');
console.log(`\n🎯 Result: [${result}]`);
