/**
 * Depth-first search implementation for graphs with extensive debugging
 * @param {Object} graph - The graph represented as an adjacency list
 * @param {*} startVertex - Optional starting vertex
 */
function search(graph, startVertex) {
  console.log('🔍 Starting DFS search...');
  console.log('📊 Graph structure:', JSON.stringify(graph, null, 2));
  
  // Create a visited map for all vertices
  const visited = {};
  const visitOrder = [];
  let stepCount = 0;
  
  // Mark all vertices as not visited initially
  console.log('⏱️ Initializing visited status for all vertices...');
  Object.keys(graph).forEach(vertex => {
    visited[vertex] = false;
    console.log(`  - Vertex ${vertex}: not visited`);
  });
  
  // For each vertex in the graph
  console.log('🔄 Starting main traversal loop...');
  Object.keys(graph).forEach(vertex => {
    // If vertex hasn't been visited yet
    if (!visited[vertex]) {
      console.log(`\n🌱 Found unvisited vertex ${vertex}, starting new DFS tree`);
      // Start DFS from this vertex
      dfs(vertex);
    }
  });
  
  console.log('\n📋 Final visit order:', visitOrder.join(' → '));
  console.log(`🏁 DFS completed with ${stepCount} steps`);
  return { visited, visitOrder };
  
  /**
   * Recursive depth-first search function
   * @param {*} vertex - Current vertex being explored
   * @param {number} depth - Current recursion depth for indentation
   */
  function dfs(vertex, depth = 0) {
    stepCount++;
    const indent = '  '.repeat(depth);
    
    // Mark current vertex as visited
    visited[vertex] = true;
    visitOrder.push(vertex);
    console.log(`${indent}🔸 Step ${stepCount}: Visiting vertex ${vertex}`);
    
    // Visit all adjacent vertices
    console.log(`${indent}👀 Checking neighbors of ${vertex}: ${graph[vertex].join(', ')}`);
    
    graph[vertex].forEach(adjacentVertex => {
      // If adjacent vertex hasn't been visited
      if (!visited[adjacentVertex]) {
        console.log(`${indent}  ➡️ Neighbor ${adjacentVertex} not visited yet, exploring...`);
        // Recursively explore it
        dfs(adjacentVertex, depth + 1);
      } else {
        console.log(`${indent}  ⏭️ Neighbor ${adjacentVertex} already visited, skipping`);
      }
    });
    
    console.log(`${indent}✅ Completed exploration of vertex ${vertex}`);
  }
}

// Test cases
console.log('==== TEST CASE 1: Simple Connected Graph ====');
const simpleGraph = {
  'A': ['B', 'C'],
  'B': ['A', 'D', 'E'],
  'C': ['A', 'F'],
  'D': ['B'],
  'E': ['B', 'F'],
  'F': ['C', 'E']
};
search(simpleGraph);

console.log('\n\n==== TEST CASE 2: Disconnected Graph ====');
const disconnectedGraph = {
  'A': ['B'],
  'B': ['A', 'C'],
  'C': ['B'],
  'D': ['E'],
  'E': ['D'],
  'F': []
};
search(disconnectedGraph);

console.log('\n\n==== TEST CASE 3: Directed Graph ====');
const directedGraph = {
  'A': ['B', 'C'],
  'B': ['D'],
  'C': ['B'],
  'D': [],
  'E': ['A']
};
search(directedGraph);

console.log('\n\n==== TEST CASE 4: Single Node Graph ====');
const singleNodeGraph = {
  'A': []
};
search(singleNodeGraph);

console.log('\n\n==== TEST CASE 5: Cycle Graph ====');
const cycleGraph = {
  'A': ['B'],
  'B': ['C'],
  'C': ['D'],
  'D': ['A']
};
search(cycleGraph);