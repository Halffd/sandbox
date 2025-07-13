import heapq
import collections
from typing import List

class Solution:
    def networkDelayTime(self, times: List[List[int]], n: int, k: int) -> int:
        """
        Calculates the time required for all nodes to receive a signal sent from node k.
        
        Implements Dijkstra's algorithm to find shortest paths in a weighted graph.
        
        Args:
            times: List of [source, target, time] edges where source sends a signal to target taking 'time' amount of time
            n: Number of nodes in the network (labeled from 1 to n)
            k: Starting node from which the signal is sent
            
        Returns:
            The time for all nodes to receive the signal, or -1 if it's impossible for all nodes to receive the signal
        """
        # Create adjacency list representation of the graph
        # Each node maps to a list of (neighbor, travel_time) pairs
        edges = collections.defaultdict(list)
        for source, target, time in times:
            edges[source].append((target, time))
        
        # Initialize priority queue with starting node and time 0
        min_heap = [(0, k)]  # (time_to_reach, node)
        
        # Track visited nodes to avoid cycles
        visited = set()
        
        # Track maximum time needed to reach any node
        max_time = 0
        
        # Process nodes in order of increasing time
        while min_heap:
            # Get the node with minimum travel time from the heap
            curr_time, curr_node = heapq.heappop(min_heap)
            
            # Skip if we've already processed this node
            if curr_node in visited:
                continue
                
            # Mark node as visited
            visited.add(curr_node)
            
            # Update the maximum time
            max_time = max(max_time, curr_time)
            
            # Explore neighbors
            for neighbor, travel_time in edges[curr_node]:
                # Only process unvisited neighbors
                if neighbor not in visited:
                    # Add to heap with cumulative time
                    total_time = curr_time + travel_time
                    heapq.heappush(min_heap, (total_time, neighbor))
        
        # If we couldn't reach all nodes, return -1
        if len(visited) < n:
            return -1
            
        return max_time

# Test cases
def run_tests():
    solution = Solution()
    
    # Test case 1: Basic example from LeetCode
    times1 = [[2,1,1],[2,3,1],[3,4,1]]
    n1, k1 = 4, 2
    result1 = solution.networkDelayTime(times1, n1, k1)
    print(f"Test 1: {result1}, Expected: 2")
    
    # Test case 2: Node cannot reach all other nodes
    times2 = [[2,1,1],[2,3,1]]
    n2, k2 = 4, 2
    result2 = solution.networkDelayTime(times2, n2, k2)
    print(f"Test 2: {result2}, Expected: -1")
    
    # Test case 3: Single node
    times3 = []
    n3, k3 = 1, 1
    result3 = solution.networkDelayTime(times3, n3, k3)
    print(f"Test 3: {result3}, Expected: 0")
    
    # Test case 4: Complex network
    times4 = [[1,2,1],[2,3,2],[1,3,4]]
    n4, k4 = 3, 1
    result4 = solution.networkDelayTime(times4, n4, k4)
    print(f"Test 4: {result4}, Expected: 3")
    
    # Test case 5: Edge case with large values
    times5 = [[2,4,10],[5,2,38],[3,4,33],[4,2,76],[3,2,64],[1,5,54],[1,4,98],[2,3,61]]
    n5, k5 = 5, 1
    result5 = solution.networkDelayTime(times5, n5, k5)
    print(f"Test 5: {result5}")

# Run the tests
if __name__ == "__main__":
    run_tests()