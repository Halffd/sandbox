# Implementation of the Traveling Salesman Problem (TSP) in Python

import numpy as np
import itertools

class TravelingSalesmanProblem:
    def __init__(self, distance_matrix):
        """
        Initialize the TSP solver with a distance matrix
        
        Args:
            distance_matrix: 2D numpy array or list of lists representing distances between cities
        """
        self.distance_matrix = np.array(distance_matrix)
        self.num_cities = len(distance_matrix)
        
    def brute_force(self, start_city=0):
        """
        Solve TSP using brute force approach (exact but exponential time)
        
        Args:
            start_city: The city to start and end the tour
            
        Returns:
            tuple: (optimal_path, minimum_cost)
        """
        if self.num_cities > 12:
            print("Warning: Brute force approach may take a very long time with more than 12 cities")
            
        # Get all cities except the starting one
        cities = list(range(self.num_cities))
        cities.remove(start_city)
        
        min_cost = float('inf')
        optimal_path = None
        
        # Try all possible permutations
        for perm in itertools.permutations(cities):
            # Create the complete path starting and ending at start_city
            path = [start_city] + list(perm) + [start_city]
            
            # Calculate path cost
            cost = sum(self.distance_matrix[path[i]][path[i+1]] for i in range(len(path)-1))
            
            # Update if better
            if cost < min_cost:
                min_cost = cost
                optimal_path = path
                
        return optimal_path, min_cost
    
    def nearest_neighbor(self, start_city=0):
        """
        Solve TSP using the Nearest Neighbor heuristic (fast but approximate)
        
        Args:
            start_city: The city to start and end the tour
            
        Returns:
            tuple: (path, cost)
        """
        path = [start_city]
        unvisited = set(range(self.num_cities))
        unvisited.remove(start_city)
        
        total_cost = 0
        
        # Greedily select next city
        current_city = start_city
        while unvisited:
            next_city = min(unvisited, key=lambda city: self.distance_matrix[current_city][city])
            unvisited.remove(next_city)
            path.append(next_city)
            total_cost += self.distance_matrix[current_city][next_city]
            current_city = next_city
            
        # Return to starting city
        path.append(start_city)
        total_cost += self.distance_matrix[current_city][start_city]
        
        return path, total_cost
    
    def two_opt(self, initial_path):
        """
        Improve a given path using 2-opt local search
        
        Args:
            initial_path: A valid TSP tour
            
        Returns:
            tuple: (improved_path, improved_cost)
        """
        best_path = initial_path.copy()
        improvement = True
        
        while improvement:
            improvement = False
            best_cost = self._calculate_path_cost(best_path)
            
            for i in range(1, len(best_path) - 2):
                for j in range(i + 1, len(best_path) - 1):
                    # Skip if edges are consecutive
                    if j == i + 1:
                        continue
                        
                    # Create new path with 2-opt swap
                    new_path = best_path.copy()
                    new_path[i:j+1] = reversed(best_path[i:j+1])
                    new_cost = self._calculate_path_cost(new_path)
                    
                    if new_cost < best_cost:
                        best_path = new_path
                        best_cost = new_cost
                        improvement = True
                        break
                
                if improvement:
                    break
                    
        return best_path, best_cost
    
    def _calculate_path_cost(self, path):
        """Calculate the total cost of a path"""
        return sum(self.distance_matrix[path[i]][path[i+1]] for i in range(len(path)-1))

# Example usage
if __name__ == "__main__":
    # Example: 4 cities with their distances
    # Representing the Southeastern states in Brazil (ES, MG, SP, RJ)
    distances = [
        [0, 300, 900, 500],  # ES to other states
        [300, 0, 600, 400],   # MG to other states
        [900, 600, 0, 400],   # SP to other states
        [500, 400, 400, 0]    # RJ to other states
    ]
    
    tsp = TravelingSalesmanProblem(distances)
    
    # Solve using brute force (exact solution)
    optimal_path, min_cost = tsp.brute_force()
    print(f"Optimal path: {optimal_path}")
    print(f"Minimum cost: {min_cost}")
    
    # Solve using nearest neighbor (heuristic)
    nn_path, nn_cost = tsp.nearest_neighbor()
    print(f"\nNearest neighbor path: {nn_path}")
    print(f"Nearest neighbor cost: {nn_cost}")
    
    # Improve nearest neighbor solution with 2-opt
    improved_path, improved_cost = tsp.two_opt(nn_path)
    print(f"\nImproved path: {improved_path}")
    print(f"Improved cost: {improved_cost}")
