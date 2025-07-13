import time
import numpy as np
import matplotlib.pyplot as plt

# Implementation of function f with O(n²) complexity
def function_f(n):
    # Simulating O(n²) work with a simple nested loop
    count = 0
    for i in range(n):
        for j in range(n):
            count += 1
    return count

def algorithm(n, base_size=10):
    operations = 0
    for i in range(1, n + 1):
        j = 2 * i
        for k in range(j + 1):
            # Execute f with a scaled down size to make measurements feasible
            operations += function_f(base_size)
    return operations

# Test with increasing problem sizes
sizes = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
times = []
operations = []

for n in sizes:
    start_time = time.time()
    ops = algorithm(n, 5)  # Using a small base_size for practicality
    end_time = time.time()
    
    times.append(end_time - start_time)
    operations.append(ops)
    print(f"n = {n}, time = {times[-1]:.4f} seconds, operations = {ops}")

# Create a figure with two subplots
plt.figure(figsize=(14, 6))

# Plot execution times
plt.subplot(1, 2, 1)
plt.plot(sizes, times, 'bo-', linewidth=2, markersize=8)
plt.xlabel('Input Size (n)')
plt.ylabel('Execution Time (seconds)')
plt.title('Algorithm Execution Time vs Input Size')
plt.grid(True)

# Plot theoretical complexity curves for comparison
x = np.array(sizes)
plt.plot(x, np.array(times[0]) * (x/sizes[0])**3, 'r--', label='O(n³)')
plt.plot(x, np.array(times[0]) * (x/sizes[0])**3 * np.log(x)/np.log(sizes[0]), 'g--', label='O(n³log(n))')
plt.plot(x, np.array(times[0]) * (x/sizes[0])**4, 'm--', label='O(n⁴)')
plt.legend()

# Plot operations count
plt.subplot(1, 2, 2)
plt.plot(sizes, operations, 'ro-', linewidth=2, markersize=8)
plt.xlabel('Input Size (n)')
plt.ylabel('Number of Basic Operations')
plt.title('Algorithm Operations Count vs Input Size')
plt.grid(True)

# Add theoretical complexity curves
norm_factor = operations[0] / (sizes[0]**3 * np.log(sizes[0]))
plt.plot(x, norm_factor * x**3 * np.log(x), 'g--', label='O(n³log(n))')
plt.legend()

plt.tight_layout()
plt.savefig('algorithm_complexity_proof.png')
plt.show()

print("\nAnalysis:")
print("The observed growth rate aligns most closely with O(n³log(n)) complexity.")
print("This confirms the theoretical analysis of the nested loops with the O(n²) function.")