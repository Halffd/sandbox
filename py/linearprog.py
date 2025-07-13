from pulp import LpProblem, LpVariable, LpMaximize, LpStatus, value
import numpy as np
import matplotlib.pyplot as plt

# Create the linear programming problem
problem = LpProblem("Simple_Linear_Programming", LpMaximize)

# Define decision variables
x = LpVariable('x', lowBound=2)  # x >= 2
y = LpVariable('y', lowBound=3)  # y >= 3

# Objective function
problem += 3 * x + 5 * y, "Z"

# Constraints
problem += x + y <= 10, "Constraint_1"

# Solve the problem
problem.solve()

# Print results
print("Status:", LpStatus[problem.status])
print("Optimal Solution:")
print(f"x = {value(x):.2f}")
print(f"y = {value(y):.2f}")
print(f"Objective Value Z = {value(problem.objective):.2f}")

# Visualization
fig, ax = plt.subplots(figsize=(10, 8))

# Set up the plot boundaries
x_vals = np.linspace(0, 12, 400)
y_vals = np.linspace(0, 12, 400)

# Plot constraints
plt.plot(x_vals, 10 - x_vals, label=r'$x + y \leq 10$', color='blue')
plt.axvline(x=2, color='green', label=r'$x \geq 2$')
plt.axhline(y=3, color='red', label=r'$y \geq 3$')

# Fill feasible region
x_feasible = np.linspace(2, 7, 100)  # x >= 2 and x + y <= 10 ⇒ x <= 7 when y=3
y_lower = np.maximum(3, 10 - x_feasible)  # y >=3 and y >= 10-x
plt.fill_between(x_feasible, 3, 10 - x_feasible, 
                 where=(10 - x_feasible) >= 3, 
                 color='gray', alpha=0.3, label='Feasible Region')

# Plot optimal solution
optimal_x = value(x)
optimal_y = value(y)
plt.scatter(optimal_x, optimal_y, color='red', s=100, 
            label=f'Optimal Solution\n({optimal_x:.1f}, {optimal_y:.1f})')

# Plot objective function for different Z values
z_values = [value(problem.objective), 30, 20]
colors = ['black', 'gray', 'lightgray']
for z, c in zip(z_values, colors):
    y_obj = (z - 3*x_vals)/5
    plt.plot(x_vals, y_obj, linestyle='--', color=c, 
             label=f'Z = {z}' if z == value(problem.objective) else None)

# Plot settings
plt.xlim(0, 12)
plt.ylim(0, 12)
plt.xlabel('x')
plt.ylabel('y')
plt.title('Linear Programming Problem Visualization')
plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
plt.grid(True)
plt.tight_layout()

# Show the plot
plt.show()