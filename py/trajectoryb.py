
import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Physics constants
alpha = 0.5  # Paraboloid steepness
g = 9.81     # Earth's way of saying "nice try"

# Get user input for friction with snarky guidance
print("=== BEYBLADE PHYSICS SIMULATOR 9000 ===")
print("Input friction coefficient (μ):")
print(" • 0.0: Frictionless fantasy land (spiral forever)")
print(" • 0.05-0.1: Decent Beyblade on smooth surface")
print(" • 0.2-0.5: Did you lubricate with maple syrup?")
print(" • 1.0+: Basically spinning in concrete")

while True:
    try:
        mu = float(input("Enter μ value (recommended: 0.0-1.0): "))
        if mu < 0:
            print("Negative friction? What is this, a perpetual motion machine?")
        else:
            break
    except ValueError:
        print("That's not a number, that's a cry for help.")

# Define the differential equations
def beyblade_motion(t, state):
    x, y, vx, vy = state
    
    # Surface gradient
    dz_dx = 2 * alpha * x
    dz_dy = 2 * alpha * y
    
    # Denominator for projection onto tangent plane
    denom = np.sqrt(1 + dz_dx**2 + dz_dy**2)
    
    # Acceleration with our user-defined existential crisis coefficient
    ax = -g * dz_dx / denom - mu * vx
    ay = -g * dz_dy / denom - mu * vy
    
    return [vx, vy, ax, ay]

# Initial conditions
x0 = 0.8
y0 = 0.0
vx0 = 0.0
vy0 = 0.8
initial_state = [x0, y0, vx0, vy0]

# Solve the differential equations
# Higher mu = shorter simulation needed
sim_time = max(10, min(50, 20/mu)) if mu > 0 else 20
t_span = (0, sim_time)
t_eval = np.linspace(0, sim_time, 1000)
solution = solve_ivp(beyblade_motion, t_span, initial_state, 
                     t_eval=t_eval, method='RK45')

# Extract results
x = solution.y[0]
y = solution.y[1]
z = alpha * (x**2 + y**2)

# Plot results
fig = plt.figure(figsize=(12, 9))
ax = fig.add_subplot(111, projection='3d')

# Create paraboloid surface
x_surf, y_surf = np.meshgrid(np.linspace(-1, 1, 20), np.linspace(-1, 1, 20))
z_surf = alpha * (x_surf**2 + y_surf**2)
ax.plot_surface(x_surf, y_surf, z_surf, alpha=0.3, color='lightblue')

# Plot the trajectory
ax.plot(x, y, z, 'r-', linewidth=2, label='Beyblade Trajectory')
ax.scatter(0, 0, 0, color='green', s=100, label='Center')
ax.scatter(x[0], y[0], z[0], color='blue', s=100, label='Start')
ax.scatter(x[-1], y[-1], z[-1], color='purple', s=100, label='End')

# Add title with physics info
ax.set_title(f'Beyblade Trajectory (μ={mu:.3f}, Spiral Loops: {len(np.where(np.diff(np.arctan2(y, x)) < -6)[0])+1})')
ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.legend()

# Add velocity decay graph as inset
vels = np.sqrt(solution.y[2]**2 + solution.y[3]**2)
energy = 0.5 * vels**2 + g * z
ax_inset = fig.add_axes([0.15, 0.15, 0.3, 0.2])
ax_inset.plot(solution.t, vels, 'g-', label='Velocity')
ax_inset.set_title('Velocity Decay')
ax_inset.set_xlabel('Time')
ax_inset.grid(True)

plt.show()

print(f"\nSimulation Results:")
print(f"• Initial velocity: {np.sqrt(vx0**2 + vy0**2):.2f} m/s")
print(f"• Final position: ({x[-1]:.3f}, {y[-1]:.3f})")
print(f"• Distance from center: {np.sqrt(x[-1]**2 + y[-1]**2):.3f} m")
print(f"• Spiral completed in {solution.t[-1]:.2f} seconds")
print(f"• {'Still moving!' if vels[-1] > 0.01 else 'Effectively stopped.'}")