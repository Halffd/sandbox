import numpy as np
import matplotlib.pyplot as plt

# Constraint line
x = np.linspace(0, 10, 100)
y = 10 - x

# Objective function contours
X, Y = np.meshgrid(np.linspace(0, 10, 100), np.linspace(0, 10, 100))
Z = X * Y

plt.figure(figsize=(10, 6))
plt.plot(x, y, 'r-', label='Constraint: $x + y = 10$')
plt.contour(X, Y, Z, levels=np.arange(0, 26, 5), cmap='viridis', alpha=0.5)
plt.scatter(5, 5, color='red', s=100, label=f'Optimal (5,5): $f=25$')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Maximizing $f(x,y)=xy$ s.t. $x+y=10$')
plt.legend()
plt.grid()
plt.colorbar(plt.contourf(X, Y, Z, levels=20), label='$f(x,y)=xy$')
plt.show()