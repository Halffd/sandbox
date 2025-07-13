# ALGORITMO 1:
def algoritmo1(a, x, n):
    soma = a[0]
    for i in range(1, n+1):
        if a[i] != 0.0:
            potencia = x
            for j in range(2, i+1):
                potencia = potencia * x
            soma = soma + a[i] * potencia
    return soma

# ALGORITMO 2:
def algoritmo2(a, x, n):
    soma = a[n]
    for i in range(n-1, -1, -1):
        soma = soma * x + a[i]
    return soma

# Example usage:
coefficients = [3, 2, 5, 1]  # Represents 3 + 2x + 5x² + 1x³
x_value = 2
n = len(coefficients) - 1

result1 = algoritmo1(coefficients, x_value, n)
result2 = algoritmo2(coefficients, x_value, n)

print(f"Result using Algorithm 1: {result1}")
print(f"Result using Algorithm 2: {result2}")
