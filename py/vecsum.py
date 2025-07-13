def somavet(v, n):
    """Recursively sum elements of list v up to index n."""
    if n == 0:
        return 0
    else:
        return v[n - 1] + somavet(v, n - 1)

v = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print(f"Sum of elements: {somavet(v, len(v))}")
