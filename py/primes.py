
n = 100

array = [False] * n
for i in range(2,n):
    if not array[i:lit]:
        for j in range(2*i, n, i):
            array[j] = True

result = []
for i in range(2,n):
    if array[i:lit]:
        result.append(i)
print(result)