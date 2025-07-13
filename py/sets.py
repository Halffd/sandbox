# Definindo conjuntos básicos
# Conjunto de letras minúsculas do alfabeto
letras_minusculas = set("abcdefghijklmnopqrstuvwxyz")
print(f"Letras minúsculas: {letras_minusculas}")

# Conjunto de inteiros ímpares positivos (limitado aos primeiros 10 para exemplo)
impares_positivos = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19}
# Ou podemos criar usando compreensão de conjunto
i = 0
impares_positivos_comp = {i for i in range(1, 20, 2)}
print(f"Inteiros ímpares positivos: {impares_positivos_comp}")

# Representação mais explícita do conjunto S = {i: i > 0, i é ímpar}
def eh_impar(n):
    return n % 2 != 0

# Usando compreensão de conjunto para criar S (limitando para os 10 primeiros)
S = {i for i in range(1, 21) if i > 0 and eh_impar(i)}
print(f"S = {S}")

# Demonstrando operações de conjuntos
A = {2, 3, 4, 5}
B = {3, 4}

# União
uniao = A.union(B)  # ou A | B
print(f"A ∪ B: {uniao}")

# Interseção
intersecao = A.intersection(B)  # ou A & B
print(f"A ∩ B: {intersecao}")

# Diferença
diferenca = A.difference(B)  # ou A - B
print(f"A - B: {diferenca}")

# Produto cartesiano
produto_cartesiano = [(a, b) for a in A for b in B]
print(f"A × B: {produto_cartesiano}")

# Conjunto vazio
conjunto_vazio = set()
print(f"Conjunto vazio: {conjunto_vazio}")

# Verificando subconjuntos
S1 = {3, 4}
is_subset = S1.issubset(A)  # ou S1 <= A
print(f"S1 ⊂ A: {is_subset}")

# Conjunto potência (para conjuntos pequenos)
from itertools import chain, combinations

def conjunto_potencia(s):
    return list(chain.from_iterable(combinations(s, r) for r in range(len(s) + 1)))

C = {1, 2, 3}
power_set = conjunto_potencia(C)
print(f"Conjunto potência de {C}: {power_set}")
