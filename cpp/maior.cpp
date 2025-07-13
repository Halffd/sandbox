#include <iostream>
using namespace std;
class Utilitario {
private:
  int* vetor;
  int tamanho;
public:
  Utilitario(int* vet, int tam) {
     vetor = vet;
     tamanho = tam;
  }
  int encontrarMaiorNumero() {
      int maior = vetor[0];  
      for (int i = 1; i < tamanho; i++) {
      if (vetor[i] > maior) {
            maior = vetor[i];
      }
    }
    return maior;
  }
};
int main() {
  int numeros[]={5,7,9,10};  
  int tam = end(numeros)-begin(numeros);
  std::cout << "\nA lista de numeros eh:" << std::endl;
  for (int i = 0; i < tam; i++) {
        std::cout << numeros[i]<<" ";
  }
  Utilitario objUtil(numeros, tam);
  int maximo = objUtil.encontrarMaiorNumero();
  std::cout << "\nO maior numero eh: " << maximo << std::endl;
  return 0;
}