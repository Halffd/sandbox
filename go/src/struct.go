package main

import "fmt"

// Define the Endereco struct (ENDEREÇO)
type Endereco struct {
	Rua string
	Nro int
	Cep int
}

// Define the Func struct (FUNC)
type Func struct {
	Nome     string
	Endereco Endereco // Nested Endereco struct
	Cidade   string
	Estado   string
	Salario  float64
}

func main() {
	// Declare a variable of type Func
	var funcionario Func

	// Assign values to the fields
	funcionario.Nome = "Joana Curadora"
	funcionario.Endereco.Rua = "Avenida das Americas"
	funcionario.Endereco.Nro = 4200
	funcionario.Endereco.Cep = 22640102
	funcionario.Cidade = "Rio de Janeiro"
	funcionario.Estado = "RJ"
	funcionario.Salario = 1.00

	// Print the struct to verify the values
	fmt.Printf("%+v\n", funcionario)
}