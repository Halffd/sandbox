package main

import (
	"fmt"
)

func pesqrec(v []float64, r float64, i, j int) bool {
	if i == j {
		if v[i] == r {
			return true
		} else {
			return false
		}
	}

	mid := (i + j) / 2

	return pesqrec(v, r, i, mid) || pesqrec(v, r, mid+1, j)
}

func pesqbin(v []float64, r float64) bool {
	return pesqrec(v, r, 0, len(v)-1)
}

func main() {
	v := []float64{1.0, 2.1, 3.3, 4.5, 5.5, 6.7, 7.9, 8.8, 9.9, 10.0}
	r := 5.5

	if pesqbin(v, r) {
		fmt.Println("Found!")
	} else {
		fmt.Println("Not found.")
	}
}
