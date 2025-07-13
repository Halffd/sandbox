package main

import "fmt"

type Product struct {
    Name     string
    Category string
    Price    int64
}

func main() {
    products := [...]Product{
        {Name: "Laptop", Category: "Electronics", Price: 99999},
        {Name: "Smartphone", Category: "Electronics", Price: 49999},
        {Name: "T-Shirt", Category: "Clothing", Price: 1999},
        {Name: "Jeans", Category: "Clothing", Price: 3999},
        {Name: "Coffee Maker", Category: "Home Appliances", Price: 8999},
    }

    groupedProducts := map[string][]Product{}
    for _, p := range products {
        groupedProducts[p.Category] = append(groupedProducts[p.Category], p)
    }

    for c, g := range groupedProducts {
        fmt.Println("Category:", c)
        for _, p := range g {
            fmt.Printf(" - %s: $%d.%02d\n", p.Name, p.Price/100, p.Price%100)
        }
    }
}