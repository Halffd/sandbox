
package main

import "fmt"

type boundariesType struct {
 start string
 end string
}

func (boundaries boundariesType) complete(text string) string {
 return boundaries.start + text + boundaries.end
}

func main() {
 boundaries := boundariesType{
 start: "Hello, ",
 end: "!",
 }
 fmt.Println(boundaries.complete("World"))
 fmt.Println(boundaries.complete("Sekai"))
}
