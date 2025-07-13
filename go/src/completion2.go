
package main

import "fmt"

func newCompletion(start, end string) func(text string) string {
 return func(text string) string {
 return start + text + end
 }
}

func main() {
 complete := newCompletion("Hello, ", "!")
 fmt.Println(complete("World"))
}

