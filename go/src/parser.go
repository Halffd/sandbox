package main 

import (
 "bufio"
 "fmt"
 "os"
 "strconv"
 "strings"
)

func main() {
 commands := readInputAsStrings("input.txt")
 fmt.Println(getFinalPosition(commands))
}

func getFinalPosition(commands []string) (aim int, depth int, horizontal int) {
 for _, s := range(commands) {
 // parseCommand
 direction, magnitude := parseCommand(s)
 // Modify state
 if direction == "forward" {
 horizontal += magnitude
 depth += aim * magnitude
 }
 if direction == "down" {
 aim += magnitude
 }
 if direction == "up" {
 aim -= magnitude
 }
 }
 return
}

func parseCommand(command string) (direction string, magnitude int) {
 s := strings.Split(command, " ")
 direction = s[0]
 magnitude, _ = strconv.Atoi(s[1])
 return
}

func readInputAsStrings(filename string) (input []string) {
 file, _ := os.Open(filename)
 defer file.Close()
 scanner := bufio.NewScanner(file)
 for scanner.Scan() {
 depth := scanner.Text()
 input = append(input, depth)
 }
 return
}
