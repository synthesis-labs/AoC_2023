package main

import (
	"fmt"
	"strings"

	"aoc/helper"
)

func mapFileContents() map[string]int {

	result := make(map[string]int)

	var fileContents = helper.GetFileContents(2022, 2)

	for i := 0; i < len(fileContents); i++ {
		var line = fileContents[i]
		var key = strings.ReplaceAll(line, " ", "")

		if value, ok := result[key]; ok {
			if ok {
				result[key] = value + 1
			}
		} else {
			result[key] = 1
		}
	}

	return result
}

func solveSample() {

	// A = ROCK = X (1)
	// B = PAPER = Y (2)
	// C = SCISSORS = Z (3)
	// 6 POINTS FOR A WIN
	// 3 POINTS FOR A DRAW

	// 9 different combinations

	// A X => For example occurs 22 times...
	// map["AX"] = 22
	// map["AY"] = 33

	sample := make(map[string]int)
	sample["AY"] = 1
	sample["BX"] = 1
	sample["CZ"] = 1

	solve(sample)
}

func solve(input map[string]int) {
	var total int = 0

	for k, v := range input {

		opponent := k[0]
		me := k[1]

		// step 1: points for what i choose

		if me == 'X' {
			total = total + (1 * v)
		}

		if me == 'Y' {
			total = total + (2 * v)
		}

		if me == 'Z' {
			total = total + (3 * v)
		}

		// step 2: points for win, draw, loss (9 combos)

		if opponent == 'A' {
			if me == 'X' { // draw
				total = total + (3 * v)
			}
			if me == 'Y' { // win
				total = total + (6 * v)
			}
			if me == 'Z' { // loss
				total = total + (0 * v)
			}
		}

		if opponent == 'B' {
			if me == 'X' { // loss
				total = total + (0 * v)
			}
			if me == 'Y' { // draw
				total = total + (3 * v)
			}
			if me == 'Z' { // win
				total = total + (6 * v)
			}
		}

		if opponent == 'C' {
			if me == 'X' { // win
				total = total + (6 * v)
			}
			if me == 'Y' { // loss
				total = total + (0 * v)
			}
			if me == 'Z' { // draw
				total = total + (3 * v)
			}
		}
	}

	println("Total Points:", total)
}

func solveSample2() {

}

func solvePart2(input map[string]int) {

}

func main() {
	//solveSample()
	var fileContents = mapFileContents()
	fmt.Println(fileContents)

	solve(fileContents)

}
