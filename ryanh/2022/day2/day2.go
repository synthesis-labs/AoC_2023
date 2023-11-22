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
	sample := make(map[string]int)
	sample["AY"] = 1
	sample["BX"] = 1
	sample["CZ"] = 1

	// X means I lose, Y means I draw, Z means I win

	// if opponent chooses A, I must choose X
	// if opponent chooses B, I must choose X
	// if opponent chooses C, I must choose X

	// TODO: Edit sample to apply updated rules before calculating points

	// A = ROCK = X (1)
	// B = PAPER = Y (2)
	// C = SCISSORS = Z (3)

	fmt.Println("Initial: ", sample)

	var updatedInput = prepareInput(sample)

	solve(updatedInput)

	// map[AX:2 BX:1 CX:1]
}

func solvePart2() {
	var fileContents = mapFileContents()
	fmt.Println("Initial: ", fileContents)
	var updatedInput = prepareInput(fileContents)
	solve(updatedInput)
}

func prepareInput(input map[string]int) map[string]int {

	var updatedInput = make(map[string]int)
	for k, v := range input {
		updatedInput[k] = v
	}

	for k, v := range input {
		opponent := k[0]
		player := k[1]

		if opponent == 'A' {
			if player == 'X' { // loss
				updatedInput["AZ"] = updatedInput["AZ"] + v
				updatedInput["AX"] = updatedInput["AX"] - v
			}
			if player == 'Y' { // draw
				updatedInput["AX"] = updatedInput["AX"] + v
				updatedInput["AY"] = updatedInput["AY"] - v
			}
			if player == 'Z' { // win
				updatedInput["AY"] = updatedInput["AY"] + v
				updatedInput["AZ"] = updatedInput["AZ"] - v
			}
		}

		// No need to handle when opponent chooses B (ROCK)

		if opponent == 'C' {
			if player == 'X' { // loss
				updatedInput["CY"] = updatedInput["CY"] + v
				updatedInput["CX"] = updatedInput["CX"] - v
			}
			if player == 'Y' { // draw
				updatedInput["CZ"] = updatedInput["CZ"] + v
				updatedInput["CY"] = updatedInput["CY"] - v
			}
			if player == 'Z' { // win
				updatedInput["CX"] = updatedInput["CX"] + v
				updatedInput["CZ"] = updatedInput["CZ"] - v
			}
		}
	}

	for k, v := range updatedInput {
		if v <= 0 {
			delete(updatedInput, k)
		}
	}

	fmt.Println("Final: ", updatedInput)

	return updatedInput
}

func main() {
	solvePart2()
}
