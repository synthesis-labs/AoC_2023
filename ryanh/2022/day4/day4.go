package main

import (
	"aoc/helper"
	"fmt"
	"strings"
)

func mapFileContents() []string {

	var sections = helper.GetFileContents(2022, 4)

	return sections
}

func part1Sample() {
	var sample = []string{"2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8", "8-54,8-54", "9-61,60-94"}

	solvePart1(sample)
}

func part1() {
	var content = mapFileContents()

	fmt.Println(content)
	solvePart1(content)
}

func solvePart1(input []string) {
	// algorithm:
	// loop through each elfGroup
	// get elf 1 min and max
	// get eld 2 min and max
	// check 2 variations of ranges
	// return count of these

	var assignmentPairs int

	for _, pairs := range input {
		var elfGroup = strings.Split(pairs, ",")
		var elf1 = elfGroup[0]

		var elf1Range = strings.Split(elf1, "-")

		var elf1Min = elf1Range[0]
		var elf1Max = elf1Range[1]

		var elf2 = elfGroup[1]
		var elf2Range = strings.Split(elf2, "-")

		var elf2Min = elf2Range[0]
		var elf2Max = elf2Range[1]

		var found = false

		if elf1Min >= elf2Min && elf1Min <= elf2Max {
			if elf1Max >= elf2Min && elf1Max <= elf2Max {
				found = true
				assignmentPairs++
				println("found", elf1, elf2)
			}
		}

		if !found {
			if elf2Min >= elf1Min && elf2Min <= elf1Max {
				if elf2Max >= elf1Min && elf2Max <= elf1Max {
					found = true
					assignmentPairs++
					println("found", elf1, elf2)
				}
			}
		}

		if !found {
			if elf1Min == elf2Min && elf1Max == elf2Max {
				found = true
				assignmentPairs++
				println("found", elf1, elf2)
			}
		}
	}

	fmt.Println(assignmentPairs)
}

func part2() {

}

func main() {
	part1()
}
