package main

import (
	"aoc/helper"
	"fmt"
	"strconv"
	"strings"
)

func mapFileContents() []string {

	var sections = helper.GetFileContents(2022, 4)

	return sections
}

func part1Sample() {
	positiveTest()
	negativeTest()
}

func positiveTest() {
	var sample = []string{"2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "4-6,6-6", "2-6,4-8"}
	solvePart1(sample)
}

func negativeTest() {
	var sample = []string{"2-4,6-8", "2-3,4-5", "5-7,7-9", "1-1,2-2", "2-2,1-1", "22-33,33-34", "33-66,32-33"}
	solvePart1(sample)
}

func part1() {
	var content = mapFileContents()

	fmt.Println(content)
	solvePart1(content)
	// Final Answer: 573
}

func solvePart1(input []string) {
	// algorithm:
	// loop through each elfGroup
	// get elf 1 min and max
	// get eld 2 min and max
	// generate range arrays for each elf and get where the arrays fully intesect
	// return count of these

	var assignmentPairs int

	for _, pairs := range input {
		var elfGroup = strings.Split(pairs, ",")
		var elf1 = elfGroup[0]

		var elf1Min, elf1Max = getElfRange(elf1)
		var elf1NewRange = generateArrayRange(elf1Min, elf1Max)

		var elf2 = elfGroup[1]

		var elf2Min, elf2Max = getElfRange(elf2)
		var elf2NewRange = generateArrayRange(elf2Min, elf2Max)

		if elf1Min <= elf2Min && elf1Max >= elf2Max {
			if arrayFullyIntersects(elf1NewRange, elf2NewRange) {
				assignmentPairs++
			}
		} else {
			if arrayFullyIntersects(elf2NewRange, elf1NewRange) {
				assignmentPairs++
			}
		}
	}

	fmt.Println(assignmentPairs)
}

func getElfRange(elf string) (int, int) {
	var elfRange = strings.Split(elf, "-")
	elfMin, _ := strconv.Atoi(elfRange[0])
	elfMax, _ := strconv.Atoi(elfRange[1])

	return elfMin, elfMax
}

func arrayFullyIntersects(a, b []int) bool {
	m := make(map[int]bool)
	for _, item := range a {
		m[item] = true
	}
	for _, item := range b {
		if !m[item] {
			return false
		}
	}
	return true
}

func arrayOverlaps(a, b []int) bool {
	m := make(map[int]bool)
	for _, item := range a {
		m[item] = true
	}
	for _, item := range b {
		if m[item] {
			return true
		}
	}

	return false
}

func generateArrayRange(min, max int) []int {
	rangeSlice := make([]int, 0, max-min+1)
	for i := min; i <= max; i++ {
		rangeSlice = append(rangeSlice, i)
	}
	return rangeSlice
}

func part2Sample() {
	part2PositiveTest()
}

func part2PositiveTest() {
	var sample = []string{"2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"}
	solvePart2(sample)
}

func part2() {
	var content = mapFileContents()
	solvePart2(content)
}

func solvePart2(input []string) {
	var assignmentPairs int

	for _, pairs := range input {
		var elfGroup = strings.Split(pairs, ",")
		var elf1 = elfGroup[0]

		var elf1Min, elf1Max = getElfRange(elf1)
		var elf1NewRange = generateArrayRange(elf1Min, elf1Max)

		var elf2 = elfGroup[1]

		var elf2Min, elf2Max = getElfRange(elf2)
		var elf2NewRange = generateArrayRange(elf2Min, elf2Max)

		// Difference here is that you don't need to check the order of the ranges since we're only looking for an overlap of a single digit.
		if arrayOverlaps(elf1NewRange, elf2NewRange) {
			assignmentPairs++
		}
	}

	fmt.Println(assignmentPairs)
}

func main() {
	part2()
}
