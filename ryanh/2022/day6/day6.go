package main

import (
	"aoc/helper"
	"fmt"
)

func mapFileContents() []string {
	var sections = helper.GetFileContents(2022, 6)
	return sections
}

func allCharectersUnique(input string) bool {
	var seen = make(map[rune]bool)
	var allUnique = true

	for _, char := range input {
		if _, exists := seen[char]; exists {
			allUnique = false
			break
		} else {
			seen[char] = true
		}
	}

	return allUnique
}

func part1Samples() {
	var example1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
	solve(example1, 4)

	var example2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
	solve(example2, 4)

	var example3 = "nppdvjthqldpwncqszvftbrmjlhg"
	solve(example3, 4)

	var example4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
	solve(example4, 4)

	var example5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
	solve(example5, 4)
}

func part2Samples() {
	var example1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
	solve(example1, 14)

	var example5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
	solve(example5, 14)
}

func solve(input string, messageMarker int) int {
	var startIndex = 0
	var endIndex = messageMarker - 1

	for endIndex+1 < len(input) {
		var testString = input[startIndex : endIndex+1]
		if allCharectersUnique(testString) {
			fmt.Println(testString, endIndex+1)
			return endIndex + 1
		}

		startIndex++
		endIndex++
	}

	return -1
}

func part1() {
	var contents = mapFileContents()
	solve(contents[0], 4)
}

func part2() {
	var contents = mapFileContents()
	solve(contents[0], 14)
}

func main() {
	part2()
}
