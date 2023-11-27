package main

import (
	"aoc/helper"
	"fmt"
	"strings"
)

const alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

func mapFileContents() []string {

	var itemsInRucksack = helper.GetFileContents(2022, 3)

	return itemsInRucksack
}

func getUniqueString(input string) string {

	// simulates a set
	set := make(map[string]bool)
	for i := 0; i < len(input); i++ {
		set[string(input[i])] = true
	}

	keys := make([]string, len(set))
	for key := range set {
		keys = append(keys, key)
	}
	keyString := strings.Join(keys, "")

	return keyString
}

func decodeCharacter(input string) int {
	var result int

	index := strings.IndexRune(alphabet, rune(input[0]))
	result = index + 1

	return result
}

func solveSample() {
	var itemsInRucksack = []string{"vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
		"PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"}

	solve(itemsInRucksack)
}

func part1() {
	var itemsInRucksack = mapFileContents()
	solve(itemsInRucksack) // 7863
}

func solve(input []string) {
	// Each rucksack has two large compartments.

	// step 1: split each rucksack into two compartments
	// step 2: find duplicate letter found in both
	// step 3: map character to priority
	// step 4: sum priority of each rucksack

	// step 1

	var sum int

	// not exactly performant but it works O(n^3) ;)
	for i := 0; i < len(input); i++ {
		var rucksack = input[i]

		var compartment1 = rucksack[0 : len(rucksack)/2]
		var compartment2 = rucksack[len(rucksack)/2:]

		var compartment1Unique = getUniqueString(compartment1)
		var compartment2Unique = getUniqueString(compartment2)

	outer:
		for j := 0; j < len(compartment1Unique); j++ {
			for k := 0; k < len(compartment2Unique); k++ {
				if compartment1Unique[j] == compartment2Unique[k] {
					var duplicate = string(compartment1Unique[j])
					var score = decodeCharacter(duplicate)
					sum += score

					break outer
				}
			}
		}
	}

	fmt.Println(sum)
}

func solvePart2Sample() {
	var itemsInRucksack = []string{"vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
		"PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"}

	solvePart2(itemsInRucksack)
}

func part2() {
	var itemsInRucksack = mapFileContents()
	solvePart2(itemsInRucksack)
}

func solvePart2(input []string) {

	var sum int
	for i := 0; i < len(input); i += 3 {
		var elfGroup = input[i : i+3]
		var commomLetter = getCommonLetter(elfGroup)
		var score = decodeCharacter(commomLetter)
		sum += score
	}

	fmt.Println(sum)
}

func getCommonLetter(elfGroup []string) string {
	commonCharacters := make(map[rune]int)

	for _, str := range elfGroup {
		set := make(map[rune]bool)
		for _, char := range str {
			set[char] = true
		}
		for char := range set {
			commonCharacters[char]++
		}
	}

	for char, count := range commonCharacters {
		if count == len(elfGroup) {
			return string(char)
		}
	}

	return ""
}

func main() {
	part2()
}
