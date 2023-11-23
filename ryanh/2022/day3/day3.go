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

func solveMain() {
	var itemsInRucksack = mapFileContents()
	solve(itemsInRucksack)
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

		var duplicateFound = false

		if !duplicateFound {
			for j := 0; j < len(compartment1Unique); j++ {
				for k := 0; k < len(compartment2Unique); k++ {
					if compartment1Unique[j] == compartment2Unique[k] {
						var duplicate = string(compartment1Unique[j])
						var score = decodeCharacter(duplicate)
						sum += score

						duplicateFound = true
					}
				}
			}
		}
	}

	fmt.Println(sum)
}

func main() {
	solveMain() // 7863
}
