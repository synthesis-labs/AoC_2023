package main

import (
	"strings"
)

func solvePart2(input []string) {

	var scratchCards = [][]int{}

	for _, line := range input {

		var cards = strings.Split(line, "|")

		var index = strings.Index(cards[0], ":")
		var card = strings.TrimSpace(cards[0][index+1:])

		var winningNumbers = GetCardNumbers(card)
		var playingNumbers = GetCardNumbers(cards[1])

		var intersectingNumbers = GetIntersectingNumbers(winningNumbers, playingNumbers)

		scratchCards = append(scratchCards, intersectingNumbers)
	}
}

func copyCard(input []int) []int {

	var result = []int{}

	for i := 0; i < len(input); i++ {

		result = append(result, input[i])
	}

	return result
}

func Part2() {
	solvePart2(GetSampleData())
}
