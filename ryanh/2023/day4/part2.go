package main

import (
	"fmt"
	"strings"
)

type Card struct {
	Index   int
	Numbers []int
	Count   int
}

func solvePart2(input []string) {

	var scratchCards = []Card{}

	for i, line := range input {

		var cards = strings.Split(line, "|")

		var index = strings.Index(cards[0], ":")
		var card = strings.TrimSpace(cards[0][index+1:])

		var winningNumbers = GetCardNumbers(card)
		var playingNumbers = GetCardNumbers(cards[1])

		var intersectingNumbers = GetIntersectingNumbers(winningNumbers, playingNumbers)
		fmt.Println(intersectingNumbers)

		scratchCards = append(scratchCards, Card{Index: i, Numbers: intersectingNumbers, Count: len(intersectingNumbers)})
	}

}

func Part2() {
	solvePart2(GetSampleData())
}
