package main

import (
	"fmt"
	"strconv"
	"strings"
)

func solvePart2Sample() {
	var hands = GetSampleInput()
	part2Solution(hands)
}

func part2Solution(hands []string) {
	var myHands = classifyHands2(hands)

	// myHands = sortHands(myHands)

	fmt.Println(myHands)

	// var total = calculateTotalWinnings(myHands)
	// fmt.Println(total)
}

func classifyHands2(input []string) []Hand {

	var hands []Hand

	for _, val := range input {
		var hand Hand

		var handStr = strings.TrimSpace(val)
		var handSplit = strings.Split(handStr, " ")

		hand.hand = handSplit[0]
		bid, _ := strconv.Atoi(handSplit[1])
		hand.bid = bid

		if isFiveOfAKind2(hand.hand) {
			hand.classification = FiveOfAKind
		} else if isFourOfAKind2(hand.hand) {
			hand.classification = FourOfAKind
		} else if isFullHouse2(hand.hand) {
			hand.classification = FullHouse
		} else if isThreeOfAKind2(hand.hand) {
			hand.classification = ThreeOfAKind
		} else if isTwoPair(hand.hand) {
			hand.classification = TwoPair
		} else if isOnePair(hand.hand) {
			hand.classification = OnePair
		} else {
			hand.classification = HighCard
		}

		hands = append(hands, hand)
	}

	return hands
}

// AAAAA OR AAAJA
func isFiveOfAKind2(hand string) bool {
	valMap := make(map[rune]bool)

	for _, val := range hand {
		if val != 'J' {
			valMap[val] = true
		}
	}

	return len(valMap) == 1
}

// AA8AA OR QQQJA OR QQJJA
func isFourOfAKind2(hand string) bool {
	valMap := make(map[rune]int)

	var jokers = 0
	for _, val := range hand {

		if val != 'J' {
			if _, exists := valMap[val]; exists {
				valMap[val] = valMap[val] + 1
			} else {
				valMap[val] = 1
			}
		}

		if val == 'J' {
			jokers++
		}
	}

	// map[Q:3 A:1], map[A:4 8:1], map[Q:2 A:1]

	for _, val := range valMap {
		if val == 4 {
			return true
		}

		if val == 3 && jokers == 1 {
			return true
		}

		if val == 2 && jokers == 2 {
			return true
		}
	}

	return false
}

// 22333
func isFullHouse2(hand string) bool {
	valMap := make(map[rune]int)

	var jokers = 0
	for _, val := range hand {
		if val != 'J' {
			if _, exists := valMap[val]; exists {
				valMap[val] = valMap[val] + 1
			} else {
				valMap[val] = 1
			}
		} else {
			jokers++
		}
	}

	// map[2:2 3:3], map[2:2 3:2]

	twos := 0
	threes := 0
	for _, val := range valMap {
		if val == 2 {
			twos++
		}
		if val == 3 {
			threes++
		}
	}

	if twos == 2 && threes == 3 {
		return true
	}

	if twos == 2 && threes == 2 && jokers == 1 {
		return true
	}

	return false
}

// TTT98, TTJ98, TJJ98
func isThreeOfAKind2(hand string) bool {
	valMap := make(map[rune]int)

	var jokers = 0
	for _, val := range hand {
		if val != 'J' {
			if _, exists := valMap[val]; exists {
				valMap[val] = valMap[val] + 1
			} else {
				valMap[val] = 1
			}
		} else {
			jokers++
		}
	}

	// map[T:3 9:1, 8:1], map[T:2 9:1, 8:1], map[T:1 9:1, 8:1]

	for _, val := range valMap {
		if val == 3 {
			return true
		}
		if val == 2 && jokers == 1 {
			return true
		}
		if val == 1 && jokers == 2 {
			return true
		}
	}

	return false
}

func getCardValue2(card rune) int {
	switch card {
	case 'T':
		return 10
	case 'J':
		return 1
	case 'Q':
		return 12
	case 'K':
		return 13
	case 'A':
		return 14
	default:
		return int(card)
	}
}

func Part2() {
	solvePart2Sample()
}
