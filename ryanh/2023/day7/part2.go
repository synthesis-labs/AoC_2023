package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

func solvePart2Sample() {
	var hands = GetSampleInput()
	part2Solution(hands)
}

func solvePart2Main() {
	var hands = GetMainInput()
	part2Solution(hands) //254837137 -> too low
}

func part2Solution(hands []string) {
	var myHands = classifyHands2(hands)

	myHands = sortHands2(myHands)

	fmt.Println(myHands)

	var total = calculateTotalWinnings(myHands)
	fmt.Println(total)
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
		} else if isTwoPair2(hand.hand) {
			hand.classification = TwoPair
		} else if isOnePair2(hand.hand) {
			hand.classification = OnePair
		} else {
			hand.classification = HighCard
		}

		hands = append(hands, hand)
	}

	return hands
}

func sortHands2(hands []Hand) []Hand {

	result := []Hand{}

	// First sort by Classification
	sort.Slice(hands, func(i, j int) bool {
		return int(hands[i].classification) < int(hands[j].classification)
	})

	// Then sort by subclassification

	var highCard = Filter(hands, func(h Hand) bool {
		return h.classification == HighCard
	})

	if len(highCard) > 1 {
		highCard = subSortHands2(highCard)
	}

	if len(highCard) > 0 {
		result = append(result, highCard...)
	}

	var onePair = Filter(hands, func(h Hand) bool {
		return h.classification == OnePair
	})

	if len(onePair) > 1 {
		onePair = subSortHands2(onePair)
	}

	if len(onePair) > 0 {
		result = append(result, onePair...)
	}

	var twoPairs = Filter(hands, func(h Hand) bool {
		return h.classification == TwoPair
	})

	if len(twoPairs) > 1 {
		twoPairs = subSortHands2(twoPairs)
	}

	if len(twoPairs) > 0 {
		result = append(result, twoPairs...)
	}

	var threeOfAKind = Filter(hands, func(h Hand) bool {
		return h.classification == ThreeOfAKind
	})

	if len(threeOfAKind) > 1 {
		threeOfAKind = subSortHands2(threeOfAKind)
	}

	if len(threeOfAKind) > 0 {
		result = append(result, threeOfAKind...)
	}

	var fullHouse = Filter(hands, func(h Hand) bool {
		return h.classification == FullHouse
	})

	if len(fullHouse) > 1 {
		fullHouse = subSortHands2(fullHouse)
	}

	if len(fullHouse) > 0 {
		result = append(result, fullHouse...)
	}

	var fourOfAKind = Filter(hands, func(h Hand) bool {
		return h.classification == FourOfAKind
	})

	if len(fourOfAKind) > 1 {
		fourOfAKind = subSortHands2(fourOfAKind)
	}

	if len(fourOfAKind) > 0 {
		result = append(result, fourOfAKind...)
	}

	var fiveOfAKind = Filter(hands, func(h Hand) bool {
		return h.classification == FiveOfAKind
	})

	if len(fiveOfAKind) > 1 {
		fiveOfAKind = subSortHands2(fiveOfAKind)
	}

	if len(fiveOfAKind) > 0 {
		result = append(result, fiveOfAKind...)
	}

	return result
}

func subSortHands2(hands []Hand) []Hand {
	sort.Slice(hands, func(a, b int) bool {

		var handA = hands[a].hand
		var handB = hands[b].hand

		var valA = 0
		var valB = 0

		for i := 0; i < len(handA); i++ {
			if handA[i] == handB[i] {
				continue
			}

			intConvA, _errA := strconv.Atoi(string(handA[i]))
			intConvB, _errB := strconv.Atoi(string(handB[i]))

			if _errA != nil {
				valA = getCardValue2(rune(handA[i]))
			} else {
				valA = intConvA
			}

			if _errB != nil {
				valB = getCardValue2(rune(handB[i]))
			} else {
				valB = intConvB
			}

			break
		}

		return valA < valB
	})

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

	return len(valMap) == 1 || len(valMap) == 0
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

		if jokers == 3 {
			return true
		}
	}

	return false
}

// 22333, 2233J
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

	if twos == 1 && threes == 1 {
		return true
	}

	if twos == 2 && jokers == 1 {
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
		if jokers == 2 {
			return true
		}
	}

	return false
}

// 22334, 22J34
func isTwoPair2(hand string) bool {
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

	// map[2:2 3:2 4:1], map[2:2 3:1 4:1]

	twos := 0
	for _, val := range valMap {
		if val == 2 {
			twos++
		}
	}

	if twos == 2 {
		return true
	}

	if twos == 1 && jokers == 1 {
		return true
	}

	return false
}

// A23A4, A23J4
func isOnePair2(hand string) bool {
	valMap := make(map[rune]int)

	for _, val := range hand {
		if val != 'J' {
			if _, exists := valMap[val]; exists {
				valMap[val] = valMap[val] + 1
			} else {
				valMap[val] = 1
			}
		} else {
			return true
		}
	}

	// map[A:2 2:1 3:1 4:1]

	for _, val := range valMap {
		if val == 2 {
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
	solvePart2Main()
}
