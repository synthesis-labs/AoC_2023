package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

func solvePart1Sample() {
	var hands = GetSampleInput()
	part1Solution(hands)
}

func solvePart1Main() {
	var hands = GetMainInput()
	part1Solution(hands) // 253954294
}

func part1Solution(hands []string) {
	var myHands = classifyHands(hands)

	myHands = sortHands(myHands)

	fmt.Println(myHands)

	var total = calculateTotalWinnings(myHands)
	fmt.Println(total)
}

func classifyHands(input []string) []Hand {

	var hands []Hand

	for _, val := range input {
		var hand Hand

		var handStr = strings.TrimSpace(val)
		var handSplit = strings.Split(handStr, " ")

		hand.hand = handSplit[0]
		bid, _ := strconv.Atoi(handSplit[1])
		hand.bid = bid

		if isFiveOfAKind(hand.hand) {
			hand.classification = FiveOfAKind
		} else if isFourOfAKind(hand.hand) {
			hand.classification = FourOfAKind
		} else if isFullHouse(hand.hand) {
			hand.classification = FullHouse
		} else if isThreeOfAKind(hand.hand) {
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

// 23456
func isHighCard(hand string) bool {
	valMap := make(map[rune]bool)
	for _, val := range hand {

		if _, exists := valMap[val]; exists {
			return false
		}

		valMap[val] = true
	}
	return true
}

// A23A4
func isOnePair(hand string) bool {
	valMap := make(map[rune]int)

	for _, val := range hand {
		if _, exists := valMap[val]; exists {
			valMap[val] = valMap[val] + 1
		} else {
			valMap[val] = 1
		}
	}

	// map[A:2 2:1 3:1 4:1]

	if len(valMap) != 4 {
		return false
	}

	count := 0
	for _, val := range valMap {
		if val == 2 {
			count++
		}
	}

	return count == 1
}

// 23432
func isTwoPair(hand string) bool {
	valMap := make(map[rune]int)

	for _, val := range hand {
		if _, exists := valMap[val]; exists {
			valMap[val] = valMap[val] + 1
		} else {
			valMap[val] = 1
		}
	}

	// map[2:2 3:2 4:1]

	if len(valMap) != 3 {
		return false
	}

	count := 0
	for _, val := range valMap {
		if val == 2 {
			count++
		}
	}

	return count == 2
}

// TTT98
func isThreeOfAKind(hand string) bool {
	valMap := make(map[rune]int)

	for _, val := range hand {
		if _, exists := valMap[val]; exists {
			valMap[val] = valMap[val] + 1
		} else {
			valMap[val] = 1
		}
	}

	// map[T:3 9:1, 8:1]

	if len(valMap) != 3 {
		return false
	}

	for _, val := range valMap {
		if val != 3 && val != 1 {
			return false
		}
	}

	return true
}

// 23332
func isFullHouse(hand string) bool {
	valMap := make(map[rune]int)

	for _, val := range hand {
		if _, exists := valMap[val]; exists {
			valMap[val] = valMap[val] + 1
		} else {
			valMap[val] = 1
		}
	}

	// map[2:2 3:3]

	if len(valMap) != 2 {
		return false
	}

	for _, val := range valMap {
		if val != 2 && val != 3 {
			return false
		}
	}

	return true
}

// AA8AA
func isFourOfAKind(hand string) bool {
	valMap := make(map[rune]int)

	for _, val := range hand {
		if _, exists := valMap[val]; exists {
			valMap[val] = valMap[val] + 1
		} else {
			valMap[val] = 1
		}
	}

	// map[A:4 8:1]

	if len(valMap) != 2 {
		return false
	}

	for _, val := range valMap {
		if val != 1 && val != 4 {
			return false
		}
	}

	return true
}

// AAAAA
func isFiveOfAKind(hand string) bool {
	valMap := make(map[rune]bool)

	for _, val := range hand {
		valMap[val] = true
	}

	return len(valMap) == 1
}

func sortHands(hands []Hand) []Hand {

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
		highCard = subSortHands(highCard)
	}

	if len(highCard) > 0 {
		result = append(result, highCard...)
	}

	var onePair = Filter(hands, func(h Hand) bool {
		return h.classification == OnePair
	})

	if len(onePair) > 1 {
		onePair = subSortHands(onePair)
	}

	if len(onePair) > 0 {
		result = append(result, onePair...)
	}

	var twoPairs = Filter(hands, func(h Hand) bool {
		return h.classification == TwoPair
	})

	if len(twoPairs) > 1 {
		twoPairs = subSortHands(twoPairs)
	}

	if len(twoPairs) > 0 {
		result = append(result, twoPairs...)
	}

	var threeOfAKind = Filter(hands, func(h Hand) bool {
		return h.classification == ThreeOfAKind
	})

	if len(threeOfAKind) > 1 {
		threeOfAKind = subSortHands(threeOfAKind)
	}

	if len(threeOfAKind) > 0 {
		result = append(result, threeOfAKind...)
	}

	var fullHouse = Filter(hands, func(h Hand) bool {
		return h.classification == FullHouse
	})

	if len(fullHouse) > 1 {
		fullHouse = subSortHands(fullHouse)
	}

	if len(fullHouse) > 0 {
		result = append(result, fullHouse...)
	}

	var fourOfAKind = Filter(hands, func(h Hand) bool {
		return h.classification == FourOfAKind
	})

	if len(fourOfAKind) > 1 {
		fourOfAKind = subSortHands(fourOfAKind)
	}

	if len(fourOfAKind) > 0 {
		result = append(result, fourOfAKind...)
	}

	var fiveOfAKind = Filter(hands, func(h Hand) bool {
		return h.classification == FiveOfAKind
	})

	if len(fiveOfAKind) > 1 {
		fiveOfAKind = subSortHands(fiveOfAKind)
	}

	if len(fiveOfAKind) > 0 {
		result = append(result, fiveOfAKind...)
	}

	return result
}

func subSortHands(hands []Hand) []Hand {
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
				valA = getCardValue(rune(handA[i]))
			} else {
				valA = intConvA
			}

			if _errB != nil {
				valB = getCardValue(rune(handB[i]))
			} else {
				valB = intConvB
			}

			break
		}

		return valA < valB
	})

	return hands
}

// Selection sort it [O(n^2)]
func selectionSortClassification(hands []Hand) []Hand {

	for i := 0; i < len(hands); i++ {

		min_idx := i
		for j := i + 1; j < len(hands); j++ {

			hand1 := hands[min_idx].hand
			hand2 := hands[j].hand

			var val1 = 0
			var val2 = 0

			for k := 0; k < 5; k++ {
				if hand1[k] == hand2[k] {
					continue
				}

				intConv1, _err1 := strconv.Atoi(string(hand1[k]))
				intConv2, _err2 := strconv.Atoi(string(hand2[k]))

				if _err1 != nil {
					val1 = getCardValue(rune(hand1[k]))
				} else {
					val1 = intConv1
				}

				if _err2 != nil {
					val2 = getCardValue(rune(hand2[k]))
				} else {
					val2 = intConv2
				}

				break
			}

			if val1 > val2 {
				min_idx = j
			}
		}

		hands[i], hands[min_idx] = hands[min_idx], hands[i]
	}

	return hands
}

func calculateTotalWinnings(hands []Hand) int {
	total := 0
	for i, val := range hands {
		var rank = i + 1

		total += val.bid * rank
	}
	return total
}

func getCardValue(card rune) int {
	switch card {
	case 'T':
		return 10
	case 'J':
		return 11
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

type keepFunc[E any] func(E) bool

func Filter[E any](s []E, f keepFunc[E]) []E {
	result := []E{}
	for _, v := range s {
		if f(v) {
			result = append(result, v)
		}
	}
	return result
}

func Part1() {
	solvePart1Main()
}
