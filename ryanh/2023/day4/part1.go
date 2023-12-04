package main

import (
	"fmt"
	"strconv"
	"strings"
)

type mapFunc[E any] func(E) E

// Map a function over a slice
func Map[E any](s []E, f mapFunc[E]) []E {
	result := make([]E, len(s))
	for i := range s {
		result[i] = f(s[i])
	}
	return result
}

func solveSample() {
	var input = GetSampleData()
	solve(input)
}

func solveMain() {
	var contents = GetFileContent()
	solve(contents) // 17803
}

func solve(input []string) {

	var sum = 0

	for _, line := range input {

		var cards = strings.Split(line, "|")

		var index = strings.Index(cards[0], ":")
		var card = strings.TrimSpace(cards[0][index+1:])

		var winningNumbers = getCardNumbers(card)
		var playingNumbers = getCardNumbers(cards[1])

		var intersectingNumbers = getIntersectingNumbers(winningNumbers, playingNumbers)

		var points = getPoints(intersectingNumbers)

		sum += points
	}

	fmt.Println(sum)
}

func getCardNumbers(card string) []int {

	result := []int{}

	var numbers = strings.Split(card, " ")
	for _, num := range numbers {
		if strings.TrimSpace(num) != "" {
			number, _ := strconv.Atoi(num)
			result = append(result, number)
		}
	}

	return result
}

func getIntersectingNumbers(winningNumbers []int, playingNumbers []int) []int {

	result := []int{}

	myMap := make(map[int]bool)

	for _, num := range winningNumbers {
		myMap[num] = true
	}

	for _, num := range playingNumbers {
		if myMap[num] {
			result = append(result, num)
		}
	}

	return result
}

func getPoints(numbers []int) int {
	var result = 0

	for i := range numbers {
		if i == 0 {
			result = 1
		} else {
			result = result * 2
		}
	}

	return result
}

func Part1() {
	solveSample()
}
