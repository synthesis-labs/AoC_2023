package main

import (
	"aoc/helper"
	"strconv"
	"strings"
)

func GetFileContent() []string {
	return helper.GetFileContents(2023, 4)
}

func GetSampleData() []string {
	var input = `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
	Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
	Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
	Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
	Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
	Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`

	var inputArray = strings.Split(input, "\n")
	return inputArray
}

func GetCardNumbers(card string) []int {

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

func GetIntersectingNumbers(winningNumbers []int, playingNumbers []int) []int {

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

func main() {
	Part2()
}
