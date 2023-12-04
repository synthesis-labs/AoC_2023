package main

import (
	"aoc/helper"
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

type reduceFunc[E any] func(E, E) E

func Reduce[E any](s []E, init E, f reduceFunc[E]) E {
	cur := init
	for _, v := range s {
		cur = f(cur, v)
	}
	return cur
}

func MultiplyInts(cur, next int) int {
	return cur * next
}

func getFileContent() []string {
	return helper.GetFileContents(2023, 3)
}

func getSampleContent() []string {
	var input = `467..114..
	...*......
	..35..633.
	......#...
	617*......
	.....+.58.
	..592.....
	......755.
	...$.*....
	.664.598..`

	var inputArray = strings.Split(input, "\n")
	inputArray = Map(inputArray, strings.TrimSpace)

	return inputArray
}

func getOtherSampleContent() []string {
	var input = `.............*............=.*.......34.941.=.......*............&.12......*....*...........*...20..356...........*......804.*...............
	..........335..........562...258........*..........761.......758...*.....................602................................955........512..
	.........................................882........................730..........................566..............................202.......`

	var inputArray = strings.Split(input, "\n")
	inputArray = Map(inputArray, strings.TrimSpace)

	return inputArray
}

func part1Sample() {
	var inputArray = getOtherSampleContent()
	solvePart1(inputArray)
}

func solvePart1(input []string) {

	var sum = 0

	for rowIndex, row := range input {
		colIndex := 0
		for colIndex < len(row) {
			col := rune(row[colIndex])

			if isDot(col) {
				colIndex++
				continue
			}
			if isSymbol(col) {
				colIndex++
				continue
			}

			if isNumber(col) {
				var number = extractNumber(rowIndex, colIndex, input[rowIndex])
				var numberLength = len(strconv.Itoa(number))

				if isPartNumber(number, rowIndex, colIndex, input) {
					fmt.Println(number)
					sum += number
				}

				colIndex += numberLength
			} else {
				colIndex++
			}
		}
	}

	fmt.Println("Sum: ", sum)
}

func extractNumber(rowIndex int, colIndex int, row string) int {
	var numberString = ""

	for i := colIndex; i < len(row); i++ {
		if isNumber(rune(row[i])) {
			numberString += string(row[i])
		} else {
			break
		}
	}

	number, _ := strconv.Atoi(numberString)

	return number
}

func isPartNumber(number int, rowIndex int, colIndex int, input []string) bool {

	inputLength := len(input)
	if evaluateRow(number, rowIndex, colIndex, input) {
		return true
	}

	// evaluate row up if possible

	if rowIndex > 0 {
		if evaluateRow(number, rowIndex-1, colIndex, input) {
			return true
		}
	}

	// evaluate row down if possible

	if rowIndex+1 < inputLength {
		if evaluateRow(number, rowIndex+1, colIndex, input) {
			return true
		}
	}

	return false
}

func evaluateRow(number int, rowIndex int, colIndex int, input []string) bool {
	numberLength := len(strconv.Itoa(number))

	var row = input[rowIndex]
	var rowLength = len(row)

	firstColIndex := colIndex
	if colIndex > 0 {
		firstColIndex -= 1
	}

	lastColIndex := colIndex + numberLength
	if lastColIndex < rowLength {
		lastColIndex += 1
	}

	for i := firstColIndex; i < lastColIndex; i++ {
		currentChar := rune(row[i])
		if isSymbol(currentChar) {
			return true
		}
	}

	return false
}

func isDot(char rune) bool {
	return char == '.'
}

func isNumber(char rune) bool {
	return char >= '0' && char <= '9'
}

func isSymbol(char rune) bool {
	return !isDot(char) && !isNumber(char)
}

func isGear(char rune) bool {
	return char == '*'
}

func part1() {
	var input = getFileContent()
	solvePart1(input) // 544664
}

func part2Sample() {
	var inputArray = getOtherSampleContent()
	solvePart2(inputArray)
}

func solvePart2(input []string) {

	var sum = 0

	for rowIndex, row := range input {
		colIndex := 0
		for colIndex < len(row) {
			col := rune(row[colIndex])

			if isDot(col) {
				colIndex++
				continue
			}
			if isNumber(col) {
				colIndex++
				continue
			}

			if isGear(col) {
				var result = getPartNumbersForGear(rowIndex, colIndex, input)
				if len(result) == 2 {
					var multiply = Reduce(result, 1, MultiplyInts)

					fmt.Println(rowIndex, colIndex, result)

					sum += multiply
				}
			}

			colIndex++
		}
	}

	fmt.Println("Sum: ", sum)
}

func getPartNumbersForGear(rowIndex int, colIndex int, input []string) []int {

	var partNumbers = []int{}

	// check left
	leftPartNumber := getLeftPartNumber(rowIndex, colIndex, input)

	// check right
	rightPartNumber := getRightPartNumber(rowIndex, colIndex, input)

	var bottomParts = []int{}
	var topParts = []int{}

	// check up
	if rowIndex > 0 {
		topParts = getDiagonalPartNumbers(rowIndex-1, colIndex, input)
	}

	// check down
	if rowIndex < len(input) {
		bottomParts = getDiagonalPartNumbers(rowIndex+1, colIndex, input)
	}

	if leftPartNumber > 0 {
		partNumbers = append(partNumbers, leftPartNumber)
	}
	if rightPartNumber > 0 {
		partNumbers = append(partNumbers, rightPartNumber)
	}

	partNumbers = append(partNumbers, topParts...)
	partNumbers = append(partNumbers, bottomParts...)

	return partNumbers
}

func getDiagonalPartNumbers(rowIndex int, colIndex int, input []string) []int {

	var partNumbers = []int{}

	// check left
	leftPartNumber := getLeftDiagonalPartNumber(rowIndex, colIndex, input)

	// check right
	rightPartNumber := getRightDiagonalRowPartNumber(rowIndex, colIndex, input)

	if leftPartNumber > 0 {
		//fmt.Println("leftPartNumber", leftPartNumber)
		partNumbers = append(partNumbers, leftPartNumber)
	}
	if rightPartNumber > 0 {
		//fmt.Println("rightPartNumber", rightPartNumber)
		partNumbers = append(partNumbers, rightPartNumber)
	}

	return partNumbers
}

func getLeftDiagonalPartNumber(rowIndex int, colIndex int, input []string) int {
	var partNumber = -1

	// move cursor to start of left number
	currentColIndex := colIndex
	if colIndex > 0 {
		if !isNumber(rune(input[rowIndex][currentColIndex])) {
			currentColIndex -= 1
		}
	}

	for currentColIndex > 0 {
		if isNumber(rune(input[rowIndex][currentColIndex])) {
			currentColIndex -= 1
		} else {
			currentColIndex++
			break
		}
	}

	currentNumber := ""
	numberFound := false

	for currentColIndex < len(input[rowIndex]) {
		if isNumber(rune(input[rowIndex][currentColIndex])) {
			currentNumber += string(input[rowIndex][currentColIndex])
			numberFound = true
		} else {
			break
		}
		currentColIndex++
	}

	if numberFound {
		number, _ := strconv.Atoi(currentNumber)
		partNumber = number
	}

	return partNumber
}

func getRightDiagonalRowPartNumber(rowIndex int, colIndex int, input []string) int {
	var partNumber = -1

	currentColIndex := colIndex
	currentNumber := ""
	numberFound := false

	// check if we can safely go right
	if !isNumber(rune(input[rowIndex][currentColIndex])) {
		currentColIndex++

		for currentColIndex < len(input[rowIndex]) {
			if isNumber(rune(input[rowIndex][currentColIndex])) {
				numberFound = true
				currentNumber += string(input[rowIndex][currentColIndex])
				currentColIndex++
			} else {
				break
			}
		}
	}

	if numberFound {
		number, _ := strconv.Atoi(currentNumber)
		partNumber = number
	}

	return partNumber
}

func getLeftPartNumber(rowIndex int, colIndex int, input []string) int {

	var partNumber = -1

	currentColIndex := colIndex

	if currentColIndex > 0 {
		currentColIndex -= 1
	}

	currentNumber := ""
	numberFound := false

	for currentColIndex >= 0 {
		if isNumber(rune(input[rowIndex][currentColIndex])) {
			currentNumber = string(input[rowIndex][currentColIndex]) + currentNumber
			numberFound = true
		} else {
			break
		}
		currentColIndex--
	}

	if numberFound {
		number, _ := strconv.Atoi(currentNumber)
		partNumber = number
	}

	return partNumber
}

func getRightPartNumber(rowIndex int, colIndex int, input []string) int {
	var partNumber = -1

	currentColIndex := colIndex
	if currentColIndex < len(input[rowIndex]) {
		currentColIndex++
	}

	currentNumber := ""
	numberFound := false

	for currentColIndex < len(input[rowIndex]) {
		if isNumber(rune(input[rowIndex][currentColIndex])) {
			currentNumber += string(input[rowIndex][currentColIndex])
			numberFound = true
		} else {
			break
		}
		currentColIndex++
	}

	if numberFound {
		number, _ := strconv.Atoi(currentNumber)
		partNumber = number
	}

	return partNumber
}

func part2() {
	var input = getFileContent()
	solvePart2(input)

	fmt.Println(len(input))
}

func main() {
	part2()
}
