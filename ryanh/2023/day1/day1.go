package main

import (
	"aoc/helper"
	"fmt"
	"strconv"
	"unicode"
)

func getFileContent() []string {
	return helper.GetFileContents(2023, 1)
}

func part1Sample() {
	var input = []string{"1abc2",
		"pqr3stu8vwx",
		"a1b2c3d4e5f",
		"treb7uchet"}

	solvePart1(input)
}

func part1() {
	var input = getFileContent()
	solvePart1(input) // 55488
}

func getDigits(input string) string {
	startIndex := 0
	endIndex := len(input) - 1

	startFound := false
	endFound := false
	start := ""
	end := ""

	for startIndex <= endIndex {

		if !startFound {
			if unicode.IsDigit(rune(input[startIndex])) {
				start = string(input[startIndex])
				startFound = true
			} else {
				startIndex++
			}
		}

		if !endFound {
			if unicode.IsDigit(rune(input[endIndex])) {
				end = string(input[endIndex])
				endFound = true
			} else {
				endIndex--
			}
		}

		if startFound && endFound {
			break
		}
	}

	return start + end
}

func solvePart1(input []string) {
	sum := 0
	for _, line := range input {
		val, _ := strconv.Atoi(getDigits(line))
		sum += val
		fmt.Println(val)
	}
	print(sum)
}

func part2Sample() {
	// var input = []string{
	// 	"two1nine",
	// 	"eightwothree",
	// 	"abcone2threexyz",
	// 	"xtwone3four",
	// 	"4nineeightseven2",
	// 	"zoneight234",
	// 	"7pqrstsixteen"}

	var input = []string{"qbhrhkhcbnsfivenszghnbhsix2ggvv"}

	solvePart2(input)
}

type NamedDigit string

var NamedDigits = map[int]NamedDigit{
	0: "empty",
	1: "one",
	2: "two",
	3: "three",
	4: "four",
	5: "five",
	6: "six",
	7: "seven",
	8: "eight",
	9: "nine",
}

func getNamedNumber(input string) NamedDigit {
	for _, value := range NamedDigits {
		if value == NamedDigit(input) && value != "empty" {
			return value
		}
	}
	return NamedDigit("empty")
}

func recursiveGetNamedNumber(input string, startIndex int, endIndex int, forwardDirection bool) NamedDigit {

	if endIndex-startIndex > 5 {
		return NamedDigit("empty")
	}

	if startIndex >= len(input) || endIndex >= len(input) {
		return NamedDigit("empty")
	}

	if startIndex < 0 || endIndex < 0 {
		return NamedDigit("empty")
	}

	// need a check for a possible actual number within this subset

	if forwardDirection {
		if unicode.IsDigit(rune(input[startIndex])) {
			return getNamedNumber(string(input[startIndex]))
		}
	} else {
		if unicode.IsDigit(rune(input[endIndex])) {
			return getNamedNumber(string(input[endIndex]))
		}
	}

	var namedDigit = getNamedNumber(string(input[startIndex : endIndex+1]))

	if namedDigit != "empty" {
		return namedDigit
	}

	if endIndex-startIndex == 5 {
		if forwardDirection {
			endIndex = startIndex + 2
			startIndex++
		} else {
			endIndex--
			startIndex = endIndex - 2
		}
	} else {
		if forwardDirection {
			endIndex++
		} else {
			startIndex--
		}
	}

	return recursiveGetNamedNumber(input, startIndex, endIndex, forwardDirection)
}

func getNamedDigitKey(namedDigit NamedDigit) string {
	for key, value := range NamedDigits {
		if value == namedDigit {
			return strconv.Itoa(key)
		}
	}
	return string(NamedDigit("empty")) // return 0 if the namedDigit is not found in the map
}

func getDigits2(input string) string {
	startIndex := 0
	startNamedIndex := 0
	startNamedIndexEnd := 2

	endIndex := len(input) - 1
	endNamedIndex := len(input) - 3
	endNamedIndexEnd := len(input) - 1

	startFound := false
	endFound := false
	start := ""
	end := ""

	for startIndex <= endIndex {

		if !startFound {
			if unicode.IsDigit(rune(input[startIndex])) {
				start = string(input[startIndex])
				startFound = true
			} else {
				// check for named digit
				var namedDigit = recursiveGetNamedNumber(input, startNamedIndex, startNamedIndexEnd, true)
				if namedDigit != NamedDigit("empty") {
					start = getNamedDigitKey(namedDigit)
					startFound = true
				} else {
					startNamedIndex++
					startNamedIndexEnd++
				}

				startIndex++
			}
		}

		if !endFound {
			if unicode.IsDigit(rune(input[endIndex])) {
				end = string(input[endIndex])
				endFound = true
			} else {
				// check for named digit
				var namedDigit = recursiveGetNamedNumber(input, endNamedIndex, endNamedIndexEnd, false)
				if namedDigit != NamedDigit("empty") {
					end = getNamedDigitKey(namedDigit)
					endFound = true
				} else {
					endNamedIndex--
					endNamedIndexEnd--
				}

				endIndex--
			}
		}

		if startFound && endFound {
			break
		}
	}

	return start + end
}

func solvePart2(input []string) {
	counter := 0
	sum := 0
	for _, line := range input {
		val, _ := strconv.Atoi(getDigits2(line))
		sum += val
		fmt.Println("Line", counter, "=>", val)
		fmt.Println(line)
		fmt.Println()
		counter++
	}
	fmt.Println("Final sum: ", sum)
}

func part2() {
	var input = getFileContent()
	solvePart2(input)
}

func main() {
	part2()
}
