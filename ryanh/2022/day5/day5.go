package main

import (
	"aoc/helper"
	"fmt"
	"sort"
	"strconv"
	"strings"
)

func mapFileContents() []string {
	var sections = helper.GetFileContents(2022, 5)
	return sections
}

func part1Sample() {
	stack1 := &Stack{}
	stack2 := &Stack{}
	stack3 := &Stack{}

	stack1.Push('Z')
	stack1.Push('N')

	stack2.Push('M')
	stack2.Push('C')
	stack2.Push('D')

	stack3.Push('P')

	// move 1 from 2 to 1
	stack1.Push(stack2.Pop())

	// move 3 from 1 to 3
	stack3.Push(stack1.Pop())
	stack3.Push(stack1.Pop())
	stack3.Push(stack1.Pop())

	// move 2 from 2 to 1
	stack1.Push(stack2.Pop())
	stack1.Push(stack2.Pop())

	// move 1 from 1 to 2
	stack2.Push(stack1.Pop())

	fmt.Print(string(stack1.Peek()))
	fmt.Print(string(stack2.Peek()))
	fmt.Print(string(stack3.Peek()))
}

func extractCrateIndices(input string, char rune) []int {
	var indices []int
	for i, c := range input {
		if c == char {
			indices = append(indices, i+1)
		}
	}
	return indices
}

func createStackMap(input string) map[int]*Stack {
	var stackMap = make(map[int]*Stack)

	for i := 0; i < len(input); i++ {
		if string(input[i]) != " " {
			value, _ := strconv.Atoi(string(input[i]))
			stackMap[value] = &Stack{}
		}
	}

	return stackMap
}

func extractCrates(input []string) map[int]*Stack {

	var stackIndex = 8

	var stackMap = createStackMap(input[stackIndex])

	for i := len(input) - 2; i >= 0; i-- {

		var indices = extractCrateIndices(input[i], '[')

		for _, index := range indices {

			switch index {
			case 1:
				stackMap[1].Push(rune(input[i][index]))
			case 5:
				stackMap[2].Push(rune(input[i][index]))
			case 9:
				stackMap[3].Push(rune(input[i][index]))
			case 13:
				stackMap[4].Push(rune(input[i][index]))
			case 17:
				stackMap[5].Push(rune(input[i][index]))
			case 21:
				stackMap[6].Push(rune(input[i][index]))
			case 25:
				stackMap[7].Push(rune(input[i][index]))
			case 29:
				stackMap[8].Push(rune(input[i][index]))
			case 33:
				stackMap[9].Push(rune(input[i][index]))
			}
		}
	}

	return stackMap
}

func peekStacks(stacks map[int]*Stack) {
	var keys []int
	for k := range stacks {
		keys = append(keys, k)
	}
	sort.Ints(keys)

	for _, k := range keys {
		fmt.Print(string(stacks[k].Peek()))
	}
}

type Move struct {
	moves int
	from  int
	to    int
}

func extractMoves(input []string) []Move {

	var output []Move

	for _, line := range input {

		var fromIndex = strings.Index(line, "from")

		moves, _ := strconv.Atoi(strings.TrimSpace(line[5:fromIndex]))

		var toIndex = strings.Index(line, "to")

		from, _ := strconv.Atoi(strings.TrimSpace(line[fromIndex+4 : toIndex]))

		to, _ := strconv.Atoi(strings.TrimSpace(line[toIndex+2:]))

		output = append(output, Move{moves: moves, from: from, to: to})
	}

	return output
}

func showMoves(moves []Move) {
	for _, move := range moves {
		fmt.Println("Move", move.moves, "From", move.from, "To", move.to)
	}
}

func solvePart1(stacks map[int]*Stack, moves []Move) {
	for _, move := range moves {
		for i := 0; i < move.moves; i++ {
			stacks[move.to].Push(stacks[move.from].Pop())
		}
	}
}

func part1() {
	var content = mapFileContents()

	// algorithm:
	// build x stacks from the input
	// go through each move line and by line and adjust stacks accordingly
	// do a peek on each of the stacks to get the final answer

	var stacks = extractCrates(content[:9])

	var moves = extractMoves(content[10:])

	solvePart1(stacks, moves)

	// trick here is to sort the keys before printing since maps are unordered
	peekStacks(stacks)
}

func main() {
	part1()
}
