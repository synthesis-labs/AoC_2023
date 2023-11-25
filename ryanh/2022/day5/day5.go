package main

import (
	"aoc/helper"
	"fmt"
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

	fmt.Println(string(stack1.Peek()))
	fmt.Println(string(stack2.Peek()))
	fmt.Println(string(stack3.Peek()))
}

func part1() {
	var content = mapFileContents()

	fmt.Println(content[1])

	// algorithm:
	// build x stacks from the input
	// go through each move line and by line and adjust stacks accordingly
	// do a peek on each of the stacks to get the final answer
}

func main() {
	part1Sample()
}
