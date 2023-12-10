package main

import (
	"fmt"
	"strings"
)

func solvePart1Sample() {
	var sample = GetSampleInput()
	solvePart1(sample)
}

func solvePart1Sample2() {
	var sample = GetSampleInput2()
	solvePart1(sample)
}

func solvePart1Main() {
	var main = GetMainInput()
	solvePart1(main)
}

func solvePart1(input []string) {

	var directions = ""
	var graph = Graph{graph: make(map[string][]string)}
	var graphStartIndex = 0

	for i, line := range input {
		var trimmedLine = strings.TrimSpace(line)

		if trimmedLine == "" {
			var temp = input[:i]
			directions = strings.TrimSpace(strings.Join(temp, ""))
			graphStartIndex = i
			continue
		}

		if i > graphStartIndex {
			// add graph edge

			var split = strings.Split(trimmedLine, " = ")

			var from = split[0]
			var to = strings.Split(split[1], ", ")

			for _, t := range to {
				t = strings.Replace(t, "(", "", -1)
				t = strings.Replace(t, ")", "", -1)

				AddEdge(graph, from, t)
			}
		}

	}

	var currentPosition = ""
	var currentNodes = graph.graph["AAA"]

	var steps = 0
	var foundEscape = false

	for !foundEscape {
		for _, d := range directions {

			var direction = string(d)

			if direction == "L" {
				currentPosition = string(currentNodes[0])
				fmt.Println("L", currentPosition)
			}

			if direction == "R" {
				currentPosition = string(currentNodes[1])
				fmt.Println("R", currentPosition)
			}

			if currentPosition == "ZZZ" {
				fmt.Println("Found ZZZ")
				foundEscape = true
				steps++
				break
			}

			currentNodes = graph.graph[currentPosition]

			steps++
		}
	}

	fmt.Println("Steps", steps)
}

func Part1() {
	solvePart1Main()
}
