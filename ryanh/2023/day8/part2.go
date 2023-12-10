package main

import (
	"fmt"
	"strings"

	"golang.org/x/exp/constraints"
)

func part2Sample() []string {
	var data = `LR

	11A = (11B, XXX)
	11B = (XXX, 11Z)
	11Z = (11B, XXX)
	22A = (22B, XXX)
	22B = (22C, 22C)
	22C = (22Z, 22Z)
	22Z = (22B, 22B)
	XXX = (XXX, XXX)`

	var input = strings.Split(data, "\n")
	return input
}

func solvePart2Sample() {
	var sample = part2Sample()
	solvePart2(sample)
}

func solvePart2Main() {
	var main = GetMainInput()
	solvePart2(main)
}

func solvePart2(input []string) {
	var graph = populateGraph(input)
	traverseGraph(graph)
}

func populateGraph(input []string) Graph {
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

	graph.directions = directions

	return graph
}

func traverseGraph(graph Graph) {

	var currentNodes = getStartingNodes(graph)

	fmt.Println("Starting Nodes", currentNodes)

	var steps = []int{}
	var allStepsAligned = false

	for !allStepsAligned {
		//for k, _ := range currentNodes {

		var k = "QXA"

		fmt.Println("Starting Node", k)

		step, currentNode := traverseNode(graph, k)
		fmt.Println("Step", step, "Current Node", currentNode)

		fmt.Println("Next starting node", currentNode)

		step, currentNode = traverseNode(graph, currentNode)
		fmt.Println("Step", step, "Current Node", currentNode)

		fmt.Println("Next starting node", currentNode)

		step, currentNode = traverseNode(graph, currentNode)
		fmt.Println("Step", step, "Current Node", currentNode)

		//}
		fmt.Println("Steps", steps)

		allStepsAligned = true
	}

	// var answer = extrapolateSteps(steps)
	// fmt.Println("Answer", answer)
}

func Min[E constraints.Integer](s []E) E {
	max := s[0]
	for _, v := range s {
		if v < max {
			max = v
		}
	}
	return max
}

func traverseNode(graph Graph, node string) (int, string) {
	var currentPosition = ""
	var currentNodes = graph.graph[node]

	var steps = 0
	var foundEscape = false

	for !foundEscape {
		for _, d := range graph.directions {

			var direction = string(d)

			if direction == "L" {
				currentPosition = string(currentNodes[0])
			}

			if direction == "R" {
				currentPosition = string(currentNodes[1])
			}

			if hasFoundEscapeForNode(currentPosition) {
				foundEscape = true
				steps++
				break
			}

			currentNodes = graph.graph[currentPosition]

			steps++
		}
	}

	return steps, currentPosition
}

func getStartingNodes(graph Graph) map[string][]string {
	var startingNodes = make(map[string][]string)
	for k, v := range graph.graph {
		if string(k[2]) == "A" {
			startingNodes[k] = v
		}
	}
	return startingNodes
}

func getNodesForPositions(graph Graph, positions []string) map[string][]string {
	var nodes = make(map[string][]string)
	for _, p := range positions {
		nodes[p] = graph.graph[p]
	}
	return nodes
}

func hasFoundEscape(nodes map[string][]string) bool {

	for k, _ := range nodes {
		if string(k[2]) != "Z" {
			return false
		}
	}

	return true
}

func hasFoundEscapeForNode(node string) bool {
	if string(node[2]) != "Z" {
		return false
	}
	return true
}

// func hasFoundEscape(positions []string) bool {
// 	for k, v := range positions {

// 	}

// 	return true
// }

func Part2() {
	solvePart2Main()
}
