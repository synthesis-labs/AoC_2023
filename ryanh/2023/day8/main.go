package main

import (
	"aoc/helper"
	"strings"
)

func GetSampleInput() []string {
	var data = `RL

	AAA = (BBB, CCC)
	BBB = (DDD, EEE)
	CCC = (ZZZ, GGG)
	DDD = (DDD, DDD)
	EEE = (EEE, EEE)
	GGG = (GGG, GGG)
	ZZZ = (ZZZ, ZZZ)`
	return strings.Split(data, "\n")
}

func GetSampleInput2() []string {
	var data = `LLR

	AAA = (BBB, BBB)
	BBB = (AAA, ZZZ)
	ZZZ = (ZZZ, ZZZ)`

	return strings.Split(data, "\n")
}

func GetMainInput() []string {
	var data = helper.GetFileContents(2023, 8)
	return data
}

type Graph struct {
	graph      map[string][]string
	directions string
}

func AddEdge(g Graph, from, to string) {
	g.graph[from] = append(g.graph[from], to)
}

func main() {
	Part2()
}
