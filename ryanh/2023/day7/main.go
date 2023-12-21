package main

import (
	"aoc/helper"
	"strings"
)

func GetSampleInput() []string {
	var data = `32T3K 765
	T55J5 684
	KK677 28
	KTJJT 220
	QQQJA 483`

	return strings.Split(data, "\n")
}

func GetMainInput() []string {
	var data = helper.GetFileContents(2023, 7)
	return data
}

type Classification int

const (
	HighCard     Classification = 1
	OnePair      Classification = 2
	TwoPair      Classification = 3
	ThreeOfAKind Classification = 4
	FullHouse    Classification = 5
	FourOfAKind  Classification = 6
	FiveOfAKind  Classification = 7
)

type Hand struct {
	hand           string
	bid            int
	classification Classification
}

func main() {
	Part2()
}
