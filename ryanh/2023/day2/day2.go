package main

import (
	"aoc/helper"
	"fmt"
	"strconv"
	"strings"

	"golang.org/x/exp/constraints"
)

func getFileContent() []string {
	return helper.GetFileContents(2023, 2)
}

type Game struct {
	Number int
	Sets   []Set
}

type Set struct {
	Cubes []Cube
}

type Cube struct {
	Color Color
	Count int
}

type Color string

const (
	Blue  Color = "blue"
	Red   Color = "red"
	Green Color = "green"
)

var maxSets = map[Color]int{
	Blue:  14,
	Red:   12,
	Green: 13,
}

func part1Sample() {
	var input = []string{
		"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
		"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
		"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
		"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
		"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
	}
	solvePart1(input)
}

func solvePart1(input []string) {
	var games = getGames(input)
	var sum = 0

	for _, game := range games {

		validGame := true

		if validGame {
			for _, set := range game.Sets {

				if !isValidSet(set) {
					validGame = false
					break
				}
			}
		}

		if validGame {
			sum += game.Number
		}
	}

	fmt.Println(sum)
}

func isValidSet(set Set) bool {
	for _, cube := range set.Cubes {
		if cube.Count > maxSets[cube.Color] {
			return false
		}
	}

	return true
}

func getGames(input []string) []Game {
	var games []Game
	for key, value := range input {
		var game Game
		game.Number = key + 1

		index := strings.Index(value, ":")
		setString := value[index+1:]
		setStrings := strings.Split(setString, ";")

		var sets []Set
		for _, setString := range setStrings {
			var set Set
			set.Cubes = make([]Cube, 0)

			cubeStrings := strings.Split(setString, ",")
			for _, cubeString := range cubeStrings {
				var cube Cube
				cubeString = strings.TrimSpace(cubeString)

				index := strings.Index(cubeString, " ")
				countString := cubeString[:index]

				count, _ := strconv.Atoi(countString)
				cube.Count = count

				switch strings.TrimSpace(cubeString[index+1:]) {
				case "blue":
					cube.Color = Blue
				case "red":
					cube.Color = Red
				case "green":
					cube.Color = Green
				}

				set.Cubes = append(set.Cubes, cube)
			}

			sets = append(sets, set)
		}

		game.Sets = sets
		games = append(games, game)
	}

	return games
}

func part1() {
	var input = getFileContent()
	solvePart1(input) // 1764 too low
}

func part2Sample() {
	var input = []string{
		"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
		"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
		"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
		"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
		"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green",
	}
	games := getGames(input)
	solvePart2(games)
}

func FilterSet[E []Set, F Color](s []Set, c Color) []Cube {
	var result []Cube
	for _, i := range s {
		for _, j := range i.Cubes {
			if j.Color == c {
				result = append(result, j)
			}
		}
	}
	return result
}

func MaxCube[E []Cube](s []Cube) int {
	max := 0
	for _, i := range s {
		if i.Count > max {
			max = i.Count
		}

	}
	return max
}

type reducerFunc[E any] func(E, E) E

func Reducer[E any](s []E, init E, f reducerFunc[E]) E {
	cur := init
	for _, v := range s {
		cur = f(cur, v)
	}
	return cur
}

func Multiplyer[E constraints.Integer](cur, next int) int {
	return cur * next
}

func solvePart2(games []Game) {
	var result int
	for _, game := range games {

		var redSet = FilterSet(game.Sets, Red)
		var maxRed = MaxCube(redSet)

		var greenSet = FilterSet(game.Sets, Green)
		var maxGreen = MaxCube(greenSet)

		var blueSet = FilterSet(game.Sets, Blue)
		var maxBlue = MaxCube(blueSet)

		var power = Reducer([]int{maxRed, maxGreen, maxBlue}, 1, Multiplyer[int])
		fmt.Println(power)

		result += power
	}

	fmt.Println("Result:", result) // 62241
}

func part2() {
	var input = getFileContent()
	var game = getGames(input)
	solvePart2(game)
}

func main() {
	part2()
}
