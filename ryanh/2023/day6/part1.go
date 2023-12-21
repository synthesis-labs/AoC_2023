package main

import (
	"fmt"
)

// Time:      7  15   30
// Distance:  9  40  200

func solveSample() {
	var data = GetSampleData()
	solve(data)
}

func solveMain() {
	var data = GetFileContent()
	solve(data) //
}

func solve(input [][]int) {
	var waysOfWinning = 1

	for i := 0; i < len(input[0]); i++ {
		var timeRecord = input[0][i]
		var distanceRecord = input[1][i]

		// get median time
		var median = timeRecord / 2

		var left = goLeft(timeRecord, distanceRecord, median)
		var right = goRight(timeRecord, distanceRecord, median+1)

		waysOfWinning = waysOfWinning * (left + right)
	}

	fmt.Println("waysOfWinning", waysOfWinning)
}

func goLeft(timeRecord int, distanceRecord int, hold int) int {
	var distanceTraveled = (timeRecord - hold) * hold

	if distanceTraveled <= distanceRecord {
		return 0
	}

	return 1 + goLeft(timeRecord, distanceRecord, hold-1)
}

func goRight(timeRecord int, distanceRecord int, hold int) int {

	var distanceTraveled = (timeRecord - hold) * hold

	if distanceTraveled <= distanceRecord {
		return 0
	}

	return 1 + goRight(timeRecord, distanceRecord, hold+1)
}

func part1() {
	solveMain()
}
