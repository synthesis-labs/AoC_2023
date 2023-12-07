package main

import (
	"fmt"
)

// Time:      71530
// Distance:  940200

func solvePart2Sample() {
	var data = [][]int{{71530}, {940200}}
	solvePart2(data)
}

func solvePart2Main() {
	// Time:        44806572
	// Distance:   208158110501102
	var data = [][]int{{44806572}, {208158110501102}}
	solvePart2(data)
}

func solvePart2(input [][]int) {
	var waysOfWinning = 1

	for i := 0; i < len(input[0]); i++ {
		var timeRecord = input[0][i]
		var distanceRecord = input[1][i]

		// get median time
		var median = timeRecord / 2

		var left = goLeft2(timeRecord, distanceRecord, median)
		var right = goRight2(timeRecord, distanceRecord, median+1)

		waysOfWinning = waysOfWinning * (left + right)
	}

	fmt.Println("waysOfWinning", waysOfWinning)
}

func goLeft2(timeRecord int, distanceRecord int, hold int) int {
	var distanceTraveled = (timeRecord - hold) * hold

	if distanceTraveled <= distanceRecord {
		return 0
	}

	return 1 + goLeft2(timeRecord, distanceRecord, hold-1)
}

func goRight2(timeRecord int, distanceRecord int, hold int) int {

	var distanceTraveled = (timeRecord - hold) * hold

	if distanceTraveled <= distanceRecord {
		return 0
	}

	return 1 + goRight2(timeRecord, distanceRecord, hold+1)
}

func part2() {
	solvePart2Main()
}
