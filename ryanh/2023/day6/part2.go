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
	solvePart2(data) // Answer: 34278221
}

// I need a much better solution for this one. My approximations are too wide.

func solvePart2(input [][]int) {
	var waysOfWinning = 0

	for i := 0; i < len(input[0]); i++ {
		var timeRecord = input[0][i]
		var distanceRecord = input[1][i]

		// get median time
		var median = timeRecord / 2

		var closestMinHold = getClosestMinHold(timeRecord, distanceRecord, median, median)
		var closestMaxHold = getClosestMaxHold(timeRecord, distanceRecord, closestMinHold, closestMinHold)
		fmt.Println("closestMinHold", closestMinHold)
		fmt.Println("closestMaxHold", closestMaxHold)

		var left = getMinHold(timeRecord, distanceRecord, closestMinHold, closestMinHold)
		fmt.Println("left", left)

		var right = getMaxHold(timeRecord, distanceRecord, closestMaxHold, closestMaxHold)
		fmt.Println("right", right)

		waysOfWinning = waysOfWinning + (right - left + 1)
	}

	fmt.Println("waysOfWinning", waysOfWinning)
}

func getClosestMinHold(timeRecord int, distanceRecord int, hold int, prevHold int) int {
	var distanceTraveled = (timeRecord - hold) * hold

	if distanceTraveled <= distanceRecord {
		return prevHold
	}

	prevHold = hold
	hold = hold / 2

	return getClosestMinHold(timeRecord, distanceRecord, hold, prevHold)
}

func getClosestMaxHold(timeRecord int, distanceRecord int, hold int, prevHold int) int {
	var distanceTraveled = (timeRecord - hold) * hold

	if distanceTraveled <= distanceRecord {
		return prevHold
	}

	prevHold = hold
	hold = hold * 2

	return getClosestMaxHold(timeRecord, distanceRecord, hold, prevHold)
}

func getMinHold(timeRecord int, distanceRecord int, hold int, prevHold int) int {
	var distanceTraveled = (timeRecord - hold) * hold

	if distanceTraveled <= distanceRecord {
		return prevHold
	}

	prevHold = hold
	hold = hold - 1

	return getMinHold(timeRecord, distanceRecord, hold, prevHold)
}

func getMaxHold(timeRecord int, distanceRecord int, hold int, prevHold int) int {
	var distanceTraveled = (timeRecord - hold) * hold

	var count = 0
	for distanceTraveled > distanceRecord {
		prevHold = hold
		hold = hold + 1

		distanceTraveled = (timeRecord - hold) * hold
		count++
	}

	fmt.Println("count", count)

	return prevHold
}

func part2() {
	solvePart2Main()
}
