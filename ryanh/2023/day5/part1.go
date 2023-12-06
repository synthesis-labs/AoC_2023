package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"time"
)

func solveSample() {
	var sample = GetSampleData()
	solve(sample)
}

func solveMain() {
	var contents = GetFileContent()
	solve(contents) //
}

func solve(input []string) {

	timer := time.Now()

	seeds := []int64{}
	seedToSoilMap := [][]int64{}
	soilToFertilizerMap := [][]int64{}
	fertilizerToWaterMap := [][]int64{}
	waterToLightMap := [][]int64{}
	lightToTemperatureMap := [][]int64{}
	temperatureToHumidityMap := [][]int64{}
	humidityToLocationMap := [][]int64{}

	for i, line := range input {
		var trimmedLine = strings.TrimSpace(line)
		if trimmedLine != "" {

			if strings.Contains(trimmedLine, "seeds:") {
				seeds = extractSeeds(trimmedLine)
			} else if strings.Contains(trimmedLine, "seed-to-soil map:") {
				seedToSoilMap = extractMapMatrix(i+1, input)
			} else if strings.Contains(trimmedLine, "soil-to-fertilizer map:") {
				soilToFertilizerMap = extractMapMatrix(i+1, input)
			} else if strings.Contains(trimmedLine, "fertilizer-to-water map:") {
				fertilizerToWaterMap = extractMapMatrix(i+1, input)
			} else if strings.Contains(trimmedLine, "water-to-light map:") {
				waterToLightMap = extractMapMatrix(i+1, input)
			} else if strings.Contains(trimmedLine, "light-to-temperature map:") {
				lightToTemperatureMap = extractMapMatrix(i+1, input)
			} else if strings.Contains(trimmedLine, "temperature-to-humidity map:") {
				temperatureToHumidityMap = extractMapMatrix(i+1, input)
			} else if strings.Contains(trimmedLine, "humidity-to-location map:") {
				humidityToLocationMap = extractMapMatrix(i+1, input)
			}

		}
	}

	// fmt.Println("Seeds:", seeds)
	fmt.Println("SeedToSoil:", seedToSoilMap)
	fmt.Println("SoilToFertilizer:", soilToFertilizerMap)
	fmt.Println("FertilizerToWater:", fertilizerToWaterMap)
	fmt.Println("WaterToLight:", waterToLightMap)
	fmt.Println("LightToTemperature:", lightToTemperatureMap)
	fmt.Println("TemperatureToHumidity:", temperatureToHumidityMap)
	fmt.Println("HumidityToLocation:", humidityToLocationMap)

	//getLocationForSeed(79, seedToSoilMap, soilToFertilizerMap, fertilizerToWaterMap, waterToLightMap, lightToTemperatureMap, temperatureToHumidityMap, humidityToLocationMap)
	var min = getMinLocation(seeds, seedToSoilMap, soilToFertilizerMap, fertilizerToWaterMap, waterToLightMap, lightToTemperatureMap, temperatureToHumidityMap, humidityToLocationMap)
	fmt.Println("Min:", min) // Too high: 360216543

	elapsedTime := time.Since(timer)
	fmt.Println("Elapsed time:", elapsedTime)
}

func extractMapMatrix(index int, input []string) [][]int64 {

	var theMap = extractMap(index, input)

	var matrix = extractMatrix(theMap)

	return matrix
}

func extractMap(index int, input []string) [][]int64 {
	var theMap = [][]int64{}

	for i := index; i < len(input); i++ {

		var trimmedLine = strings.TrimSpace(input[i])
		if trimmedLine == "" {
			break
		}

		var numbers = strings.Split(trimmedLine, " ")
		var row = []int64{}

		for _, num := range numbers {
			if strings.TrimSpace(num) != "" {
				number, _ := strconv.ParseInt(num, 10, 64)
				row = append(row, number)
			}
		}

		theMap = append(theMap, row)
	}

	return theMap
}

func getMinLocation(seeds []int64, seedToSoilMap [][]int64, soilToFertilizerMap [][]int64, fertilizerToWaterMap [][]int64, waterToLightMap [][]int64, lightToTemperatureMap [][]int64, temperatureToHumidityMap [][]int64, humidityToLocationMap [][]int64) int64 {

	var minLocation int64 = math.MaxInt64

	for _, seed := range seeds {
		var location = getLocationForSeed(seed, seedToSoilMap, soilToFertilizerMap, fertilizerToWaterMap, waterToLightMap, lightToTemperatureMap, temperatureToHumidityMap, humidityToLocationMap)
		if location < minLocation {
			minLocation = location
		}
	}

	return minLocation
}

func getLocationForSeed(seed int64, seedToSoilMap [][]int64, soilToFertilizerMap [][]int64, fertilizerToWaterMap [][]int64, waterToLightMap [][]int64, lightToTemperatureMap [][]int64, temperatureToHumidityMap [][]int64, humidityToLocationMap [][]int64) int64 {

	// step 1: get soil
	var soil int64 = 0

	for i := 0; i < len(seedToSoilMap); i++ {
		if seed >= seedToSoilMap[i][0] && seed <= seedToSoilMap[i+1][0] {
			var theRange = seedToSoilMap[i][1] - seedToSoilMap[i][0]
			soil = seed + theRange
			break
		}
	}

	// step 2: get fertilizer
	var fertilizer int64 = 0

	for i := 0; i < len(soilToFertilizerMap); i++ {
		if soil >= soilToFertilizerMap[i][0] && soil <= soilToFertilizerMap[i+1][0] {
			var theRange = soilToFertilizerMap[i][1] - soilToFertilizerMap[i][0]
			fertilizer = soil + theRange
			break
		}
	}

	// step 3: get water
	var water int64 = 0

	for i := 0; i < len(fertilizerToWaterMap); i++ {
		if fertilizer >= fertilizerToWaterMap[i][0] && fertilizer <= fertilizerToWaterMap[i+1][0] {
			var theRange = fertilizerToWaterMap[i][1] - fertilizerToWaterMap[i][0]
			water = fertilizer + theRange
			break
		}
	}

	// step 4: get light
	var light int64 = 0

	for i := 0; i < len(waterToLightMap); i++ {
		if water >= waterToLightMap[i][0] && water <= waterToLightMap[i+1][0] {
			var theRange = waterToLightMap[i][1] - waterToLightMap[i][0]
			light = water + theRange
			break
		}
	}

	// step 5: get temperature
	var temperature int64 = 0

	for i := 0; i < len(lightToTemperatureMap); i++ {
		if light >= lightToTemperatureMap[i][0] && light <= lightToTemperatureMap[i+1][0] {
			var theRange = lightToTemperatureMap[i][1] - lightToTemperatureMap[i][0]
			temperature = light + theRange
			break
		}
	}

	// step 6: get humidity
	var humidity int64 = 0

	for i := 0; i < len(temperatureToHumidityMap); i++ {
		if temperature >= temperatureToHumidityMap[i][0] && temperature <= temperatureToHumidityMap[i+1][0] {
			var theRange = temperatureToHumidityMap[i][1] - temperatureToHumidityMap[i][0]
			humidity = temperature + theRange
			break
		}
	}

	// step 7: get location
	var location int64 = 0

	for i := 0; i < len(humidityToLocationMap); i++ {
		if humidity >= humidityToLocationMap[i][0] && humidity <= humidityToLocationMap[i+1][0] {
			var theRange = humidityToLocationMap[i][1] - humidityToLocationMap[i][0]
			location = humidity + theRange
			break
		}
	}

	return location
}

func extractMatrix(theMap [][]int64) [][]int64 {

	var matrix = [][]int64{}

	// [[0, 0] [49, 49], [50, 52], [97, 99]....]

	var min int64 = math.MaxInt64
	var max int64 = -1

	for i := 0; i < len(theMap); i++ {

		var minLink = []int64{}
		var maxLink = []int64{}

		var destination = theMap[i][0]
		var source = theMap[i][1]
		var theRange = theMap[i][2]

		minLink = append(minLink, source)
		minLink = append(minLink, destination)

		maxLink = append(maxLink, source+theRange-1)
		maxLink = append(maxLink, destination+theRange-1)

		matrix = append(matrix, minLink)
		matrix = append(matrix, maxLink)

		if source < min {
			min = source
		}

		if destination < min {
			min = destination
		}

		if source+theRange-1 > max {
			max = source + theRange - 1
		}
		if destination+theRange-1 > max {
			max = destination + theRange - 1
		}

	}

	if min != math.MaxInt64 && min > 0 {
		matrix = append(matrix, []int64{0, 0})
		matrix = append(matrix, []int64{min - 1, min - 1})
	}

	if max >= 0 {
		matrix = append(matrix, []int64{max + 1, max + 1})
		matrix = append(matrix, []int64{math.MaxInt64, math.MaxInt64})
	}

	matrix = orderMatrixBySource(matrix)

	return removeDuplicates(matrix)
}

func removeDuplicates(matrix [][]int64) [][]int64 {

	var result = [][]int64{}

	for i := 0; i < len(matrix); i++ {

		var found = false

		for j := 0; j < len(result); j++ {
			if matrix[i][0] == result[j][0] {
				found = true
				break
			}
		}

		if !found {
			result = append(result, matrix[i])
		}
	}

	return result
}

func orderMatrixBySource(matrix [][]int64) [][]int64 {

	var result = [][]int64{}

	for i := 0; i < len(matrix); i++ {

		var minIndex = i

		for j := i + 1; j < len(matrix); j++ {
			if matrix[j][0] < matrix[minIndex][0] {
				minIndex = j
			}
		}

		var temp = matrix[i]
		matrix[i] = matrix[minIndex]
		matrix[minIndex] = temp

		result = append(result, matrix[i])
	}

	return result
}

func extractSeeds(line string) []int64 {

	seeds := []int64{}

	var index = strings.Index(line, ":")
	var seedStr = strings.TrimSpace(line[index+1:])
	var seedArray = strings.Split(seedStr, " ")

	for _, seed := range seedArray {
		if strings.TrimSpace(seed) != "" {
			var seedInt, _ = strconv.ParseInt(seed, 10, 64)
			seeds = append(seeds, seedInt)
		}
	}

	return seeds
}

func Part1() {
	solveMain()
}
