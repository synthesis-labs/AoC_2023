package main

func GetFileContent() [][]int {

	// Time:        44     80     65     72
	// Distance:   208   1581   1050   1102

	var data = [][]int{{44, 80, 65, 72}, {208, 1581, 1050, 1102}}

	return data
}

func GetSampleData() [][]int {
	var data = [][]int{{7, 15, 30}, {9, 40, 200}}

	return data
}

func main() {
	part2()
}
