// Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func read_file() []int {

	var output []int

	file, err := os.Open("input.txt")

	if err != nil {
		fmt.Println("Error reading file")
	}

	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var line = scanner.Text()
		if line == "" {
			output = append(output, -1)
		} else {
			num, err := strconv.Atoi(line)
			if err != nil {
				fmt.Println("Error converting string to int")
				continue
			}
			output = append(output, num)
		}
	}

	return output
}

func solve_sample() {
	var sample = []int{1000, 2000, 3000, -1, 4000, -1, 5000, 6000, -1, 7000, 8000, 9000, -1, 10000}

	var total int = 0
	var max_total int = 0

	for i := 0; i < len(sample); i++ {
		if sample[i] == -1 {
			if total > max_total {
				max_total = total
			}
			total = 0 // reset total
		} else {
			total += sample[i]
		}
	}

	fmt.Println("Total Calories:", max_total)
}

func solve_main(n []int) {
	var total int = 0
	var max_total int = 0

	for i := 0; i < len(n); i++ {
		if n[i] == -1 {
			if total > max_total {
				max_total = total
			}
			total = 0 // reset total
		} else {
			total += n[i]
		}
	}

	fmt.Println("Total Calories:", max_total)
}

func main() {
	//solve_sample()

	var elf_data = read_file()

	solve_main(elf_data)
}
