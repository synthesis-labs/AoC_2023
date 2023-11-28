package main

import (
	"aoc/helper"
	"fmt"
	"strings"
)

func mapFileContents() []string {
	var sections = helper.GetFileContents(2022, 7)
	return sections
}

func part1Samples() {

	var example = `$ cd /
	$ ls
	dir a
	14848514 b.txt
	8504156 c.dat
	dir d
	$ cd a
	$ ls
	dir e
	29116 f
	2557 g
	62596 h.lst
	$ cd e
	$ ls
	584 i
	$ cd ..
	$ cd ..
	$ cd d
	$ ls
	4060174 j
	8033020 d.log
	5626152 d.ext
	7214296 k`

	var contents = strings.Split(example, "\n")
	for _, line := range contents {
		var output = strings.TrimSpace(line)

		if strings.HasPrefix(output, "$") {
			// command
			if strings.Contains(output, "cd /") {
				// reset to root
			} else if strings.Contains(output, "cd ..") {
				// go up one level
			} else if strings.Contains(output, "cd") {
				// go down one level
			} else if strings.Contains(output, "ls") {
				// list contents
			}
		} else if strings.HasPrefix(output, "dir") {
			// directory
		} else {
			// file
		}
	}
}

func part1() {
	var contents = mapFileContents()
	for _, line := range contents {
		fmt.Println(line)
	}
}

func main() {
	part1Samples()
}
