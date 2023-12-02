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

type Node struct {
	Value       string
	IsDirectory bool
	IsFile      bool
	Children    []*Node
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

	//lsFlag := false
	depth := 1

	previousDirectory := '/'
	currentDirectory := '/'

	root := &Node{Value: fmt.Sprintf("dir %c", currentDirectory), IsDirectory: true, IsFile: false}

	for _, line := range contents {
		var output = strings.TrimSpace(line)

		if strings.HasPrefix(output, "$") {
			// command
			if strings.Contains(output, "cd /") {
				// reset to root
				depth = 1
			} else if strings.Contains(output, "cd ..") {
				// go up one level
				depth--
			} else if strings.Contains(output, "cd") {
				// go down one level
				previousDirectory = currentDirectory
				currentDirectory = rune(output[len(output)-1])
				depth++
				//createDirectoryNode(root, depth, output, currentDirectory)
			} else if strings.Contains(output, "ls") {
				// list contents
				//lsFlag = true
			}
		} else if strings.HasPrefix(output, "dir") {
			// directory
			// return parent node
			createDirectoryNode(root, depth, output, currentDirectory, previousDirectory)
		} else {
			// file
			// send in parent node
			//createNode(root, depth, output, false, true)
		}
	}

	// recurse(root, 1)
	//printTreeRecursive(root, 1)
}

func createDirectoryNode(root *Node, depth int, value string, currentDirectory rune, previousDirectory rune) {

	if depth == 1 {
		var directory = &Node{Value: value, IsDirectory: true, IsFile: false}
		root.Children = append(root.Children, directory)
	}

	if depth == 2 {
		var directory = &Node{Value: value, IsDirectory: true, IsFile: false}
		root.Children = append(root.Children, directory)
	}

	fmt.Println(string(currentDirectory), string(previousDirectory))
}

func printTreeRecursive(root *Node, depth int) {
	for _, child := range root.Children {
		fmt.Println(child.Value, "=>", depth)
		if child.Children != nil {
			printTreeRecursive(child, depth+1)
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
