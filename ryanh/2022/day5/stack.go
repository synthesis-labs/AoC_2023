package main

type Stack []rune

func (s *Stack) Push(v rune) {
	*s = append(*s, v)
}

func (s *Stack) Pop() rune {
	result := (*s)[len(*s)-1]
	*s = (*s)[:len(*s)-1]
	return result
}

func (s *Stack) IsEmpty() bool {
	return len(*s) == 0
}

func (s *Stack) Peek() rune {
	return (*s)[len(*s)-1]
}
