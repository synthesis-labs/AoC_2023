package main

import (
	"fmt"
	"sort"

	"golang.org/x/exp/constraints"
)

// Finding elements in slices
func Contains[E comparable](s []E, v E) bool {
	for _, vs := range s {
		if v == vs {
			return true
		}
	}
	return false
}

func TestContains() {
	s := []int{1, 2, 3, 4}
	fmt.Println(Contains(s, 3))
}

type keepFunc[E any] func(E) bool

func Filter[E any](s []E, f keepFunc[E]) []E {
	result := []E{}
	for _, v := range s {
		if f(v) {
			result = append(result, v)
		}
	}
	return result
}

func TestFilter() {
	s := []int{1, 2, 3, 4}
	fmt.Println(Filter(s, func(v int) bool {
		return v%2 == 0
	}))
}

func Reverse[E any](s []E) []E {
	result := make([]E, 0, len(s))
	for i := len(s) - 1; i >= 0; i-- {
		result = append(result, s[i])
	}
	return result
}

func TestReverse() {
	s := []int{1, 2, 3, 4}
	fmt.Println(Reverse(s))
}

func Sort[E constraints.Ordered](s []E) []E {
	result := make([]E, len(s))
	copy(result, s)
	sort.Slice(result, func(i, j int) bool {
		return result[i] < result[j]
	})
	return result
}

func TestSort() {
	s := []int{4, 2, 3, 1}
	fmt.Println(Sort(s))
}

type mapFunc[E any] func(E) E

// Map a function over a slice
func Map[E any](s []E, f mapFunc[E]) []E {
	result := make([]E, len(s))
	for i := range s {
		result[i] = f(s[i])
	}
	return result
}

func TestMap() {
	s := []int{1, 2, 3, 4}
	fmt.Println(Map(s, func(v int) int {
		return v * v
	}))
}

func IsEven[T constraints.Integer](v T) bool {
	return v%2 == 0
}

func TestIsEven() {
	s := []int{1, 2, 3, 4}
	fmt.Println(Filter(s, IsEven[int]))
}

type reduceFunc[E any] func(E, E) E

func Reduce[E any](s []E, init E, f reduceFunc[E]) E {
	cur := init
	for _, v := range s {
		cur = f(cur, v)
	}
	return cur
}

func AddInts(cur, next int) int {
	return cur + next
}

func MultiplyInts(cur, next int) int {
	return cur * next
}

func TestReduce() {
	s := []int{1, 2, 3, 4}

	sum := Reduce(s, 0, AddInts)
	fmt.Println(sum)

	multiply := Reduce(s, 1, MultiplyInts)
	fmt.Println(multiply)

	myString := []string{"a", "b", "c"}
	j := Reduce(myString, "", func(c, n string) string {
		return c + n
	})
	fmt.Println(j)
}

func Max[E constraints.Integer](s []E) E {
	max := s[0]
	for _, v := range s {
		if v > max {
			max = v
		}
	}
	return max
}

func TestMax() {
	s := []int{1, 2, 3, 4}
	fmt.Println(Max(s))
}
