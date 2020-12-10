package day10

import (
	"sort"
	"testing"

	"github.com/stretchr/testify/require"
)

var simpleValues = mustCompleteAdapterList([]int{16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4})
var longValues = mustCompleteAdapterList([]int{28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3})

func mustCompleteAdapterList(adapters []int) []int {
	max := 0
	for _, val := range adapters {
		if val > max {
			max = val
		}
	}
	newList := append(adapters, max+3)
	sort.Ints(newList)
	return newList
}

func TestSimplePart1(t *testing.T) {
	one, three := ComputePart1(simpleValues)
	require.Equal(t, 7, one)
	require.Equal(t, 5, three)
}

func TestLongPart1(t *testing.T) {
	one, three := ComputePart1(longValues)
	require.Equal(t, 22, one)
	require.Equal(t, 10, three)
}

func TestSimplePart2(t *testing.T) {
	permuations := GetPermutationCount(simpleValues)
	require.Equal(t, 8, permuations)
}

func TestLongPart2(t *testing.T) {
	permuations := GetPermutationCount(longValues)
	require.Equal(t, 19208, permuations)
}
