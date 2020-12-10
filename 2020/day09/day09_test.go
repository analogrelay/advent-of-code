package day09

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestIsValid(t *testing.T) {
	sequence := make([]int, 25)
	for i := 0; i < 25; i++ {
		sequence[i] = i + 1
	}

	require.True(t, IsValid(sequence, 26))
	require.True(t, IsValid(sequence, 49))
	require.False(t, IsValid(sequence, 100))
	require.False(t, IsValid(sequence, 50))
}

func TestFirstInvalidValue(t *testing.T) {
	sequence := []int{35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576}
	value, err := FirstInvalidValue(sequence, 5)
	require.NoError(t, err)
	require.Equal(t, 127, value)
}

func TestFindContiguousSum(t *testing.T) {
	sequence := []int{35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576}
	start, end, err := FindContiguousSum(sequence, 127)
	require.NoError(t, err)
	require.Equal(t, 2, start)
	require.Equal(t, 5, end)
}

func TestFindWeakness(t *testing.T) {
	sequence := []int{35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576}
	value, err := FindWeakness(sequence, 127)
	require.NoError(t, err)
	require.Equal(t, 62, value)
}
