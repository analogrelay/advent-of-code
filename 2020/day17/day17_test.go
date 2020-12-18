package day17

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestNeighbors(t *testing.T) {
	doTestNeighbors(t, 26, false)
	doTestNeighbors(t, 80, true)
}

func doTestNeighbors(t *testing.T, count int, includeW bool) {
	neighbors := getNeighbors(includeW)
	require.Equal(t, count, len(neighbors))
	for _, cell := range neighbors {
		matches := 0
		for _, other := range neighbors {
			if cell.X == other.X && cell.Y == other.Y && cell.Z == other.Z && cell.W == other.W {
				matches++
			}
		}
		require.Equal(t, 1, matches)
	}
}

func TestStep(t *testing.T) {
	dimension, err := ParseBoard([]string{
		".#.",
		"..#",
		"###",
	}, false)
	require.NoError(t, err)
	require.ElementsMatch(t, []Point{
		{1, 0, 0, 0},
		{2, 1, 0, 0},
		{0, 2, 0, 0},
		{1, 2, 0, 0},
		{2, 2, 0, 0},
	}, dimension.GetActiveCells())

	dimension = dimension.Step()
	activeCells := dimension.GetActiveCells()
	require.Equal(t, 11, len(activeCells))

	dimension = dimension.Step()
	activeCells = dimension.GetActiveCells()
	require.Equal(t, 21, len(activeCells))

	dimension = dimension.Step()
	activeCells = dimension.GetActiveCells()
	require.Equal(t, 38, len(activeCells))

	for i := 0; i < 3; i++ {
		dimension = dimension.Step()
	}
	require.Equal(t, 112, len(dimension.GetActiveCells()))
}
