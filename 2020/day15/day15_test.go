package day15

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestGame(t *testing.T) {
	game := NewGame()
	game.SpeakAll(0, 3, 6)
	require.Equal(t, 3, game.TurnIndex)

	require.Equal(t, 0, game.TakeTurn())
	require.Equal(t, 4, game.TurnIndex)

	require.Equal(t, 3, game.TakeTurn())
	require.Equal(t, 5, game.TurnIndex)

	require.Equal(t, 3, game.TakeTurn())
	require.Equal(t, 6, game.TurnIndex)

	require.Equal(t, 1, game.TakeTurn())
	require.Equal(t, 7, game.TurnIndex)

	require.Equal(t, 0, game.TakeTurn())
	require.Equal(t, 8, game.TurnIndex)

	require.Equal(t, 4, game.TakeTurn())
	require.Equal(t, 9, game.TurnIndex)

	require.Equal(t, 0, game.TakeTurn())
	require.Equal(t, 10, game.TurnIndex)
}

func TestExampleGames(t *testing.T) {
	require.Equal(t, 1, playGame([]int{1, 3, 2}, 2020))
	require.Equal(t, 10, playGame([]int{2, 1, 3}, 2020))
	require.Equal(t, 27, playGame([]int{1, 2, 3}, 2020))
	require.Equal(t, 78, playGame([]int{2, 3, 1}, 2020))
	require.Equal(t, 438, playGame([]int{3, 2, 1}, 2020))
	require.Equal(t, 1836, playGame([]int{3, 1, 2}, 2020))
}

func TestExampleJumboGames(t *testing.T) {
	require.Equal(t, 175594, playGame([]int{0, 3, 6}, 30000000))
}

func playGame(startingNumbers []int, to int) int {
	game := NewGame()
	game.SpeakAll(startingNumbers...)
	game.PlayTo(to)
	return game.LastSpoken
}
