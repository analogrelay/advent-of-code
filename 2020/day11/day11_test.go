package day11

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

var exampleBoard = []string{
	"L.LL.LL.LL",
	"LLLLLLL.LL",
	"L.L.L..L..",
	"LLLL.LL.LL",
	"L.LL.LL.LL",
	"L.LLLLL.LL",
	"..L.L.....",
	"LLLLLLLLLL",
	"L.LLLLLL.L",
	"L.LLLLL.LL",
}

func TestParseBoard(t *testing.T) {
	lines := []string{
		"LLLL.LLLLL",
		"#LLLLLL.LL",
	}
	board := ParseBoard(lines)
	require.Equal(t, 10, board.width)
	require.Equal(t, 2, board.height)
	require.Equal(t, Occupied, board.GetCell(0, 1))
	require.Equal(t, Floor, board.GetCell(4, 0))
	require.Equal(t, Floor, board.GetCell(7, 1))
}

func TestGetAdjacents(t *testing.T) {
	require.Equal(t, []Point{
		{X: 4, Y: 4}, {X: 5, Y: 4}, {X: 6, Y: 4},
		{X: 4, Y: 5}, {X: 6, Y: 5},
		{X: 4, Y: 6}, {X: 5, Y: 6}, {X: 6, Y: 6},
	}, GetAdjacents(5, 5))

	require.Equal(t, []Point{
		{X: 1, Y: 0}, {X: 0, Y: 1}, {X: 1, Y: 1},
	}, GetAdjacents(0, 0))
}

func TestTick(t *testing.T) {
	sequence := []string{
		`#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##`,

		`#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##`,

		`#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##`,

		`#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##`,

		`#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##`,
	}
	board := ParseBoard(exampleBoard)
	var changed bool
	for _, result := range sequence {
		board, changed = board.Tick()
		require.True(t, changed)
		require.Equal(t, result, strings.TrimSpace(board.String()))
	}

	board, changed = board.Tick()
	require.False(t, changed)
	require.Equal(t, sequence[len(sequence)-1], strings.TrimSpace(board.String()))
}

func TestRayCast(t *testing.T) {
	board := ParseBoard([]string{
		".......#.",
		"...#.....",
		".#.......",
		".........",
		"..#L....#",
		"....#....",
		".........",
		"#........",
		"...#....."})
	require.Equal(t, Point{X: 1, Y: 2}, board.Raycast(3, 4, -1, -1))
	require.Equal(t, Point{X: 3, Y: 1}, board.Raycast(3, 4, 0, -1))
	require.Equal(t, Point{X: 7, Y: 0}, board.Raycast(3, 4, 1, -1))
	require.Equal(t, Point{X: 8, Y: 4}, board.Raycast(3, 4, 1, 0))
	require.Equal(t, Point{X: 4, Y: 5}, board.Raycast(3, 4, 1, 1))
	require.Equal(t, Point{X: 3, Y: 8}, board.Raycast(3, 4, 0, 1))
	require.Equal(t, Point{X: 0, Y: 7}, board.Raycast(3, 4, -1, 1))

	board = ParseBoard([]string{
		".............",
		".L.L.#.#.#.#.",
		"............."})
	require.Equal(t, InvalidPoint, board.Raycast(1, 1, -1, -1))
	require.Equal(t, InvalidPoint, board.Raycast(1, 1, 0, -1))
	require.Equal(t, InvalidPoint, board.Raycast(1, 1, 1, -1))
	require.Equal(t, InvalidPoint, board.Raycast(1, 1, 1, 0))
	require.Equal(t, InvalidPoint, board.Raycast(1, 1, 1, 1))
	require.Equal(t, InvalidPoint, board.Raycast(1, 1, 0, 1))
	require.Equal(t, InvalidPoint, board.Raycast(1, 1, -1, 1))
}

func TestTick2(t *testing.T) {
	sequence := []string{
		`#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##`,

		`#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#`,

		`#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#`,

		`#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##LL.LL.L#
L.LL.LL.L#
#.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLL#.L
#.L#LL#.L#`,

		`#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.#L.L#
#.L####.LL
..#.#.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#`,

		`#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.LL.L#
#.LLLL#.LL
..#.L.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#`,
	}

	board := ParseBoard(exampleBoard)
	var changed bool
	for _, result := range sequence {
		board, changed = board.Tick2()
		require.True(t, changed)
		require.Equal(t, result, strings.TrimSpace(board.String()))
	}

	board, changed = board.Tick()
	require.False(t, changed)
	require.Equal(t, sequence[len(sequence)-1], strings.TrimSpace(board.String()))
}
