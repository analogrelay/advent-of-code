package day11

import (
	"fmt"
	"strings"
)

func Run(lines []string) error {
	board := ParseBoard(lines)

	var changed bool
	for {
		board, changed = board.Tick()
		if !changed {
			break
		}
	}

	fmt.Println("Part 1:", board.CountWhere(Occupied))

	board = ParseBoard(lines)
	for {
		board, changed = board.Tick2()
		if !changed {
			break
		}
	}

	fmt.Println("Part 2:", board.CountWhere(Occupied))
	return nil
}

type Cell int

type Point struct {
	X, Y int
}

var InvalidPoint = Point{-1, -1}

const (
	Floor Cell = iota
	Empty
	Occupied
)

type Board struct {
	cells  []Cell
	width  int
	height int
}

func ParseBoard(lines []string) Board {
	width := len(lines[0])
	height := len(lines)
	cells := make([]Cell, 0, width*height)
	for _, line := range lines {
		for _, char := range line {
			switch char {
			case '.':
				cells = append(cells, Floor)
			case 'L':
				cells = append(cells, Empty)
			case '#':
				cells = append(cells, Occupied)
			}
		}
	}
	return Board{cells, width, height}
}

func (b *Board) CountWhere(predicate Cell) int {
	count := 0
	for _, cell := range b.cells {
		if cell == predicate {
			count++
		}
	}
	return count
}

func (b *Board) String() string {
	var builder strings.Builder
	for y := 0; y < b.width; y++ {
		for x := 0; x < b.height; x++ {
			switch b.cells[y*b.width+x] {
			case Occupied:
				builder.WriteString("#")
			case Empty:
				builder.WriteString("L")
			case Floor:
				builder.WriteString(".")
			}
		}
		builder.WriteString("\n")
	}
	return builder.String()
}

func (b *Board) GetCell(x, y int) Cell {
	index := y*b.width + x
	return b.cells[index]
}

func (b *Board) Tick() (Board, bool) {
	cells := make([]Cell, 0, len(b.cells))
	changed := false
	for y := 0; y < b.height; y++ {
		for x := 0; x < b.width; x++ {
			newCell, cellChanged := b.tickCell(x, y)
			changed = changed || cellChanged
			cells = append(cells, newCell)
		}
	}

	return Board{cells, b.width, b.height}, changed
}

func (b *Board) Tick2() (Board, bool) {
	cells := make([]Cell, 0, len(b.cells))
	changed := false
	for y := 0; y < b.height; y++ {
		for x := 0; x < b.width; x++ {
			newCell, cellChanged := b.tickCell2(x, y)
			changed = changed || cellChanged
			cells = append(cells, newCell)
		}
	}

	return Board{cells, b.width, b.height}, changed
}

func (b *Board) tickCell(x, y int) (Cell, bool) {
	occupiedCount := 0
	current := b.GetCell(x, y)
	for _, adjacent := range GetAdjacents(x, y) {
		if b.IsValidAddress(adjacent.X, adjacent.Y) && b.GetCell(adjacent.X, adjacent.Y) == Occupied {
			occupiedCount++
		}
	}

	if current == Empty && occupiedCount == 0 {
		return Occupied, true
	} else if current == Occupied && occupiedCount >= 4 {
		return Empty, true
	} else {
		return current, false
	}
}

func (b *Board) tickCell2(x, y int) (Cell, bool) {
	occupiedCount := 0
	current := b.GetCell(x, y)
	for _, vector := range vectors {
		occupied := b.Raycast(x, y, vector.X, vector.Y)
		if occupied != InvalidPoint {
			occupiedCount++
		}
	}

	if current == Empty && occupiedCount == 0 {
		return Occupied, true
	} else if current == Occupied && occupiedCount >= 5 {
		return Empty, true
	} else {
		return current, false
	}
}

func (b *Board) Raycast(x, y, dx, dy int) Point {
	for {
		x = x + dx
		y = y + dy
		if !b.IsValidAddress(x, y) {
			return InvalidPoint
		}
		cell := b.GetCell(x, y)
		if cell == Occupied {
			return Point{x, y}
		} else if cell != Floor {
			return InvalidPoint
		}
	}
}

func (b *Board) IsValidAddress(x, y int) bool {
	return y < b.height && y >= 0 && x < b.width && x >= 0
}

var vectors = []Point{
	{X: -1, Y: -1}, {X: 0, Y: -1}, {X: 1, Y: -1},
	{X: -1, Y: 0}, {X: 1, Y: 0},
	{X: -1, Y: 1}, {X: 0, Y: 1}, {X: 1, Y: 1},
}

func GetAdjacents(x, y int) []Point {
	var adjacents []Point
	for _, vector := range vectors {
		adjacents = appendIfNonNegative(adjacents, Point{X: x + vector.X, Y: y + vector.Y})
	}
	return adjacents
}

func appendIfNonNegative(slice []Point, newPoint Point) []Point {
	if newPoint.X >= 0 && newPoint.Y >= 0 {
		return append(slice, newPoint)
	}
	return slice
}
