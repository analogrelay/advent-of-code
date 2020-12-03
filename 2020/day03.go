package main

import "fmt"

type board struct {
	terrain []bool
	stride  int
}

func (b *board) render() {
	for i, v := range b.terrain {
		if v {
			fmt.Print("#")
		} else {
			fmt.Print(".")
		}
		if (i+1)%b.stride == 0 {
			fmt.Println()
		}
	}
}

func (b *board) getCell(x, y int) (bool, error) {
	x = x % b.stride
	pos := (y * b.stride) + x
	if pos >= len(b.terrain) {
		return false, fmt.Errorf("position out of range")
	}
	return b.terrain[pos], nil
}

func (b *board) walk(xoff, yoff int) int {
	x, y := xoff, yoff
	count := 0
	tree, err := b.getCell(x, y)
	for err == nil {
		if tree {
			count++
		}
		x += xoff
		y += yoff
		tree, err = b.getCell(x, y)
	}
	return count
}

func init() {
	registerDay(3, day03)
}

func day03(args []string) error {
	if len(args) < 1 {
		return fmt.Errorf("Usage: aoc2020 3 [input file]")
	}
	lines, err := readLines(args[0])
	if err != nil {
		return fmt.Errorf("error reading input: %v", err)
	}

	board := parseBoard(lines)

	// Try all the walks
	allWalks := []int{
		board.walk(1, 1),
		board.walk(3, 1),
		board.walk(5, 1),
		board.walk(7, 1),
		board.walk(1, 2),
	}
	fmt.Println("Part 1:", allWalks[1], "trees")

	product := 1
	for _, count := range allWalks {
		product *= count
	}
	fmt.Println("Part 2:", product, "trees")

	return nil
}

func parseBoard(lines []string) board {
	stride := len(lines[0])
	size := stride * len(lines)
	terrain := make([]bool, 0, size)
	for _, line := range lines {
		for _, chr := range line {
			terrain = append(terrain, chr == '#')
		}
	}
	return board{terrain: terrain, stride: stride}
}
