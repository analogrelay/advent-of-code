package day17

import (
	"fmt"
	"math"
)

func Run(lines []string) error {
	dimension, err := ParseBoard(lines, false)
	if err != nil {
		return err
	}

	for i := 0; i < 6; i++ {
		dimension = dimension.Step()
	}

	activeCells := dimension.GetActiveCells()
	fmt.Println("Part 1:", len(activeCells))

	dimension, err = ParseBoard(lines, true)
	if err != nil {
		return err
	}

	for i := 0; i < 6; i++ {
		dimension = dimension.Step()
	}

	activeCells = dimension.GetActiveCells()
	fmt.Println("Part 2:", len(activeCells))
	return nil
}

type hyperCubeMap map[int]cubeMap
type cubeMap map[int]planeMap
type planeMap map[int]rowMap
type rowMap map[int]bool
type Point struct{ X, Y, Z, W int }

type Range struct {
	Min int
	Max int
}

var EmptyRange = Range{math.MaxInt32, math.MinInt32}

type PocketDimension struct {
	cubes    hyperCubeMap
	xRange   Range
	yRange   Range
	zRange   Range
	wRange   Range
	includeW bool
}

func NewPocketDimension(includeW bool) PocketDimension {
	wRange := Range{0, 0}
	if includeW {
		wRange = Range{-1, 1}
	}

	return PocketDimension{
		cubes:    make(hyperCubeMap),
		xRange:   Range{-1, 1},
		yRange:   Range{-1, 1},
		zRange:   Range{-1, 1},
		wRange:   wRange,
		includeW: includeW,
	}
}

func ParseBoard(lines []string, includeW bool) (PocketDimension, error) {
	dimension := NewPocketDimension(includeW)
	for y, line := range lines {
		for x, cell := range line {
			switch cell {
			case '.':
				dimension.SetCell(x, y, 0, 0, false)
			case '#':
				dimension.SetCell(x, y, 0, 0, true)
			default:
				return dimension, fmt.Errorf("invalid cell '%v'", cell)
			}
		}
	}
	return dimension, nil
}

func (p *PocketDimension) Step() PocketDimension {
	newDimension := NewPocketDimension(p.includeW)
	p.eachCell(func(x, y, z, w int) {
		val := p.stepCell(x, y, z, w)
		newDimension.SetCell(x, y, z, w, val)
	})
	// fmt.Println("===")
	return newDimension
}
func (p *PocketDimension) GetActiveCells() []Point {
	activeCells := []Point{}
	p.eachCell(func(x, y, z, w int) {
		if p.GetCell(x, y, z, w) {
			activeCells = append(activeCells, Point{x, y, z, w})
		}
	})
	return activeCells
}

func (p *PocketDimension) stepCell(x, y, z, w int) bool {
	current := p.GetCell(x, y, z, w)
	activeNeighbors := 0
	for _, neighbor := range getNeighbors(p.includeW) {
		if p.GetCell(x+neighbor.X, y+neighbor.Y, z+neighbor.Z, w+neighbor.W) {
			activeNeighbors++
		}
	}

	// fmt.Printf("(%d, %d, %d) has %d active neighbors => ", x, y, z, activeNeighbors)
	if activeNeighbors == 3 || (current && activeNeighbors == 2) {
		// fmt.Println("active")
		return true
	}
	// fmt.Println("inactive")
	return false
}

func (p *PocketDimension) eachCell(fn func(x, y, z, w int)) {
	for z := p.zRange.Min; z <= p.zRange.Max; z++ {
		for y := p.yRange.Min; y <= p.yRange.Max; y++ {
			for x := p.xRange.Min; x <= p.xRange.Max; x++ {
				for w := p.wRange.Min; w <= p.wRange.Max; w++ {
					fn(x, y, z, w)
				}
			}
		}
	}
}

func (p *PocketDimension) SetCell(x, y, z, w int, value bool) {
	if !p.includeW && w != 0 {
		panic("tried to set non-zero w in 3d-cube!")
	}

	cube, ok := p.cubes[w]
	if !ok {
		cube = make(cubeMap)
		p.cubes[w] = cube
	}

	plane, ok := cube[z]
	if !ok {
		plane = make(planeMap)
		cube[z] = plane
	}

	row, ok := plane[y]
	if !ok {
		row = make(rowMap)
		plane[y] = row
	}

	row[x] = value
	p.zRange = updateRange(p.zRange, z, 1)
	p.yRange = updateRange(p.yRange, y, 1)
	p.xRange = updateRange(p.xRange, x, 1)

	if p.includeW {
		p.wRange = updateRange(p.wRange, w, 1)
	}
}

func updateRange(rg Range, val int, delta int) Range {
	if val <= rg.Min {
		return Range{Min: val - delta, Max: rg.Max}
	} else if val >= rg.Max {
		return Range{Min: rg.Min, Max: val + delta}
	} else {
		return rg
	}
}

func (p *PocketDimension) GetCell(x, y, z, w int) bool {
	if !p.includeW && w != 0 {
		panic("tried to access non-zero w in 3d cube!")
	}

	if cube, ok := p.cubes[w]; ok {
		if plane, ok := cube[z]; ok {
			if row, ok := plane[y]; ok {
				if cell, ok := row[x]; ok {
					return cell
				}
			}
		}
	}
	return false
}

func getNeighbors(includeW bool) []Point {
	startW, maxW := 0, 0
	if includeW {
		startW, maxW = -1, 1
	}

	result := []Point{}
	for x := -1; x <= 1; x++ {
		for y := -1; y <= 1; y++ {
			for z := -1; z <= 1; z++ {
				for w := startW; w <= maxW; w++ {
					if x != 0 || y != 0 || z != 0 || w != 0 {
						result = append(result, Point{x, y, z, w})
					}
				}
			}
		}
	}
	return result
}
