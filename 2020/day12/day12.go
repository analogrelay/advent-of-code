package day12

import (
	"fmt"
	"math"
	"strconv"
)

func Run(lines []string) error {
	f := NewFerry()

	for _, c := range lines {
		f.MoveNormal(c)
	}

	fmt.Println("Part 1:", f.Position.DistanceFromOrigin())

	f = NewFerry()

	for _, c := range lines {
		f.MoveWaypointed(c)
	}

	fmt.Println("Part 2:", f.Position.DistanceFromOrigin())
	return nil
}

type Ferry struct {
	Position  Point
	Direction string
	Waypoint  Point
	Anchored  bool
}

func NewFerry() Ferry {
	return Ferry{
		Position:  Point{0, 0},
		Direction: "E",
		Waypoint:  Point{10, 1},
	}
}

func parseCommand(command string) (string, int, error) {
	opcode := command[0:1]
	quantity, err := strconv.Atoi(command[1:])
	if err != nil {
		return "", 0, fmt.Errorf("invalid instruction '")
	}
	return opcode, quantity, nil
}

func (f *Ferry) MoveWaypointed(command string) error {
	opcode, quantity, err := parseCommand(command)
	if err != nil {
		return err
	}

	switch opcode {
	case "N":
		f.Waypoint = f.Waypoint.Move(0, quantity)
	case "S":
		f.Waypoint = f.Waypoint.Move(0, -quantity)
	case "W":
		f.Waypoint = f.Waypoint.Move(-quantity, 0)
	case "E":
		f.Waypoint = f.Waypoint.Move(quantity, 0)
	case "L":
		f.Waypoint = f.Waypoint.Rotate(-quantity)
	case "R":
		f.Waypoint = f.Waypoint.Rotate(quantity)
	default:
		f.Position = f.Position.Move(f.Waypoint.X*quantity, f.Waypoint.Y*quantity)
	}

	return nil
}

func (f *Ferry) MoveNormal(command string) error {
	opcode, quantity, err := parseCommand(command)
	if err != nil {
		return err
	}

	// Re-write a "forward" command to a movement in the current direction
	if opcode == "F" {
		opcode = f.Direction
	}

	switch opcode {
	case "N":
		f.Position = f.Position.Move(0, quantity)
	case "S":
		f.Position = f.Position.Move(0, -quantity)
	case "W":
		f.Position = f.Position.Move(-quantity, 0)
	case "E":
		f.Position = f.Position.Move(quantity, 0)
	case "L":
		f.Direction = rotate(f.Direction, -quantity)
	case "R":
		f.Direction = rotate(f.Direction, quantity)
	default:
		return fmt.Errorf("unknown opcode '%s'", opcode)
	}

	return nil
}

type Point struct {
	X int
	Y int
}

func (p Point) Rotate(degrees int) Point {
	if degrees < 0 {
		degrees = 360 + degrees
	}

	steps := degrees / 90

	for i := 0; i < steps; i++ {
		newY := -p.X
		newX := p.Y
		p.Y = newY
		p.X = newX
	}

	return p
}

func (p *Point) DistanceFromOrigin() int {
	return int(math.Abs(float64(p.X)) + math.Abs(float64(p.Y)))
}

func (p Point) Move(x, y int) Point {
	return Point{p.X + x, p.Y + y}
}

var rightRotation = map[string]string{
	"N": "E",
	"E": "S",
	"S": "W",
	"W": "N",
}

func rotate(direction string, quantity int) string {
	if quantity < 0 {
		quantity = 360 + quantity
	}

	steps := quantity / 90

	for i := 0; i < steps; i++ {
		direction = rightRotation[direction]
	}
	return direction
}
