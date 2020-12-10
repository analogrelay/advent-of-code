package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/anurse/advent-of-code/advent"
	"github.com/anurse/advent-of-code/day01"
	"github.com/anurse/advent-of-code/day02"
	"github.com/anurse/advent-of-code/day03"
	"github.com/anurse/advent-of-code/day04"
	"github.com/anurse/advent-of-code/day05"
	"github.com/anurse/advent-of-code/day06"
	"github.com/anurse/advent-of-code/day07"
	"github.com/anurse/advent-of-code/day08"
	"github.com/anurse/advent-of-code/day09"
	"github.com/anurse/advent-of-code/day10"
)

func init() {
	advent.RegisterDay(1, advent.WithInputFileAsLines(day01.Run))
	advent.RegisterDay(2, advent.WithInputFileAsLines(day02.Run))
	advent.RegisterDay(3, advent.WithInputFileAsLines(day03.Run))
	advent.RegisterDay(4, advent.WithInputFileAsLines(day04.Run))
	advent.RegisterDay(5, advent.WithInputFileAsLines(day05.Run))
	advent.RegisterDay(6, advent.WithInputFileAsLines(day06.Run))
	advent.RegisterDay(7, advent.WithInputFileAsLines(day07.Run))
	advent.RegisterDay(8, advent.WithInputFileAsLines(day08.Run))
	advent.RegisterDay(9, advent.WithInputFileAsLines(day09.Run))
	advent.RegisterDay(10, advent.WithInputFileAsLines(day10.Run))
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s [day number]\n", os.Args[0])
		os.Exit(1)
	}

	day, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Invalid Day %v\n", os.Args[1])
		os.Exit(1)
	}

	if handler, ok := advent.GetDay(day); ok {
		args := os.Args[2:]
		if err = handler(day, args); err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			os.Exit(1)
		}
	} else {
		fmt.Fprintf(os.Stderr, "No handler registered for day %v\n", day)
	}
}
