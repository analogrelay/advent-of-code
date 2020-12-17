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
	"github.com/anurse/advent-of-code/day11"
	"github.com/anurse/advent-of-code/day12"
	"github.com/anurse/advent-of-code/day13"
	"github.com/anurse/advent-of-code/day14"
	"github.com/anurse/advent-of-code/day15"
	"github.com/anurse/advent-of-code/day16"
	"github.com/anurse/advent-of-code/day17"
	"github.com/anurse/advent-of-code/day18"
	"github.com/anurse/advent-of-code/day19"
	"github.com/anurse/advent-of-code/day20"
	"github.com/anurse/advent-of-code/day21"
	"github.com/anurse/advent-of-code/day22"
	"github.com/anurse/advent-of-code/day23"
	"github.com/anurse/advent-of-code/day24"
	"github.com/anurse/advent-of-code/day25"
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
	advent.RegisterDay(11, advent.WithInputFileAsLines(day11.Run))
	advent.RegisterDay(12, advent.WithInputFileAsLines(day12.Run))
	advent.RegisterDay(13, advent.WithInputFileAsLines(day13.Run))
	advent.RegisterDay(14, advent.WithInputFileAsLines(day14.Run))
	advent.RegisterDay(15, advent.WithInputFileAsLines(day15.Run))
	advent.RegisterDay(16, advent.WithInputFileAsLines(day16.Run))
	advent.RegisterDay(17, advent.WithInputFileAsLines(day17.Run))
	advent.RegisterDay(18, advent.WithInputFileAsLines(day18.Run))
	advent.RegisterDay(19, advent.WithInputFileAsLines(day19.Run))
	advent.RegisterDay(20, advent.WithInputFileAsLines(day20.Run))
	advent.RegisterDay(21, advent.WithInputFileAsLines(day21.Run))
	advent.RegisterDay(22, advent.WithInputFileAsLines(day22.Run))
	advent.RegisterDay(23, advent.WithInputFileAsLines(day23.Run))
	advent.RegisterDay(24, advent.WithInputFileAsLines(day24.Run))
	advent.RegisterDay(25, advent.WithInputFileAsLines(day25.Run))
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
