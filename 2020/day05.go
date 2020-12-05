package main

import (
	"fmt"
	"sort"
)

func init() {
	registerDay(5, day05)
}

func day05(args []string) error {
	if len(args) < 1 {
		return fmt.Errorf("Usage: aoc2020 5 [input file]")
	}
	lines, err := readLines(args[0])
	if err != nil {
		return fmt.Errorf("error reading input: %v", err)
	}

	fmt.Println("Total Boarding Passes:", len(lines))

	maxSeatID := 0
	presentSeats := make([]int, 0, len(lines))
	for _, line := range lines {
		seatID := getSeatID(line)
		if err != nil {
			return fmt.Errorf("error computing seat ID '%s': %v", line, err)
		}
		if seatID > maxSeatID {
			maxSeatID = seatID
		}
		presentSeats = append(presentSeats, seatID)
	}

	fmt.Println("Part 1:", maxSeatID)

	// Sort the list
	sort.Ints(presentSeats)

	// Ignore the first and last entry
	possibleSeats := presentSeats[1 : len(presentSeats)-1]

	// Find a gap
	last := 0
	for _, seat := range possibleSeats {
		if last != 0 && seat != (last+1) {
			fmt.Println("Part 2:", seat-1)
			break
		}
		last = seat
	}

	return nil
}

// Shoulda noticed this first, but meh.
func getSeatID(boardingPass string) int {
	var seatID int
	for _, c := range boardingPass {
		seatID <<= 1
		if c == 'B' || c == 'R' {
			seatID++
		}
	}
	return seatID
}
