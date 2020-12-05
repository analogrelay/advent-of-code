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
		seatID, err := getSeatID(line)
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

type space struct {
	start int
	end   int
}

func newSpace(start, end int) space {
	return space{start, end}
}

func (s *space) partition(low bool) space {
	newLen := (s.end - s.start) / 2
	if low {
		return space{
			start: s.start,
			end:   s.start + newLen,
		}
	} else {
		return space{
			start: s.end - newLen,
			end:   s.end,
		}
	}
}

func (s *space) getValue(sequence []bool) (int, error) {
	current := *s
	for _, low := range sequence {
		current = current.partition(low)
	}

	if current.len() != 1 {
		return 0, fmt.Errorf("sequence (%v-%v) did not result in a single item", current.start, current.end)
	}
	return current.start, nil
}

func (s *space) len() int {
	return s.end - s.start + 1
}

func parseSequence(sequence string) []bool {
	// We just assume you're passing a single-type sequence in here, either F/B or L/R.
	output := make([]bool, 0, len(sequence))
	for _, c := range sequence {
		output = append(output, c == 'F' || c == 'L')
	}
	return output
}

var rowSpace = newSpace(0, 127)
var colSpace = newSpace(0, 7)

func getSeatID(boardingPass string) (int, error) {
	if len(boardingPass) != 10 {
		return 0, fmt.Errorf("boarding pass should be 10 characters")
	}
	row := parseSequence(boardingPass[0:7])
	col := parseSequence(boardingPass[7:])

	rowNumber, err := rowSpace.getValue(row)
	if err != nil {
		return 0, fmt.Errorf("row sequence invalid: %v", err)
	}
	colNumber, err := colSpace.getValue(col)
	if err != nil {
		return 0, fmt.Errorf("col sequence invalid: %v", err)
	}

	return rowNumber*8 + colNumber, nil
}
