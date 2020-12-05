package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestPartition(t *testing.T) {
	tests := map[string]struct {
		initial  space
		low      bool
		expected space
	}{
		"lower half": {
			initial:  newSpace(0, 127),
			low:      true,
			expected: newSpace(0, 63),
		},
		"upper half": {
			initial:  newSpace(0, 63),
			low:      false,
			expected: newSpace(32, 63),
		},
		"two numbers - low": {
			initial:  newSpace(44, 45),
			low:      true,
			expected: newSpace(44, 44),
		},
		"two numbers - high": {
			initial:  newSpace(44, 45),
			low:      false,
			expected: newSpace(45, 45),
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			new := tc.initial.partition(tc.low)
			require.Equal(t, tc.expected, new)
		})
	}
}

func TestGetValue(t *testing.T) {
	tests := map[string]struct {
		initial  space
		sequence []bool
		expected int
		err      error
	}{
		"not enough in sequence": {
			initial:  newSpace(0, 127),
			sequence: []bool{true, false},
			expected: 0,
			err:      fmt.Errorf("sequence (32-63) did not result in a single item"),
		},
		"example1rows": {
			initial:  newSpace(0, 127),
			sequence: parseSequence("FBFBBFF"),
			expected: 44,
		},
		"example2rows": {
			initial:  newSpace(0, 127),
			sequence: parseSequence("BFFFBBF"),
			expected: 70,
		},
		"example3rows": {
			initial:  newSpace(0, 127),
			sequence: parseSequence("FFFBBBF"),
			expected: 14,
		},
		"example4rows": {
			initial:  newSpace(0, 127),
			sequence: parseSequence("BBFFBBF"),
			expected: 102,
		},
		"example1seats": {
			initial:  newSpace(0, 7),
			sequence: parseSequence("RLR"),
			expected: 5,
		},
		"example2and3seats": {
			initial:  newSpace(0, 7),
			sequence: parseSequence("RRR"),
			expected: 7,
		},
		"example4seats": {
			initial:  newSpace(0, 7),
			sequence: parseSequence("RLL"),
			expected: 4,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			result, err := tc.initial.getValue(tc.sequence)
			require.Equal(t, tc.err, err)
			require.Equal(t, tc.expected, result)
		})
	}
}

func TestBoardingPasses(t *testing.T) {
	tests := map[string]int{
		"FBFBBFFRLR": 357,
		"BFFFBBFRRR": 567,
		"FFFBBBFRRR": 119,
		"BBFFBBFRLL": 820,
	}

	for sequence, expected := range tests {
		t.Run(fmt.Sprint("test", sequence), func(t *testing.T) {
			seatID, err := getSeatID(sequence)
			require.Nil(t, err)
			require.Equal(t, expected, seatID)
		})
	}
}
