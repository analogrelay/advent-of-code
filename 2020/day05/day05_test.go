package day05

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestBoardingPasses(t *testing.T) {
	tests := map[string]int{
		"FBFBBFFRLR": 357,
		"BFFFBBFRRR": 567,
		"FFFBBBFRRR": 119,
		"BBFFBBFRLL": 820,
	}

	for sequence, expected := range tests {
		t.Run(fmt.Sprint("test", sequence), func(t *testing.T) {
			seatID := getSeatID(sequence)
			require.Equal(t, expected, seatID)
		})
	}
}
