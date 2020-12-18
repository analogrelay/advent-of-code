package day13

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestPart1(t *testing.T) {
	busIds, remainders, err := parseBusIDs("7,13,x,x,59,x,31,19")
	require.NoError(t, err)
	require.Equal(t, []int{7, 13, 59, 31, 19}, busIds)
	require.Equal(t, []int{0, 1, 4, 6, 7}, remainders)
	id, departure, err := ComputeBestDeparture(939, busIds)
	require.NoError(t, err)
	require.Equal(t, 59, id)
	require.Equal(t, 944, departure)
}

var testCases = []struct {
	sequentialDeparture int64
	ids                 []int
	offsets             []int
}{
	{3417, []int{17, 13, 19}, []int{0, 2, 3}},
	{754018, []int{67, 7, 59, 61}, []int{0, 1, 2, 3}},
	{779210, []int{67, 7, 59, 61}, []int{0, 2, 3, 4}},
	{1261476, []int{67, 7, 59, 61}, []int{0, 1, 3, 4}},
	{1202161486, []int{1789, 37, 47, 1889}, []int{0, 1, 2, 3}},
}

func TestIsValidDeparture(t *testing.T) {
	for _, testCase := range testCases {
		require.True(t, isValidDeparture(testCase.sequentialDeparture, testCase.ids, testCase.offsets))
	}
}

func TestComputeSequentialDeparture(t *testing.T) {
	for _, testCase := range testCases {
		require.Equal(t, testCase.sequentialDeparture, ComputeSequentialDeparture(testCase.ids, testCase.offsets))
	}
}
