package day13

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

func Run(lines []string) error {
	timestamp, err := strconv.Atoi(lines[0])
	if err != nil {
		return fmt.Errorf("invalid timestamp '%s'", lines[0])
	}
	busIDs, offsets, err := parseBusIDs(lines[1])
	if err != nil {
		return fmt.Errorf("error parsing bus IDs: %v", err)
	}

	id, departure, err := ComputeBestDeparture(timestamp, busIDs)
	if err != nil {
		return err
	}
	waitTime := departure - timestamp
	result := id * waitTime

	fmt.Println("Part 1:", result)

	sequentialDeparture := ComputeSequentialDeparture(busIDs, offsets)
	fmt.Println("Part 2:", sequentialDeparture)

	return nil
}

func parseBusIDs(input string) ([]int, []int, error) {
	splat := strings.Split(input, ",")
	ids := []int{}
	offsets := []int{}
	for idx, idstr := range splat {
		if idstr != "x" {
			id, err := strconv.Atoi(idstr)
			if err != nil {
				return nil, nil, fmt.Errorf("invalid bus ID '%s'", idstr)
			}
			ids = append(ids, id)
			offsets = append(offsets, idx)
		}
	}
	return ids, offsets, nil
}

func ComputeSequentialDeparture(ids []int, offsets []int) int64 {
	incrementor := int64(ids[0])
	timeIndex := int64(incrementor)
	foundMatch := make([]bool, len(offsets))
	foundMatch[0] = true
	remainingIntervals := len(ids) - 1
	for {
		for idx := range ids {
			if (timeIndex+int64(offsets[idx]))%int64(ids[idx]) == 0 {
				if !foundMatch[idx] {
					incrementor *= int64(ids[idx])
					foundMatch[idx] = true
					remainingIntervals -= 1
				}
			}
		}

		if remainingIntervals == 0 {
			if isValidDeparture(timeIndex, ids, offsets) {
				return timeIndex
			} else {
				panic("something went horribly wrong")
			}
		}

		timeIndex += incrementor
	}
}

func isValidDeparture(timeIndex int64, ids []int, offsets []int) bool {
	for idx := range ids {
		if (timeIndex+int64(offsets[idx]))%int64(ids[idx]) != 0 {
			return false
		}
	}
	return true
}

func ComputeBestDeparture(timestamp int, busIDs []int) (int, int, error) {
	bestID := -1
	bestDeparture := -1
	for _, id := range busIDs {
		bestDivisor := int(math.Ceil(float64(timestamp) / float64(id)))
		departure := bestDivisor * id
		if bestDeparture == -1 || departure < bestDeparture {
			bestID = id
			bestDeparture = departure
		}
	}
	return bestID, bestDeparture, nil
}
