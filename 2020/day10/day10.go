package day10

import (
	"fmt"
	"sort"
	"strconv"
)

func Run(lines []string) error {
	adapters := make([]int, 0, len(lines))
	for _, line := range lines {
		parsed, err := strconv.Atoi(line)
		if err != nil {
			return err
		}
		adapters = append(adapters, parsed)
	}

	sort.Ints(adapters)
	adapters = append(adapters, adapters[len(adapters)-1]+3)

	one, three := ComputePart1(adapters)
	fmt.Println("Part 1:", one*three)
	fmt.Println("Part 2:", GetPermutationCount(adapters))
	return nil
}

func ComputePart1(adapters []int) (int, int) {
	oneJolts := 0
	threeJolts := 0
	previous := 0
	for _, joltage := range adapters {
		diff := joltage - previous
		if diff == 1 {
			oneJolts++
		} else if diff == 3 {
			threeJolts++
		}
		previous = joltage
	}
	return oneJolts, threeJolts
}

func GetPermutationCount(adapters []int) int {
	ways := make(map[int]int)
	ways[0] = 1
	for _, joltage := range adapters {
		currentWays := 0
		if val, ok := ways[joltage-1]; ok {
			currentWays += val
		}
		if val, ok := ways[joltage-2]; ok {
			currentWays += val
		}
		if val, ok := ways[joltage-3]; ok {
			currentWays += val
		}
		ways[joltage] = currentWays
	}

	return ways[adapters[len(adapters)-1]]
}

func NumberOfWaysForward(current int, adapters []int) int {
	count := 0
	for _, val := range adapters {
		if val-current <= 3 {
			count++
		} else {
			return count
		}
	}
	return count
}
