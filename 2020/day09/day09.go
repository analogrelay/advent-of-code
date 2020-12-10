package day09

import (
	"fmt"
	"strconv"
)

func Run(lines []string) error {
	numbers := make([]int, 0, len(lines))
	for _, line := range lines {
		number, err := strconv.Atoi(line)
		if err != nil {
			return fmt.Errorf("error parsing line: %v", err)
		}
		numbers = append(numbers, number)
	}

	firstInvalid, err := FirstInvalidValue(numbers, 25)
	if err != nil {
		return err
	}
	fmt.Println("Part 1:", firstInvalid)

	weakness, err := FindWeakness(numbers, firstInvalid)
	if err != nil {
		return err
	}
	fmt.Println("Part 2:", weakness)

	return nil
}

func IsValid(sequence []int, value int) bool {
	for i, x := range sequence {
		for j, y := range sequence {
			if i != j && x+y == value {
				return true
			}
		}
	}
	return false
}

func FirstInvalidValue(xmasCode []int, preambleLength int) (int, error) {
	for i := preambleLength; i < len(xmasCode); i++ {
		if !IsValid(xmasCode[i-preambleLength:i], xmasCode[i]) {
			return xmasCode[i], nil
		}
	}
	return 0, fmt.Errorf("there is no invalid value")
}

func FindWeakness(data []int, target int) (int, error) {
	start, end, err := FindContiguousSum(data, target)
	if err != nil {
		return 0, err
	}

	min := data[start]
	max := data[start]
	for _, v := range data[start : end+1] {
		if v > max {
			max = v
		}
		if v < min {
			min = v
		}
	}
	return min + max, nil
}

func FindContiguousSum(data []int, target int) (int, int, error) {
	for start := range data {
		for end := start + 1; end < len(data); end++ {
			if sumRange(data, start, end) == target {
				return start, end, nil
			}
		}
	}
	return 0, 0, fmt.Errorf("no contiguous sum found")
}

func sumRange(data []int, start, end int) int {
	sum := 0
	for _, val := range data[start : end+1] {
		sum += val
	}
	return sum
}
