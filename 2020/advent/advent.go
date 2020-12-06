package advent

import (
	"bufio"
	"fmt"
	"os"
	"path"
)

type DayHandler = func(int, []string) error

var days = make(map[int]DayHandler)

func RegisterDay(dayNumber int, handler DayHandler) {
	days[dayNumber] = handler
}

func GetDay(dayNumber int) (DayHandler, bool) {
	handler, ok := days[dayNumber]
	return handler, ok
}

func ReadLines(filePath string) ([]string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("error opening file: %v", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var values []string
	for scanner.Scan() {
		values = append(values, scanner.Text())
	}

	return values, nil
}

func findInput(day int) (string, bool) {
	wd, err := os.Getwd()
	if err != nil {
		return "", false
	}
	candidate := path.Join(wd, fmt.Sprintf("day%02d", day), "input.txt")
	if _, err := os.Stat(candidate); err == nil {
		return candidate, true
	}
	return "", false
}

func WithInputFileAsLines(fn func([]string) error) DayHandler {
	return func(day int, args []string) error {
		var inputFile string
		if len(args) < 1 {
			// Try to find the input file
			file, ok := findInput(day)
			if !ok {
				return fmt.Errorf("Usage: aoc2020 [day number] [input file]")
			} else {
				inputFile = file
			}
		} else {
			inputFile = args[0]
		}
		lines, err := ReadLines(inputFile)
		if err != nil {
			return fmt.Errorf("error reading input: %v", err)
		}

		return fn(lines)
	}
}
