package day02

import (
	"fmt"
	"strconv"
	"strings"
)

type passwordRule struct {
	Letter         string
	MinOccurrences int
	MaxOccurrences int
}

func (r *passwordRule) EvaluateLegacyRule(input string) bool {
	actualOccurrences := strings.Count(input, r.Letter)
	return actualOccurrences >= r.MinOccurrences && actualOccurrences <= r.MaxOccurrences
}

func (r *passwordRule) Evaluate(input string) bool {
	// MinOccurrences and MaxOccurrences are actually 1-based POSITIONS
	// Exactly one of these positions must contain the letter provided
	pos1 := input[r.MinOccurrences-1]
	pos2 := input[r.MaxOccurrences-1]

	letter := r.Letter[0]
	return (pos1 == letter && pos2 != letter) ||
		(pos1 != letter && pos2 == letter)
}

type passwordEntry struct {
	Rule     passwordRule
	Password string
}

func Run(lines []string) error {
	legacyValid := 0
	newValid := 0
	for _, line := range lines {
		entry, err := parsePasswordEntry(line)
		if err != nil {
			return fmt.Errorf("error parsing password entry '%s': %v", line, err)
		}

		// Count valid passwords for part 1
		if entry.Rule.EvaluateLegacyRule(entry.Password) {
			legacyValid++
		}
		if entry.Rule.Evaluate(entry.Password) {
			newValid++
		}
	}

	fmt.Println("Part 1:", legacyValid)
	fmt.Println("Part 2:", newValid)

	return nil
}

func parsePasswordEntry(line string) (passwordEntry, error) {
	colonIndex := strings.Index(line, ":")
	if colonIndex < 0 {
		return passwordEntry{}, fmt.Errorf("invalid entry, missing ':'")
	}

	rule, err := parsePasswordRule(line[:colonIndex])
	if err != nil {
		return passwordEntry{}, fmt.Errorf("error parsing rule: %v", err)
	}
	password := line[colonIndex+2:]

	return passwordEntry{Rule: rule, Password: password}, nil
}

func parsePasswordRule(input string) (passwordRule, error) {
	spaceIdx := strings.Index(input, " ")
	if spaceIdx < 0 {
		return passwordRule{}, fmt.Errorf("invalid entry, missing ' '")
	}
	occurrences := input[:spaceIdx]
	character := input[spaceIdx+1 : spaceIdx+2]

	dashIdx := strings.Index(occurrences, "-")
	if dashIdx < 0 {
		return passwordRule{}, fmt.Errorf("invalid entry, missing '-'")
	}
	min, err := strconv.Atoi(occurrences[:dashIdx])
	if err != nil {
		return passwordRule{}, fmt.Errorf("error parsing occurrence count '%s': %v", occurrences[:dashIdx], err)
	}
	max, err := strconv.Atoi(occurrences[dashIdx+1:])
	if err != nil {
		return passwordRule{}, fmt.Errorf("error parsing occurrence count '%s': %v", occurrences[dashIdx+1:], err)
	}

	return passwordRule{
		Letter:         character,
		MinOccurrences: min,
		MaxOccurrences: max,
	}, nil
}
