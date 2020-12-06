package day06

import (
	"fmt"
	"strings"
)

func Run(lines []string) error {
	groups := parseGroups(lines)

	count := 0
	for _, group := range groups {
		count += len(group.distinctAnswers())
	}

	fmt.Println("Part 1:", count)

	count = 0
	for _, group := range groups {
		count += len(group.commonAnswers())
	}

	fmt.Println("Part 2:", count)

	return nil
}

type group struct {
	answers []string
}

func parseGroups(input []string) []group {
	var answers []string
	var groups []group
	for _, line := range input {
		if len(line) > 0 {
			answers = append(answers, line)
		} else {
			groups = append(groups, group{answers})
			answers = make([]string, 0)
		}
	}
	if len(answers) > 0 {
		groups = append(groups, group{answers})
	}
	return groups
}

func (g *group) distinctAnswers() string {
	var distinctAnswers string
	for _, answer := range g.answers {
		for _, chr := range answer {
			if !strings.ContainsRune(distinctAnswers, chr) {
				distinctAnswers += string(chr)
			}
		}
	}
	return distinctAnswers
}

var allAnswers string = "abcdefghijklmnopqrstuvwxyz"

func (g *group) commonAnswers() string {
	var commonAnswers string
	for _, answer := range allAnswers {
		if allContain(string(answer), g.answers) {
			commonAnswers += string(answer)
		}
	}
	return commonAnswers
}

func allContain(needle string, haystack []string) bool {
	for _, candidate := range haystack {
		if !strings.Contains(candidate, needle) {
			return false
		}
	}
	return true
}
