package day16

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

const (
	STATE_RULES = iota
	STATE_YOUR_TICKET
	STATE_NEARBY_TICKETS
)

func Run(lines []string) error {
	rules, yourTicket, nearbyTickets, err := ParseInput(lines)
	if err != nil {
		return err
	}

	fmt.Println("Rule count:", len(rules))

	errorRate := 0
	validTickets := []Ticket{}
	for _, ticket := range nearbyTickets {
		invalidValues := ticket.GetInvalidValues(rules)
		for _, invalidValue := range invalidValues {
			errorRate += invalidValue
		}

		if len(invalidValues) == 0 {
			validTickets = append(validTickets, ticket)
		}
	}

	fmt.Println("Part 1:", errorRate)

	mappings, err := FindRuleMappings(validTickets, rules)
	if err != nil {
		return err
	}
	result := 1
	for key, idx := range mappings {
		if strings.HasPrefix(key, "departure") {
			result *= yourTicket.Values[idx]
		}
	}
	fmt.Println("Part 2:", result)
	return nil
}

func ParseInput(input []string) (map[string]Rule, Ticket, []Ticket, error) {
	rules := make(map[string]Rule)
	var yourTicket Ticket
	var nearbyTickets []Ticket
	state := STATE_RULES

	for _, line := range input {
		switch state {
		case STATE_RULES:
			if len(line) == 0 {
				state = STATE_YOUR_TICKET
			} else {
				rule, err := ParseRule(line)
				if err != nil {
					return map[string]Rule{}, Ticket{}, []Ticket{}, err
				}
				rules[rule.name] = rule
			}
		case STATE_YOUR_TICKET:
			if len(line) == 0 {
				state = STATE_NEARBY_TICKETS
			} else if line != "your ticket:" {
				t, err := ParseTicket(line)
				if err != nil {
					return map[string]Rule{}, Ticket{}, []Ticket{}, err
				}
				yourTicket = t
			}
		case STATE_NEARBY_TICKETS:
			if line != "nearby tickets:" {
				t, err := ParseTicket(line)
				if err != nil {
					return map[string]Rule{}, Ticket{}, []Ticket{}, err
				}
				nearbyTickets = append(nearbyTickets, t)
			}
		}
	}
	return rules, yourTicket, nearbyTickets, nil
}

type Ticket struct {
	Values []int
}

func ParseTicket(line string) (Ticket, error) {
	splat := strings.Split(line, ",")
	vals := make([]int, 0, len(splat))
	for _, s := range splat {
		v, err := strconv.Atoi(s)
		if err != nil {
			return Ticket{}, fmt.Errorf("invalid value '%s'", s)
		}
		vals = append(vals, v)
	}
	return Ticket{vals}, nil
}

func (t *Ticket) GetInvalidValues(rules map[string]Rule) []int {
	invalid := []int{}
	for _, val := range t.Values {
		validForRule := false
		for _, r := range rules {
			if r.Matches(val) {
				validForRule = true
			}
		}
		if !validForRule {
			invalid = append(invalid, val)
		}
	}
	return invalid
}

func ValidRulesAtIndex(tickets []Ticket, index int, rules map[string]Rule) []string {
	validRules := []string{}
	for _, rule := range rules {
		allValid := true
		for _, ticket := range tickets {
			if !rule.Matches(ticket.Values[index]) {
				allValid = false
				break
			}
		}
		if allValid {
			validRules = append(validRules, rule.name)
		}
	}
	return validRules
}

func FindAllValidRules(tickets []Ticket, rules map[string]Rule) [][]string {
	validRules := [][]string{}
	for i := 0; i < len(tickets[0].Values); i++ {
		validRules = append(validRules, ValidRulesAtIndex(tickets, i, rules))
	}
	return validRules
}

func FindRuleMappings(tickets []Ticket, rules map[string]Rule) (map[string]int, error) {
	validRulesByIndex := FindAllValidRules(tickets, rules)
	mappings := map[string]int{}
	for cycle := 0; len(mappings) < len(rules); cycle++ {
		promoted := false
		// Check for any columns with only one valid rule
		for idx, validRules := range validRulesByIndex {
			if len(validRules) == 1 {
				promoted = true
				fmt.Printf("Promoted: '%s' => %d\n", validRules[0], idx)
				mappings[validRules[0]] = idx
			}
		}

		if !promoted {
			return nil, fmt.Errorf("made no progress")
		}

		// Remove the now-mapped rule from the valid set
		newValidRulesByIndex := [][]string{}
		for _, validRules := range validRulesByIndex {
			newValidRules := []string{}
			for _, currentRule := range validRules {
				if _, ok := mappings[currentRule]; !ok {
					newValidRules = append(newValidRules, currentRule)
				}
			}
			newValidRulesByIndex = append(newValidRulesByIndex, newValidRules)
		}
		validRulesByIndex = newValidRulesByIndex
	}
	return mappings, nil
}

type Range struct {
	Min int
	Max int
}

func (r *Range) Includes(num int) bool {
	return num >= r.Min && num <= r.Max
}

func ParseRange(input string) (Range, error) {
	splat := strings.Split(input, "-")
	if len(splat) != 2 {
		return Range{}, fmt.Errorf("invalid range '%s'", input)
	}

	min, err := strconv.Atoi(splat[0])
	if err != nil {
		return Range{}, fmt.Errorf("invalid minimum '%s'", splat[0])
	}
	max, err := strconv.Atoi(splat[1])
	if err != nil {
		return Range{}, fmt.Errorf("invalid maximum '%s'", splat[0])
	}
	return Range{min, max}, nil
}

type Rule struct {
	name   string
	ranges []Range
}

var ruleMatcher = regexp.MustCompile(`^([^:]*): (\d+-\d+) or (\d+-\d+)$`)

func ParseRules(input []string) ([]Rule, error) {
	rules := make([]Rule, 0, len(input))
	for _, line := range input {
		rule, err := ParseRule(line)
		if err != nil {
			return nil, err
		}
		rules = append(rules, rule)
	}
	return rules, nil
}

func ParseRule(input string) (Rule, error) {
	subexprs := ruleMatcher.FindStringSubmatch(input)
	if len(subexprs) != 4 {
		return Rule{}, fmt.Errorf("invalid rule '%s'", input)
	}
	name := strings.TrimSpace(subexprs[1])
	ranges := make([]Range, 0, len(subexprs)-2)
	for _, s := range subexprs[2:] {
		rg, err := ParseRange(s)
		if err != nil {
			return Rule{}, fmt.Errorf("invalid range '%s': %v", s, err)
		}
		ranges = append(ranges, rg)
	}
	return Rule{name, ranges}, nil
}

func (r *Rule) Matches(num int) bool {
	for _, rg := range r.ranges {
		if rg.Includes(num) {
			return true
		}
	}
	return false
}
