package day19

import (
	"fmt"
	"strconv"
	"strings"
)

func Run(lines []string) error {
	rulesParsed := false
	rules := map[int]Rule{}
	inputs := []string{}
	for _, line := range lines {
		if rulesParsed {
			inputs = append(inputs, line)
		} else if len(line) == 0 {
			rulesParsed = true
		} else {
			rule, err := ParseRule(line)
			if err != nil {
				return fmt.Errorf("error parsing rule '%s': %v", line, err)
			}
			rules[rule.Id] = rule
		}
	}

	matchCount := 0
	rule0 := rules[0]
	for _, input := range inputs {
		result, _ := rule0.Matches(input, rules)
		if result == len(input) {
			matchCount++
		}
	}
	fmt.Println("Part 1:", matchCount)

	matchCount = 0
	for idx, input := range inputs {
		remaining := MatchPart2(rules, input)
		fmt.Println("Processed line", idx)
		if len(remaining) == 0 {
			matchCount++
		}
	}
	fmt.Println("Part 2:", matchCount)

	return nil
}

type Rule struct {
	Id          int
	expressions []RuleExpression
}

func NewRule(id int, expressions ...RuleExpression) Rule {
	return Rule{id, expressions}
}

func (r *Rule) Matches(input string, allRules map[int]Rule) (int, string) {
	bestLength := 0
	bestRemaining := input
	for _, expr := range r.expressions {
		match, remaining := expr.Matches(input, allRules)
		if match > bestLength {
			bestLength = match
			bestRemaining = remaining
		}
	}
	return bestLength, bestRemaining
}

func (r *Rule) Stringify(allRules map[int]Rule) []string {
	results := []string{}
	for _, expr := range r.expressions {
		for _, result := range expr.Stringify(allRules) {
			results = append(results, result)
		}
	}
	return results
}

type RuleExpression interface {
	Matches(input string, allRules map[int]Rule) (int, string)
	Stringify(allRules map[int]Rule) []string
}

type FuncRule func(string, map[int]Rule) (int, string)

func (f FuncRule) Matches(input string, allRules map[int]Rule) (int, string) {
	return f(input, allRules)
}

func (f FuncRule) Stringify(allRules map[int]Rule) []string {
	return []string{fmt.Sprintf("[fn:%v]", f)}
}

type LiteralRule struct {
	value string
}

func (s *LiteralRule) Matches(input string, allRules map[int]Rule) (int, string) {
	if len(s.value) <= len(input) {
		matches := input[:len(s.value)] == s.value
		if matches {
			return len(s.value), input[len(s.value):]
		}
	}
	return 0, input
}

func (s *LiteralRule) Stringify(allRules map[int]Rule) []string {
	return []string{s.value}
}

type RuleSequence struct {
	rules []int
}

func (s *RuleSequence) Matches(input string, allRules map[int]Rule) (int, string) {
	remaining := input
	matchLength := 0
	for _, ruleID := range s.rules {
		rule := allRules[ruleID]

		var match int
		match, remaining = rule.Matches(remaining, allRules)
		if match == 0 {
			return 0, input
		}
		matchLength += match
	}
	return matchLength, remaining
}

func (s *RuleSequence) Stringify(allRules map[int]Rule) []string {
	firstRule := allRules[s.rules[0]]
	results := firstRule.Stringify(allRules)
	for i := 1; i < len(s.rules); i++ {
		newResults := []string{}
		rule := allRules[s.rules[i]]
		for _, outResult := range rule.Stringify(allRules) {
			for _, inResult := range results {
				newResults = append(newResults, fmt.Sprint(inResult, outResult))
			}
		}
		results = newResults
	}
	return results
}

func ParseRules(lines []string) (map[int]Rule, error) {
	rules := map[int]Rule{}
	for _, line := range lines {
		rule, err := ParseRule(line)
		if err != nil {
			return nil, err
		}
		rules[rule.Id] = rule
	}
	return rules, nil
}

func ParseRule(line string) (Rule, error) {
	colonIdx := strings.Index(line, ":")
	if colonIdx < 0 {
		return Rule{}, fmt.Errorf("invalid rule: '%s'", line)
	}
	id, err := strconv.Atoi(line[:colonIdx])
	if err != nil {
		return Rule{}, fmt.Errorf("invalid id: %s", line[:colonIdx])
	}

	exprstrs := strings.Split(line[colonIdx+1:], "|")
	expressions := []RuleExpression{}
	for _, exprstr := range exprstrs {
		exprstr = strings.TrimSpace(exprstr)

		var expr RuleExpression
		if exprstr[0] == '"' {
			expr, err = ParseLiteral(exprstr)
			if err != nil {
				return Rule{}, err
			}
		} else {
			expr, err = ParseRuleSequence(exprstr)
			if err != nil {
				return Rule{}, err
			}
		}
		expressions = append(expressions, expr)
	}

	return Rule{id, expressions}, nil
}

func ParseLiteral(lit string) (*LiteralRule, error) {
	return &LiteralRule{lit[1 : len(lit)-1]}, nil
}

func ParseRuleSequence(seq string) (*RuleSequence, error) {
	ids := []int{}
	for _, idStr := range strings.Split(seq, " ") {
		id, err := strconv.Atoi(idStr)
		if err != nil {
			return nil, fmt.Errorf("invalid id: %s", idStr)
		}
		ids = append(ids, id)
	}
	return &RuleSequence{ids}, nil
}

func MatchPart2(rules map[int]Rule, input string) string {
	rule0 := &Part2Rule{42, 42, 31}
	_, remaining := rule0.Matches(input, rules)
	return remaining
}

type Part2Rule struct {
	repeatRuleID int
	openRuleID   int
	closeRuleID  int
}

func (r *Part2Rule) Stringify(allRules map[int]Rule) []string {
	return []string{"PART2"}
}

func (r *Part2Rule) Matches(input string, allRules map[int]Rule) (int, string) {
	repeatRule := allRules[r.repeatRuleID]
	openRule := allRules[r.openRuleID]
	closeRule := allRules[r.closeRuleID]

	notMaxedOut := true
	for repetitionCount := 1; notMaxedOut; repetitionCount++ {
		fmt.Println("Trying repetition count:", repetitionCount)
		repeatLen, repeatRemaining, didMaxOut := r.tryRepeat(input, allRules, &repeatRule, repetitionCount)
		notMaxedOut = !didMaxOut
		if repeatLen == 0 {
			// Must have matched at least one
			return 0, input
		}

		// Now do the open/close with the remainder
		openCloseLen, openCloseRemaining := r.tryBalance(repeatRemaining, allRules, &openRule, &closeRule)
		if openCloseLen == 0 {
			continue
		}
		if len(openCloseRemaining) == 0 {
			// Found a match!
			return openCloseLen + repeatLen, openCloseRemaining
		}
	}

	// No match at any repeat count
	return 0, input
}

func (r *Part2Rule) tryBalance(input string, allRules map[int]Rule, openRule *Rule, closeRule *Rule) (int, string) {
	openCount := 0
	totalLen := 0
	remaining := input

	// Find opens
	for {
		matchLen, matchRemaining := openRule.Matches(remaining, allRules)
		if matchLen == 0 {
			break
		}
		openCount++
		totalLen += matchLen
		remaining = matchRemaining
	}

	// Balance closes
	for i := 0; i < openCount; i++ {
		matchLen, matchRemaining := closeRule.Matches(remaining, allRules)
		if matchLen == 0 {
			// We failed to do balancing
			return 0, ""
		}
		totalLen += matchLen
		remaining = matchRemaining
	}

	return totalLen, remaining
}

func (r *Part2Rule) tryRepeat(input string, allRules map[int]Rule, repeatRule *Rule, maxRepetitions int) (int, string, bool) {
	remaining := input
	matchLen := 0
	// Run the repeat rule repetitionCount times
	for i := 0; i < maxRepetitions; i++ {
		subMatchLen, newRemaining := repeatRule.Matches(remaining, allRules)
		matchLen += subMatchLen
		if subMatchLen == 0 {
			// stopped because further reptitions are impossible
			return matchLen, newRemaining, true
		}
		remaining = newRemaining
	}

	// stopped because we hit our repetition max
	return matchLen, remaining, false
}
