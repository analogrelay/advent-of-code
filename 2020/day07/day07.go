package day07

import (
	"fmt"
	"strconv"
	"strings"
)

func Run(lines []string) error {
	ruleSet := ParseRuleSet(lines)

	containers := ruleSet.FindAllContainers("shiny gold")
	fmt.Println("Part 1:", len(containers))
	fmt.Println("Part 2:", ruleSet.GetTotalContentCount("shiny gold")-1)

	return nil
}

type RuleSet map[string]Rule

type Rule struct {
	BagType          string
	RequiredContents []BagContent
}

func (r *Rule) CanDirectlyContain(targetBag string) bool {
	for _, content := range r.RequiredContents {
		if content.BagType == targetBag {
			return true
		}
	}
	return false
}

type BagContent struct {
	Count   int
	BagType string
}

func (r *RuleSet) FindAllContainers(targetBag string) []string {
	var matches []string
	for bagType := range *r {
		if r.CanContain(bagType, targetBag) {
			matches = append(matches, bagType)
		}
	}
	return matches
}

func (r *RuleSet) GetTotalContentCount(outerBag string) int {
	rule := (*r)[outerBag]
	count := 1
	for _, subBag := range rule.RequiredContents {
		subBagContentCount := r.GetTotalContentCount(subBag.BagType)
		count += subBagContentCount * subBag.Count
	}
	return count
}

func (r *RuleSet) CanContain(outerBag, targetBag string) bool {
	rule := (*r)[outerBag]

	if rule.CanDirectlyContain(targetBag) {
		return true
	}

	for _, content := range rule.RequiredContents {
		if r.CanContain(content.BagType, targetBag) {
			return true
		}
	}

	return false
}

func ParseRuleSet(lines []string) RuleSet {
	rules := make(RuleSet)
	for _, line := range lines {
		rule := ParseRule(line)
		rules[rule.BagType] = rule
	}
	return rules
}

func ParseRule(line string) Rule {
	containIdx := strings.Index(line, "contain")
	bagType := cleanBagType(line[:containIdx-1])
	rest := strings.TrimSpace(line[containIdx+7:])

	var requiredContents []BagContent
	if rest != "no other bags." {
		contents := strings.Split(rest, ",")
		for _, content := range contents {
			content = strings.Trim(strings.TrimSpace(content), ".")
			spaceIdx := strings.Index(content, " ")
			number, _ := strconv.Atoi(content[:spaceIdx])
			childBagType := cleanBagType(content[spaceIdx+1:])
			requiredContents = append(requiredContents, BagContent{Count: number, BagType: childBagType})
		}
	}

	return Rule{BagType: bagType, RequiredContents: requiredContents}
}

func cleanBagType(input string) string {
	if strings.HasSuffix(input, " bags") {
		return input[:len(input)-5]
	} else if strings.HasSuffix(input, " bag") {
		return input[:len(input)-4]
	} else {
		return input
	}
}
