package day07

import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

var testRules = map[string]Rule{
	"light red bags contain 1 bright white bag, 2 muted yellow bags.": {
		BagType: "light red",
		RequiredContents: []BagContent{
			{Count: 1, BagType: "bright white"},
			{Count: 2, BagType: "muted yellow"},
		},
	},
	"dark orange bags contain 3 bright white bags, 4 muted yellow bags.": {
		BagType: "dark orange",
		RequiredContents: []BagContent{
			{Count: 3, BagType: "bright white"},
			{Count: 4, BagType: "muted yellow"},
		},
	},
	"bright white bags contain 1 shiny gold bag.": {
		BagType: "bright white",
		RequiredContents: []BagContent{
			{Count: 1, BagType: "shiny gold"},
		},
	},
	"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.": {
		BagType: "muted yellow",
		RequiredContents: []BagContent{
			{Count: 2, BagType: "shiny gold"},
			{Count: 9, BagType: "faded blue"},
		},
	},
	"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.": {
		BagType: "shiny gold",
		RequiredContents: []BagContent{
			{Count: 1, BagType: "dark olive"},
			{Count: 2, BagType: "vibrant plum"},
		},
	},
	"dark olive bags contain 3 faded blue bags, 4 dotted black bags.": {
		BagType: "dark olive",
		RequiredContents: []BagContent{
			{Count: 3, BagType: "faded blue"},
			{Count: 4, BagType: "dotted black"},
		},
	},
	"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.": {
		BagType: "vibrant plum",
		RequiredContents: []BagContent{
			{Count: 5, BagType: "faded blue"},
			{Count: 6, BagType: "dotted black"},
		},
	},
	"faded blue bags contain no other bags.": {
		BagType: "faded blue",
	},
	"dotted black bags contain no other bags.": {
		BagType: "dotted black",
	},
}

func TestParseRule(t *testing.T) {
	for line, expected := range testRules {
		t.Run(fmt.Sprintf("parse_%s", strings.ReplaceAll(line, " ", "_")), func(t *testing.T) {
			actual := ParseRule(line)
			require.Equal(t, expected, actual)
		})
	}
}

func TestContainment(t *testing.T) {
	ruleSet := make(RuleSet)
	for _, rule := range testRules {
		ruleSet[rule.BagType] = rule
	}

	tests := map[string]bool{
		"light red": true,
		// "dark orange":  true,
		// "bright white": true,
		// "muted yellow": true,
		// "shiny gold":   false,
		// "dark olive":   false,
		// "vibrant plum": false,
		// "faded blue":   false,
		// "dotted black": false,
	}

	for bagType, expected := range tests {
		t.Run(fmt.Sprintf("can_contain_%s", bagType), func(t *testing.T) {
			require.Equal(t, expected, ruleSet.CanContain(bagType, "shiny gold"))
		})
	}
}

func TestContentCount(t *testing.T) {
	ruleSet := make(RuleSet)
	for _, rule := range testRules {
		ruleSet[rule.BagType] = rule
	}

	// We include the outer bag, so this is one higher than in the actual output.
	require.Equal(t, 33, ruleSet.GetTotalContentCount("shiny gold"))
}

func TestContentCount2(t *testing.T) {
	ruleSet := ParseRuleSet([]string{
		"shiny gold bags contain 2 dark red bags.",
		"dark red bags contain 2 dark orange bags.",
		"dark orange bags contain 2 dark yellow bags.",
		"dark yellow bags contain 2 dark green bags.",
		"dark green bags contain 2 dark blue bags.",
		"dark blue bags contain 2 dark violet bags.",
		"dark violet bags contain no other bags.",
	})

	// We include the outer bag, so this is one higher than in the actual output.
	require.Equal(t, 127, ruleSet.GetTotalContentCount("shiny gold"))
}
