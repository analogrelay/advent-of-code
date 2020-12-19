package day19

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestParseRule(t *testing.T) {
	lines := []string{
		`0: 4 1 5`,
		`1: 2 3 | 3 2`,
		`2: 4 4 | 5 5`,
		`3: 4 5 | 5 4`,
		`4: "a"`,
		`5: "b"`,
	}

	rules, err := ParseRules(lines)
	require.NoError(t, err)
	require.Equal(t, []RuleExpression{
		&RuleSequence{[]int{4, 1, 5}},
	}, rules[0].expressions)
	require.Equal(t, []RuleExpression{
		&RuleSequence{[]int{2, 3}},
		&RuleSequence{[]int{3, 2}},
	}, rules[1].expressions)
	require.Equal(t, []RuleExpression{
		&RuleSequence{[]int{4, 4}},
		&RuleSequence{[]int{5, 5}},
	}, rules[2].expressions)
	require.Equal(t, []RuleExpression{
		&RuleSequence{[]int{4, 5}},
		&RuleSequence{[]int{5, 4}},
	}, rules[3].expressions)
	require.Equal(t, []RuleExpression{
		&LiteralRule{"a"},
	}, rules[4].expressions)
	require.Equal(t, []RuleExpression{
		&LiteralRule{"b"},
	}, rules[5].expressions)
}

func TestLiteralRule(t *testing.T) {
	runRuleTest(t, "bab", "ab", `0: "b"`)
}

func TestLiteralSequence(t *testing.T) {
	runRuleTest(t, "bac", "c", `0: 1 2`, `1: "b"`, `2: "a"`)
}

func TestAlternate(t *testing.T) {
	runRuleTest(t, "bac", "c", `0: 1 2`, `1: 2 | 3`, `2: "a"`, `3: "b"`)
}

func TestExample1(t *testing.T) {
	lines := []string{
		`0: 1 2`,
		`1: "a"`,
		`2: 1 3 | 3 1`,
		`3: "b"`,
	}

	runRuleTest(t, "aab", "", lines...)
	runRuleTest(t, "aba", "", lines...)

	result, remaining := mustRunRules("baa", lines...)
	require.Equal(t, 0, result)
	require.Equal(t, "baa", remaining)
}

func TestExample2(t *testing.T) {
	lines := []string{
		`0: 4 1 5`,
		`1: 2 3 | 3 2`,
		`2: 4 4 | 5 5`,
		`3: 4 5 | 5 4`,
		`4: "a"`,
		`5: "b"`,
	}

	runRuleTest(t, "aaaabb", "", lines...)
	runRuleTest(t, "aaabab", "", lines...)
	runRuleTest(t, "abbabb", "", lines...)
	runRuleTest(t, "abbbab", "", lines...)
	runRuleTest(t, "aabaab", "", lines...)
	runRuleTest(t, "aabbbb", "", lines...)
	runRuleTest(t, "abaaab", "", lines...)
	runRuleTest(t, "ababbb", "", lines...)
}

func TestRepetitionRule(t *testing.T) {
	lines := []string{
		`0: 1 | 1 0`,
		`1: "a"`,
	}

	runRuleTest(t, "a", "", lines...)
	runRuleTest(t, "aa", "", lines...)
	runRuleTest(t, "aaa", "", lines...)
	runRuleTest(t, "aaaa", "", lines...)
	runRuleTest(t, "aaaaa", "", lines...)
	runRuleTest(t, "aaaaab", "b", lines...)
}

func TestSeparatorRule(t *testing.T) {
	lines := []string{
		`0: 1 2 | 1 0 2`,
		`1: "a"`,
		`2: "b"`,
	}

	runRuleTest(t, "ab", "", lines...)
	runRuleTest(t, "aabb", "", lines...)
	runRuleTest(t, "aaabbb", "", lines...)
	runRuleTest(t, "abaaabbb", "aaabbb", lines...)
}

func TestPart2Example(t *testing.T) {
	lines := []string{
		`42: 9 14 | 10 1`,
		`9: 14 27 | 1 26`,
		`10: 23 14 | 28 1`,
		`1: "a"`,
		`11: 42 31 | 42 11 31`,
		`5: 1 14 | 15 1`,
		`19: 14 1 | 14 14`,
		`12: 24 14 | 19 1`,
		`16: 15 1 | 14 14`,
		`31: 14 17 | 1 13`,
		`6: 14 14 | 1 14`,
		`2: 1 24 | 14 4`,
		`0: 8 11`,
		`13: 14 3 | 1 12`,
		`15: 1 | 14`,
		`17: 14 2 | 1 7`,
		`23: 25 1 | 22 14`,
		`28: 16 1`,
		`4: 1 1`,
		`20: 14 14 | 1 15`,
		`3: 5 14 | 16 1`,
		`27: 1 6 | 14 18`,
		`14: "b"`,
		`21: 14 1 | 1 14`,
		`25: 1 1 | 1 14`,
		`22: 14 14`,
		`8: 42 | 42 8`,
		`26: 14 22 | 1 20`,
		`18: 15 15`,
		`7: 14 5 | 1 21`,
		`24: 14 1`,
	}

	rules, err := ParseRules(lines)
	require.NoError(t, err)

	require.Equal(t, "", MatchPart2(rules, "bbabbbbaabaabba"))
	require.Equal(t, "", MatchPart2(rules, "babbbbaabbbbbabbbbbbaabaaabaaa"))
	require.Equal(t, "", MatchPart2(rules, "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"))
	require.Equal(t, "", MatchPart2(rules, "bbbbbbbaaaabbbbaaabbabaaa"))
}

func runRuleTest(t *testing.T, input string, expectedRemaining string, rules ...string) {
	result, remaining := mustRunRules(input, rules...)
	require.Greater(t, result, 0)
	require.Equal(t, expectedRemaining, remaining)
}

func mustRunRules(input string, rules ...string) (int, string) {
	ruleSet, err := ParseRules(rules)
	if err != nil {
		panic(err)
	}

	rule := ruleSet[0]
	result, remaining := rule.Matches(input, ruleSet)
	return result, remaining
}

func removeMatch(rules map[int]Rule, input string) string {
	rule := rules[0]
	_, remaining := rule.Matches(input, rules)
	return remaining
}
