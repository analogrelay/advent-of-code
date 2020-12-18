package day16

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestRuleParser(t *testing.T) {
	input := []string{
		"class: 1-3 or 5-7",
		"row: 6-11 or 33-44",
		"seat: 13-40 or 45-50",
	}

	rules, err := ParseRules(input)
	require.NoError(t, err)
	require.Equal(t, []Rule{
		{"class", []Range{{1, 3}, {5, 7}}},
		{"row", []Range{{6, 11}, {33, 44}}},
		{"seat", []Range{{13, 40}, {45, 50}}},
	}, rules)
}

func TestTicketParser(t *testing.T) {
	ticket, err := ParseTicket("7,1,14")
	require.NoError(t, err)
	require.Equal(t, []int{7, 1, 14}, ticket.Values)
}

var testRules = map[string]Rule{
	"class": {"class", []Range{{1, 3}, {5, 7}}},
	"row":   {"row", []Range{{6, 11}, {33, 44}}},
	"seat":  {"seat", []Range{{13, 40}, {45, 50}}},
}

var testYourTicket = Ticket{[]int{7, 1, 14}}

var testNearbyTickets = []Ticket{
	{[]int{7, 3, 47}},
	{[]int{40, 4, 50}},
	{[]int{55, 2, 20}},
	{[]int{38, 6, 12}},
}

func TestFullParse(t *testing.T) {
	input := []string{
		"class: 1-3 or 5-7",
		"row: 6-11 or 33-44",
		"seat: 13-40 or 45-50",
		"",
		"your ticket:",
		"7,1,14",
		"",
		"nearby tickets:",
		"7,3,47",
		"40,4,50",
		"55,2,20",
		"38,6,12",
	}

	rules, yourTicket, nearbyTickets, err := ParseInput(input)
	require.NoError(t, err)
	require.Equal(t, testRules, rules)
	require.Equal(t, testYourTicket, yourTicket)
	require.Equal(t, testNearbyTickets, nearbyTickets)
}

func TestFindInvalidTickets(t *testing.T) {
	require.Equal(t, []int{}, testNearbyTickets[0].GetInvalidValues(testRules))
	require.Equal(t, []int{4}, testNearbyTickets[1].GetInvalidValues(testRules))
	require.Equal(t, []int{55}, testNearbyTickets[2].GetInvalidValues(testRules))
	require.Equal(t, []int{12}, testNearbyTickets[3].GetInvalidValues(testRules))
}

func TestValidRulesAtIndex(t *testing.T) {
	testRules := map[string]Rule{
		"class": {"class", []Range{{0, 1}, {4, 16}}},
		"row":   {"row", []Range{{0, 5}, {8, 19}}},
		"seat":  {"seat", []Range{{0, 13}, {16, 19}}},
	}
	testTickets := []Ticket{
		{[]int{3, 9, 18}},
		{[]int{15, 1, 5}},
		{[]int{5, 14, 9}},
	}
	expectedRuleValidity := [][]string{
		{"row"},
		{"class", "row"},
		{"seat", "row"},
	}

	actual := FindAllValidRules(testTickets, testRules)

	for i := range actual {
		require.ElementsMatch(t, expectedRuleValidity[i], actual[i])
	}

	mappings, err := FindRuleMappings(testTickets, testRules)
	require.NoError(t, err)
	require.Equal(t, map[string]int{"row": 0, "class": 1, "seat": 2}, mappings)
}
