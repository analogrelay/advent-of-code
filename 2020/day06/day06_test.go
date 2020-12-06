package day06

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

var testInput string = `abc

a
b
c

ab
ac

a
a
a
a

b`

func TestParseGroups(t *testing.T) {
	lines := strings.Split(testInput, "\n")
	groups := parseGroups(lines)

	require.Equal(t, []group{
		{answers: []string{"abc"}},
		{answers: []string{"a", "b", "c"}},
		{answers: []string{"ab", "ac"}},
		{answers: []string{"a", "a", "a", "a"}},
		{answers: []string{"b"}},
	}, groups)
}

func TestDistinctAnswers(t *testing.T) {
	lines := strings.Split(testInput, "\n")
	groups := parseGroups(lines)

	require.Equal(t, "abc", groups[0].distinctAnswers())
	require.Equal(t, "abc", groups[1].distinctAnswers())
	require.Equal(t, "abc", groups[2].distinctAnswers())
	require.Equal(t, "a", groups[3].distinctAnswers())
	require.Equal(t, "b", groups[4].distinctAnswers())
}

func TestCommonAnswers(t *testing.T) {
	lines := strings.Split(testInput, "\n")
	groups := parseGroups(lines)

	require.Equal(t, "abc", groups[0].commonAnswers())
	require.Equal(t, "", groups[1].commonAnswers())
	require.Equal(t, "a", groups[2].commonAnswers())
	require.Equal(t, "a", groups[3].commonAnswers())
	require.Equal(t, "b", groups[4].commonAnswers())
}
