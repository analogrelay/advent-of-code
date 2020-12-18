package day18

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestPushPop(t *testing.T) {
	stack := []string{}
	stack = push(stack, "a")
	stack = push(stack, "b")
	stack = push(stack, "c")
	require.Equal(t, []string{"a", "b", "c"}, stack)

	var res string
	stack, res = pop(stack)
	require.Equal(t, []string{"a", "b"}, stack)
	require.Equal(t, "c", res)

	stack, res = pop(stack)
	require.Equal(t, []string{"a"}, stack)
	require.Equal(t, "b", res)

	stack, res = pop(stack)
	require.Equal(t, []string{}, stack)
	require.Equal(t, "a", res)

	stack, res = pop(stack)
	require.Nil(t, stack)
	require.Equal(t, "", res)
}

func TestTokenize(t *testing.T) {
	input := "((1 + 23) * 42)"
	tokens := tokenize(input)
	require.Equal(t, []string{
		"(", "(", "1", "+", "23", ")", "*", "42", ")",
	}, tokens)
}

func TestPostfixize(t *testing.T) {
	input := []string{
		"(", "(", "1", "+", "23", ")", "*", "42", ")",
	}
	expectedOutput := []string{
		"1", "23", "+", "42", "*",
	}
	require.Equal(t, expectedOutput, postfixize(input, EqualPrecedence))

	input = []string{
		"1", "+", "2", "*", "3", "+", "4", "*", "5", "+", "6",
	}
	expectedOutput = []string{
		"1", "2", "+", "3", "*", "4", "+", "5", "*", "6", "+",
	}
	require.Equal(t, expectedOutput, postfixize(input, EqualPrecedence))
}

func TestEvaluatePostfixed(t *testing.T) {
	tokens := []string{
		"1", "23", "+", "42", "*",
	}
	res, err := evaluatePostfixed(tokens)
	require.NoError(t, err)
	require.Equal(t, 1008, res)
}

func TestEvaluate(t *testing.T) {
	require.Equal(t, 71, mustEvaluate(t, "1 + 2 * 3 + 4 * 5 + 6", EqualPrecedence))
	require.Equal(t, 51, mustEvaluate(t, "1 + (2 * 3) + (4 * (5 + 6))", EqualPrecedence))
	require.Equal(t, 26, mustEvaluate(t, "2 * 3 + (4 * 5)", EqualPrecedence))
	require.Equal(t, 437, mustEvaluate(t, "5 + (8 * 3 + 9 + 3 * 4 * 3)", EqualPrecedence))
	require.Equal(t, 12240, mustEvaluate(t, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", EqualPrecedence))
	require.Equal(t, 13632, mustEvaluate(t, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", EqualPrecedence))
}

func TestEvaluateAdvanced(t *testing.T) {
	require.Equal(t, 231, mustEvaluate(t, "1 + 2 * 3 + 4 * 5 + 6", AdvancedPrecedence))
	require.Equal(t, 51, mustEvaluate(t, "1 + (2 * 3) + (4 * (5 + 6))", AdvancedPrecedence))
	require.Equal(t, 46, mustEvaluate(t, "2 * 3 + (4 * 5)", AdvancedPrecedence))
	require.Equal(t, 1445, mustEvaluate(t, "5 + (8 * 3 + 9 + 3 * 4 * 3)", AdvancedPrecedence))
	require.Equal(t, 669060, mustEvaluate(t, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", AdvancedPrecedence))
	require.Equal(t, 23340, mustEvaluate(t, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", AdvancedPrecedence))
}

func mustEvaluate(t *testing.T, input string, prec Precedences) int {
	res, err := Evaluate(input, prec)
	require.NoError(t, err)
	return res
}
