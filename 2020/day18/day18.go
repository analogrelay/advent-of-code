package day18

import (
	"fmt"
	"strconv"
	"strings"
	"text/scanner"
)

func Run(lines []string) error {
	sum, err := SumAll(lines, EqualPrecedence)
	if err != nil {
		return err
	}
	fmt.Println("Part 1:", sum)
	sum, err = SumAll(lines, AdvancedPrecedence)
	if err != nil {
		return err
	}
	fmt.Println("Part 2:", sum)
	return nil
}

func SumAll(input []string, prec Precedences) (int, error) {
	sum := 0
	for _, line := range input {
		val, err := Evaluate(line, prec)
		if err != nil {
			return 0, err
		}
		sum += val
	}
	return sum, nil
}

func Evaluate(input string, prec Precedences) (int, error) {
	tokens := tokenize(input)
	tokens = postfixize(tokens, prec)
	res, err := evaluatePostfixed(tokens)
	if err != nil {
		return 0, err
	}
	return res, nil
}

func evaluatePostfixed(input []string) (int, error) {
	stack := []int{}
	for _, token := range input {
		switch token {
		case "+", "*":
			if len(stack) < 2 {
				return 0, fmt.Errorf("stack underflow")
			}
			x, y := stack[len(stack)-2], stack[len(stack)-1]
			stack = stack[:len(stack)-2]
			res, err := execute(token, x, y)
			if err != nil {
				return 0, err
			}
			stack = append(stack, res)
		default:
			num, err := strconv.Atoi(token)
			if err != nil {
				return 0, fmt.Errorf("invalid number '%s'", token)
			}
			stack = append(stack, num)
		}
	}

	if len(stack) != 1 {
		return 0, fmt.Errorf("after evaluating %v, stack should have been at 1 item: %v", input, stack)
	}
	return stack[0], nil
}

func execute(op string, x, y int) (int, error) {
	if op == "+" {
		return x + y, nil
	} else if op == "*" {
		return x * y, nil
	} else {
		return 0, fmt.Errorf("unknown operator '%s'", op)
	}
}

func tokenize(input string) []string {
	tokens := []string{}
	s := scanner.Scanner{}
	s.Init(strings.NewReader(input))
	for tok := s.Scan(); tok != scanner.EOF; tok = s.Scan() {
		tokens = append(tokens, s.TokenText())
	}
	return tokens
}

type Precedences map[string]int

var EqualPrecedence = Precedences{"+": 1, "*": 1}
var AdvancedPrecedence = Precedences{"+": 2, "*": 1}

func (p *Precedences) PrecedenceOf(input string) int {
	if prec, ok := (*p)[input]; ok {
		return prec
	}
	return 0
}

func postfixize(input []string, prec Precedences) []string {
	output := []string{}
	operators := []string{}
	for _, token := range input {
		switch token {
		case "(":
			operators = push(operators, token)
		case ")":
			popUntil(&operators, &output, func(x string) bool { return x == "(" })

			// do pop the "("
			var popped string
			operators, popped = pop(operators)
			if popped != "(" {
				panic("parens out of balance")
			}
		case "+", "*":
			if prec.PrecedenceOf(token) > prec.PrecedenceOf(top(operators)) {
				operators = push(operators, token)
			} else {
				for top(operators) != "" && prec.PrecedenceOf(token) <= prec.PrecedenceOf(top(operators)) {
					popTo(&operators, &output)
				}
				operators = push(operators, token)
			}
		default:
			output = push(output, token)
		}
	}

	// Pop remaining operators to the output
	popUntil(&operators, &output, func(_ string) bool { return false })
	return output
}

func popTo(source *[]string, dest *[]string) {
	newSrc, popped := pop(*source)
	*dest = push(*dest, popped)
	*source = newSrc
}

func popUntil(operators *[]string, output *[]string, pred func(string) bool) {
	// Pop operators until '('
	var popped string
	for {
		t := top(*operators)
		if t == "" || pred(t) {
			break
		}
		*operators, popped = pop(*operators)
		*output = push(*output, popped)
	}
}

func push(stack []string, value string) []string {
	return append(stack, value)
}

func pop(stack []string) ([]string, string) {
	if len(stack) == 0 {
		return nil, ""
	}
	return stack[0 : len(stack)-1], stack[len(stack)-1]
}

func top(stack []string) string {
	if len(stack) == 0 {
		return ""
	}
	return stack[len(stack)-1]
}
