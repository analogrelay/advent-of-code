package day15

import (
	"fmt"
	"strconv"
	"strings"
)

func Run(lines []string) error {
	splat := strings.Split(lines[0], ",")
	numbers := make([]int, 0, len(splat))
	for _, s := range splat {
		num, err := strconv.Atoi(s)
		if err != nil {
			return fmt.Errorf("invalid number '%s'", s)
		}
		numbers = append(numbers, num)
	}

	game := NewGame()
	game.SpeakAll(numbers...)
	lastNumber := game.PlayTo(2020)

	fmt.Println("Part 1:", lastNumber)

	lastNumber = game.PlayTo(30000000)

	fmt.Println("Part 2:", lastNumber)
	return nil
}

type GameState struct {
	LastSpoken    int
	TurnIndex     int
	numbersSpoken map[int][]int
}

func NewGame() GameState {
	return GameState{LastSpoken: 0, TurnIndex: 0, numbersSpoken: make(map[int][]int)}
}

func (g *GameState) PlayTo(index int) int {
	for {
		number := g.TakeTurn()
		if g.TurnIndex == index {
			return number
		}
	}
}

// TakeTurn takes a game turn and returns the number spoken in it
func (g *GameState) TakeTurn() int {
	if spokenAt, ok := g.numbersSpoken[g.LastSpoken]; ok && len(spokenAt) > 1 {
		age := spokenAt[len(spokenAt)-1] - spokenAt[len(spokenAt)-2]
		g.Speak(age)
	} else {
		g.Speak(0)
	}
	return g.LastSpoken
}

func (g *GameState) Speak(number int) {
	g.TurnIndex++
	g.LastSpoken = number
	if list, ok := g.numbersSpoken[number]; ok {
		g.numbersSpoken[number] = append(list, g.TurnIndex)
	} else {
		g.numbersSpoken[number] = []int{g.TurnIndex}
	}
}

func (g *GameState) SpeakAll(numbers ...int) {
	for _, num := range numbers {
		g.Speak(num)
	}
}
