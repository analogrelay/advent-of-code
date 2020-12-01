package main

import (
	"fmt"
	"os"
	"strconv"
)

type dayHandler = func([]string) error

var days map[int]dayHandler = make(map[int]dayHandler)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s [day number]\n", os.Args[0])
		os.Exit(1)
	}

	day, err := strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Invalid Day %v\n", os.Args[1])
		os.Exit(1)
	}

	if handler, ok := days[day]; ok {
		args := os.Args[2:]
		if err = handler(args); err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			os.Exit(1)
		}
	} else {
		fmt.Fprintf(os.Stderr, "No handler registered for day %v\n", day)
	}
}

func registerDay(dayNumber int, handler dayHandler) {
	days[dayNumber] = handler
}
