all: bin/aoc

bin:
	@mkdir $@

bin/aoc: main.go | bin
	go build -o $@ $<