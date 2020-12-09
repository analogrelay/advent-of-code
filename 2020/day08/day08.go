package day08

import (
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var ExecutedTwiceFault = errors.New("executed instruction twice")
var EndOfProgramFault = errors.New("end of program")

func Run(lines []string) error {
	trace := os.Getenv("AOC_TRACE") == "1"

	program, err := ParseProgram(lines)
	if err != nil {
		return fmt.Errorf("invalid program: %v", err)
	}
	cpu := NewCPU(program)
	err = cpu.Execute(trace)
	if err != ExecutedTwiceFault {
		return fmt.Errorf("unexpected fault: %v", err)
	}
	fmt.Println("Part 1:", cpu.Accumulator)

	// Try flipping one nop or jmp
	counter := 0
	for counter < len(program) {
		modifiedProgram, err := toggleInstruction(program, counter)
		if err != nil {
			return err
		}
		cpu := NewCPU(modifiedProgram)
		err = cpu.Execute(trace)
		if err == EndOfProgramFault {
			fmt.Println("Part 2:", cpu.Accumulator)
			return nil
		}
		counter++
	}

	return fmt.Errorf("no modification caused the program to exit")
}

func toggleInstruction(program []*Instruction, targetCount int) ([]*Instruction, error) {
	newProgram := make([]*Instruction, 0, len(program))
	count := 0
	didModify := false
	for _, instr := range program {
		newInstr := instr
		if instr.Opcode == OP_NOP {
			if count == targetCount {
				newInstr = &Instruction{
					Opcode:  OP_JMP,
					Operand: instr.Operand,
				}
				didModify = true
			}
			count++
		} else if instr.Opcode == OP_JMP {
			if count == targetCount {
				newInstr = &Instruction{
					Opcode:  OP_NOP,
					Operand: instr.Operand,
				}
				didModify = true
			}
			count++
		}

		newInstr.ExecuteCount = 0
		newProgram = append(newProgram, newInstr)
	}

	if didModify {
		return newProgram, nil
	} else {
		return nil, fmt.Errorf("no more nops or jmps")
	}
}

type Operation int

const (
	OP_NOP Operation = 0
	OP_ACC Operation = 1
	OP_JMP Operation = 2
)

var opcodes = map[string]Operation{
	"nop": OP_NOP,
	"acc": OP_ACC,
	"jmp": OP_JMP,
}

type Instruction struct {
	Opcode       Operation
	Operand      int
	ExecuteCount int
}

func (i *Instruction) String() string {
	var opcode string
	switch i.Opcode {
	case OP_ACC:
		opcode = "acc"
	case OP_NOP:
		opcode = "nop"
	case OP_JMP:
		opcode = "jmp"
	default:
		opcode = "unk"
	}

	return fmt.Sprintf("%s %+d", opcode, i.Operand)
}

type CPU struct {
	Accumulator    int
	ProgramCounter int
	Program        []*Instruction
}

func NewCPU(program []*Instruction) CPU {
	return CPU{
		Accumulator:    0,
		ProgramCounter: 0,
		Program:        program,
	}
}

func (c *CPU) Execute(trace bool) error {
	for {
		err := c.Step(trace)
		if err != nil {
			return err
		}
	}
}

func (c *CPU) Step(trace bool) error {
	if c.ProgramCounter < 0 {
		return fmt.Errorf("segmentation fault") // :P
	} else if c.ProgramCounter == len(c.Program) {
		return EndOfProgramFault
	}

	inst := c.Program[c.ProgramCounter]
	if inst == nil {
		return fmt.Errorf("invalid program")
	}

	if trace {
		fmt.Printf("Executing 0x%04X : %v\n", c.ProgramCounter, inst)
	}

	inst.ExecuteCount++
	if inst.ExecuteCount > 1 {
		if trace {
			fmt.Println("> ExecutedTwiceFault")
		}
		return ExecutedTwiceFault
	}
	switch inst.Opcode {
	case OP_ACC:
		c.Accumulator += inst.Operand
		c.ProgramCounter++
	case OP_JMP:
		c.ProgramCounter += inst.Operand
	case OP_NOP:
		c.ProgramCounter++
	default:
		return fmt.Errorf("unknown opcode: 0x%02X", inst.Opcode)
	}

	return nil
}

func ParseProgram(lines []string) ([]*Instruction, error) {
	instrs := make([]*Instruction, 0, len(lines))
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if len(line) > 0 {
			instr, err := ParseInstruction(line)
			if err != nil {
				return nil, err
			}
			instrs = append(instrs, &instr)
		}
	}
	return instrs, nil
}

func ParseInstruction(line string) (Instruction, error) {
	spaceIdx := strings.Index(line, " ")
	if spaceIdx < 0 {
		return Instruction{}, fmt.Errorf("invalid instruction '%s'", line)
	}

	arg := line[spaceIdx+1:]
	operand, err := strconv.Atoi(arg)
	if err != nil {
		return Instruction{}, fmt.Errorf("error parsing argument '%s': %v", arg, err)
	}

	opcode, ok := opcodes[line[:spaceIdx]]
	if !ok {
		return Instruction{}, fmt.Errorf("unknown opcode '%s'", line[:spaceIdx])
	}

	return Instruction{Opcode: opcode, Operand: operand}, nil
}
