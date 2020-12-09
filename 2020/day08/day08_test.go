package day08

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestParseInstructions(t *testing.T) {
	tests := map[string]Instruction{
		"nop +0":  {Opcode: OP_NOP, Operand: 0},
		"acc +1":  {Opcode: OP_ACC, Operand: 1},
		"jmp +4":  {Opcode: OP_JMP, Operand: 4},
		"acc +3":  {Opcode: OP_ACC, Operand: 3},
		"jmp -3":  {Opcode: OP_JMP, Operand: -3},
		"acc -99": {Opcode: OP_ACC, Operand: -99},
		"jmp -4":  {Opcode: OP_JMP, Operand: -4},
		"acc +6":  {Opcode: OP_ACC, Operand: 6},
	}

	for instr, expected := range tests {
		name := strings.ReplaceAll(instr, " ", "_")
		name = strings.ReplaceAll(name, "+", "p")
		name = strings.ReplaceAll(name, "-", "m")
		t.Run(name, func(t *testing.T) {
			actual, err := ParseInstruction(instr)
			require.NoError(t, err)
			require.Equal(t, expected, actual)
		})
	}
}

func TestProgram(t *testing.T) {
	program, err := ParseProgram([]string{
		"nop +0",
		"acc +1",
		"jmp +4",
		"acc +3",
		"jmp -3",
		"acc -99",
		"acc +1",
		"jmp -4",
		"acc +6",
	})
	require.NoError(t, err)

	cpu := NewCPU(program)

	require.Equal(t, 0, cpu.ProgramCounter)
	require.Equal(t, 0, cpu.Accumulator)

	require.NoError(t, cpu.Step()) // nop +0
	require.Equal(t, 1, cpu.ProgramCounter)
	require.Equal(t, 0, cpu.Accumulator)

	require.NoError(t, cpu.Step()) // acc +1
	require.Equal(t, 2, cpu.ProgramCounter)
	require.Equal(t, 1, cpu.Accumulator)

	require.NoError(t, cpu.Step()) // jmp +4
	require.Equal(t, 6, cpu.ProgramCounter)
	require.Equal(t, 1, cpu.Accumulator)

	require.NoError(t, cpu.Step()) // acc +1
	require.Equal(t, 7, cpu.ProgramCounter)
	require.Equal(t, 2, cpu.Accumulator)

	require.NoError(t, cpu.Step()) // jmp -4
	require.Equal(t, 3, cpu.ProgramCounter)
	require.Equal(t, 2, cpu.Accumulator)

	require.NoError(t, cpu.Step()) // acc +3
	require.Equal(t, 4, cpu.ProgramCounter)
	require.Equal(t, 5, cpu.Accumulator)

	require.NoError(t, cpu.Step()) // jmp -3
	require.Equal(t, 1, cpu.ProgramCounter)
	require.Equal(t, 5, cpu.Accumulator)

	require.Equal(t, ExecutedTwiceFault, cpu.Step())
	require.Equal(t, 1, cpu.ProgramCounter)
	require.Equal(t, 5, cpu.Accumulator)
}
