package day14

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestApplyMask(t *testing.T) {
	mask := "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
	require.Equal(t, int64(73), applyMask(11, mask))
	require.Equal(t, int64(101), applyMask(101, mask))
	require.Equal(t, int64(64), applyMask(0, mask))
}

func TestVersion1(t *testing.T) {
	program := []string{
		"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
		"mem[8] = 11",
		"mem[7] = 101",
		"mem[8] = 0",
	}

	cpu := NewCPU(1)

	require.Equal(t, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", cpu.Mask)
	require.Equal(t, 0, len(cpu.Memory))

	require.NoError(t, cpu.Step(program[0]))
	require.Equal(t, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", cpu.Mask)
	require.Equal(t, 0, len(cpu.Memory))

	require.NoError(t, cpu.Step(program[1]))
	require.Equal(t, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", cpu.Mask)
	require.Equal(t, 1, len(cpu.Memory))
	require.Equal(t, int64(73), cpu.Memory[8])

	require.NoError(t, cpu.Step(program[2]))
	require.Equal(t, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", cpu.Mask)
	require.Equal(t, 2, len(cpu.Memory))
	require.Equal(t, int64(73), cpu.Memory[8])
	require.Equal(t, int64(101), cpu.Memory[7])

	require.NoError(t, cpu.Step(program[3]))
	require.Equal(t, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", cpu.Mask)
	require.Equal(t, 2, len(cpu.Memory))
	require.Equal(t, int64(64), cpu.Memory[8])
	require.Equal(t, int64(101), cpu.Memory[7])
}

func TestVersion2(t *testing.T) {
	program := []string{
		"mask = 000000000000000000000000000000X1001X",
		"mem[42] = 100",
		"mask = 00000000000000000000000000000000X0XX",
		"mem[26] = 1",
	}

	cpu := NewCPU(2)

	require.Equal(t, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", cpu.Mask)
	require.Equal(t, 0, len(cpu.Memory))

	require.NoError(t, cpu.Step(program[0]))
	require.Equal(t, "000000000000000000000000000000X1001X", cpu.Mask)
	require.Equal(t, 0, len(cpu.Memory))

	require.NoError(t, cpu.Step(program[1]))
	require.Equal(t, "000000000000000000000000000000X1001X", cpu.Mask)
	require.Equal(t, 4, len(cpu.Memory))
	require.Equal(t, int64(100), cpu.Memory[26])
	require.Equal(t, int64(100), cpu.Memory[27])
	require.Equal(t, int64(100), cpu.Memory[58])
	require.Equal(t, int64(100), cpu.Memory[59])

	require.NoError(t, cpu.Step(program[2]))
	require.Equal(t, "00000000000000000000000000000000X0XX", cpu.Mask)
	require.Equal(t, 4, len(cpu.Memory))

	require.NoError(t, cpu.Step(program[3]))
	require.Equal(t, "00000000000000000000000000000000X0XX", cpu.Mask)
	require.Equal(t, 10, len(cpu.Memory))
	require.Equal(t, int64(1), cpu.Memory[16])
	require.Equal(t, int64(1), cpu.Memory[17])
	require.Equal(t, int64(1), cpu.Memory[18])
	require.Equal(t, int64(1), cpu.Memory[19])
	require.Equal(t, int64(1), cpu.Memory[24])
	require.Equal(t, int64(1), cpu.Memory[25])
	require.Equal(t, int64(1), cpu.Memory[26])
	require.Equal(t, int64(1), cpu.Memory[27])

	require.Equal(t, int64(100), cpu.Memory[58])
	require.Equal(t, int64(100), cpu.Memory[59])
}

func TestExpandFloating(t *testing.T) {
	require.Equal(t, []string{
		"000000000000000000000000000000011010",
		"000000000000000000000000000000011011",
		"000000000000000000000000000000111010",
		"000000000000000000000000000000111011",
	}, expandFloating("000000000000000000000000000000X1101X"))

	require.Equal(t, []string{
		"000000000000000000000000000000010000",
		"000000000000000000000000000000010001",
		"000000000000000000000000000000010010",
		"000000000000000000000000000000010011",
		"000000000000000000000000000000011000",
		"000000000000000000000000000000011001",
		"000000000000000000000000000000011010",
		"000000000000000000000000000000011011",
	}, expandFloating("00000000000000000000000000000001X0XX"))
}

func TestApplyMaskV2(t *testing.T) {
	result, err := applyMaskV2(42, "000000000000000000000000000000X1001X")
	require.NoError(t, err)
	require.Equal(t, "000000000000000000000000000000X1101X", result)

	result, err = applyMaskV2(26, "00000000000000000000000000000000X0XX")
	require.NoError(t, err)
	require.Equal(t, "00000000000000000000000000000001X0XX", result)
}
