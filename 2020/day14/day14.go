package day14

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

func Run(lines []string) error {
	cpu := NewCPU(1)

	err := cpu.Run(lines)
	if err != nil {
		return err
	}
	sum := cpu.SumMemory()
	fmt.Println("Part 1:", sum)

	cpu = NewCPU(2)

	err = cpu.Run(lines)
	if err != nil {
		return err
	}
	sum = cpu.SumMemory()
	fmt.Println("Part 2:", sum)

	return nil
}

func applyMaskV2(value int64, mask string) (string, error) {
	var result strings.Builder
	for i := range mask {
		if mask[i] == '0' {
			valueBit := (value >> (35 - i)) & 0b1
			result.WriteString(fmt.Sprint(valueBit))
		} else {
			err := result.WriteByte(mask[i])
			if err != nil {
				return "", fmt.Errorf("error writing to builder: %v", err)
			}
		}
	}
	return result.String(), nil
}

func applyMask(value int64, mask string) int64 {
	var output int64
	output = 0
	for i := 0; i < 36; i++ {
		switch mask[35-i] {
		case 'X':
			// Copy the current flag value
			output |= (value & (int64(1) << i))
		case '1':
			output |= 1 << i
		}
		// For a 0, just leave the output at zero
	}
	return output
}

func expandFloating(input string) []string {
	results := []string{}
	for _, chr := range input {
		if chr == 'X' {
			results = expandOne(results, []string{"0", "1"})
		} else {
			results = expandOne(results, []string{string(chr)})
		}
	}

	return results
}

func expandOne(inputs []string, expansions []string) []string {
	if len(inputs) == 0 {
		return expansions
	} else {
		results := make([]string, 0, len(expansions)*len(inputs))
		for _, input := range inputs {
			for _, exp := range expansions {
				results = append(results, input+exp)
			}
		}
		return results
	}
}

type CPU struct {
	Mask    string
	Memory  map[int64]int64
	Version int
}

func NewCPU(version int) CPU {
	return CPU{Mask: "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Memory: make(map[int64]int64), Version: version}
}

var memMatcher = regexp.MustCompile(`^mem\[(\d+)\] = (\d+)$`)

func (c *CPU) SumMemory() int64 {
	sum := int64(0)
	for _, val := range c.Memory {
		sum += val
	}
	return sum
}

func (c *CPU) Run(program []string) error {
	for _, line := range program {
		err := c.Step(line)
		if err != nil {
			return fmt.Errorf("error executing '%s': %v", line, err)
		}
	}
	return nil
}

func (c *CPU) Step(line string) error {
	if strings.HasPrefix(line, "mask = ") {
		newMask := line[7:]
		if len(newMask) != 36 {
			return fmt.Errorf("invalid instruction: %s", line)
		}
		c.Mask = newMask
	} else {
		matches := memMatcher.FindStringSubmatch(line)
		if len(matches) == 3 {
			addr, err := strconv.ParseInt(matches[1], 10, 64)
			if err != nil {
				return fmt.Errorf("invalid address: %s", matches[1])
			}
			value, err := strconv.ParseInt(matches[2], 10, 64)
			if err != nil {
				return fmt.Errorf("invalid value: %s", matches[2])
			}

			addrs, err := c.getAddrs(addr)
			if err != nil {
				return err
			}
			value = c.getVal(value)

			for _, addr := range addrs {
				c.Memory[addr] = value
			}
		} else {
			return fmt.Errorf("invalid instruction: %s", line)
		}
	}
	return nil
}

func (c *CPU) getAddrs(addr int64) ([]int64, error) {
	if c.Version == 1 {
		return []int64{addr}, nil
	} else {
		mask, err := applyMaskV2(addr, c.Mask)
		if err != nil {
			return nil, err
		}
		strs := expandFloating(mask)
		results := make([]int64, 0, len(strs))
		for _, str := range strs {
			i, err := strconv.ParseInt(str, 2, 64)
			if err != nil {
				return nil, fmt.Errorf("failed to parse input '%s'", str)
			}
			results = append(results, i)
		}
		return results, nil
	}
}

func (c *CPU) getVal(val int64) int64 {
	if c.Version == 1 {
		return applyMask(val, c.Mask)
	} else {
		return val
	}
}
