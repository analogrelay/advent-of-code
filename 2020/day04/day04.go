package day04

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

var requiredFields = []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
var validColors = []string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}

var hexColorRegex = regexp.MustCompile("^#[0-9a-f]{6}$")
var pidRegex = regexp.MustCompile("^[0-9]{9}$")

type passport struct {
	fields map[string]string
}

func (p *passport) HasRequiredFields() bool {
	for _, f := range requiredFields {
		if _, ok := p.fields[f]; !ok {
			return false
		}
	}

	return true
}

func (p *passport) IsValid() bool {
	return validateYear(p.fields["byr"], 1920, 2002) &&
		validateYear(p.fields["iyr"], 2010, 2020) &&
		validateYear(p.fields["eyr"], 2020, 2030) &&
		validateHeight(p.fields["hgt"]) &&
		hexColorRegex.MatchString(p.fields["hcl"]) &&
		validateSet(p.fields["ecl"], validColors) &&
		pidRegex.MatchString(p.fields["pid"])
}

func validateHeight(height string) bool {
	len := len(height)
	value, err := strconv.Atoi(height[:len-2])
	if err != nil {
		return false
	}
	suffix := height[len-2:]
	if suffix == "cm" {
		return value >= 150 && value <= 193
	} else {
		return value >= 59 && value <= 76
	}
}

func validateSet(input string, set []string) bool {
	for _, cand := range set {
		if cand == input {
			return true
		}
	}
	return false
}

func validateYear(year string, min, max int) bool {
	if len(year) != 4 {
		return false
	}
	val, err := strconv.Atoi(year)
	if err != nil {
		return false
	}
	return val >= min && val <= max
}

func Run(lines []string) error {
	passports, err := parsePassports(lines)
	if err != nil {
		return fmt.Errorf("error processing passports: %v", err)
	}

	fmt.Println("Total passports:", len(passports))

	requiredFieldCount := 0
	validCount := 0
	for _, p := range passports {
		if p.HasRequiredFields() {
			if p.IsValid() {
				validCount++
			}
			requiredFieldCount++
		}
	}

	fmt.Println("Part 1:", requiredFieldCount)
	fmt.Println("Part 2:", validCount)

	return nil
}

func parsePassports(lines []string) ([]passport, error) {
	currentFields := make(map[string]string)
	var passports []passport
	for _, line := range lines {
		if len(line) != 0 {
			fields := strings.Split(line, " ")
			for _, field := range fields {
				colonIdx := strings.Index(field, ":")
				if colonIdx < 0 {
					return passports, fmt.Errorf("invalid field '%s'", field)
				}
				fieldName := field[:colonIdx]
				fieldValue := field[colonIdx+1:]
				currentFields[fieldName] = fieldValue
			}
		} else {
			if len(currentFields) > 0 {
				passports = append(passports, passport{fields: currentFields})
			}
			currentFields = make(map[string]string)
		}
	}

	if len(currentFields) > 0 {
		passports = append(passports, passport{fields: currentFields})
	}

	return passports, nil
}
