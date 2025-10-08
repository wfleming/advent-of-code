package main

import (
	"fmt"
	"iter"
	"maps"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"
	"util"
)

func lineCalibration(line string) (int, error) {
	var digits []rune = []rune{}

	for _, ch := range line {
		if unicode.IsDigit(ch) {
			digits = append(digits, ch)
		}
	}

	if len(digits) < 1 {
		return 0, fmt.Errorf("Didn't find any digits in line '%s'", line)
	}

	valStr := string(digits[0]) + string(digits[len(digits)-1])

	valInt, err := strconv.Atoi(valStr)
	if err != nil {
		return 0, fmt.Errorf("Failed to parse '%s' into int: %s", valStr, err)
	}

	return valInt, nil
}

func tokenIsValidPrefix(token string, validTokens iter.Seq[string]) bool {
	for possibleToken := range validTokens {
		if strings.HasPrefix(possibleToken, token) {
			return true
		}
	}

	return false
}

func findFirstDigit(line []rune, strDigits map[string]rune) (rune, error) {
	allDigitTokens := maps.Keys(strDigits)
	token := ""

	for _, ch := range line {
		if unicode.IsDigit(ch) {
			return ch, nil
		} else {
			token += string(ch)

			for len(token) > 0 && !tokenIsValidPrefix(token, allDigitTokens) {
				token = token[1:]
			}

			digit, exists := strDigits[token]
			if exists {
				return digit, nil
			}
		}
	}

	return '\000', fmt.Errorf("No digit found in '%s'", string(line))
}

func lineCalibration2(line string) (calibration int, err error) {
	var strDigits map[string]rune = map[string]rune{
		"one":   '1',
		"two":   '2',
		"three": '3',
		"four":  '4',
		"five":  '5',
		"six":   '6',
		"seven": '7',
		"eight": '8',
		"nine":  '9',
	}

	firstDigit, err := findFirstDigit([]rune(line), strDigits)
	if err != nil {
		return
	}

	reverseStrDigits := map[string]rune{}
	for k, v := range strDigits {
		reverseK := []rune(k)
		slices.Reverse(reverseK)
		reverseStrDigits[string(reverseK)] = v
	}

	reverseLine := []rune(line)
	slices.Reverse(reverseLine)
	lastDigit, err := findFirstDigit(reverseLine, reverseStrDigits)
	if err != nil {
		return
	}

	valStr := string(firstDigit) + string(lastDigit)
	valInt, err := strconv.Atoi(valStr)
	if err != nil {
		return 0, fmt.Errorf("Failed to parse '%s' into int: %s", valStr, err)
	}

	return valInt, nil
}

func lineCalibrations(lines []string, lineCalc func(string) (int, error)) ([]int, error) {
	var calibrations []int = []int{}

	for _, l := range lines {
		lc, err := lineCalc(l)
		if err != nil {
			return []int{}, err
		}
		calibrations = append(calibrations, lc)
	}

	return calibrations, nil
}

func main() {
	lines, err := util.InputLines()
	if err != nil {
		fmt.Println("Error:", err)
		os.Exit(1)
	}

	p1LineCalibrations, err := lineCalibrations(lines, lineCalibration)
	if err != nil {
		fmt.Println("p1 failed: ", err)
	} else {
		fmt.Println("p1: ", util.Sum(p1LineCalibrations))
	}

	p2LineCalibrations, err := lineCalibrations(lines, lineCalibration2)
	if err != nil {
		fmt.Println("p2 failed: ", err)
	} else {
		fmt.Println("p2: ", util.Sum(p2LineCalibrations))
	}
}
