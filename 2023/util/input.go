package util

import (
	"errors"
	"os"
	"strconv"
	"strings"
)

func InputStr() (content string, err error) {
	if len(os.Args) < 2 {
		return "", errors.New("missing input file argument")
	}

	bytes, err := os.ReadFile(os.Args[1])
	if err != nil {
		return
	}

	return string(bytes), nil
}

func SplitLines(content string) []string {
	lines := []string{}
	for l := range strings.Lines(content) {
		lines = append(lines, strings.TrimSpace(l))
	}
	return lines
}

func SpaceSepInts(content string) ([]int, error) {
	nums := []int{}

	for _, numStr := range strings.Fields(content) {
		numInt, err := strconv.Atoi(numStr)
		if err != nil {
			return []int{}, err
		}
		nums = append(nums, numInt)
	}

	return nums, nil
}

func InputLines() (lines []string, err error) {
	content, err := InputStr()
	if err != nil {
		return
	}

	return SplitLines(content), nil
}
