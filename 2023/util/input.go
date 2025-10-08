package util

import (
	"errors"
	"os"
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

func InputLines() (lines []string, err error) {
	content, err := InputStr()
	if err != nil {
		return
	}

	lines = []string{}
	for l := range strings.Lines(content) {
		lines = append(lines, strings.TrimSpace(l))
	}

	return lines, nil
}
