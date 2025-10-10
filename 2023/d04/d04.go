package main

import (
	"fmt"
	"os"
	"util"
)

func main() {
	lines, err := util.InputLines()
	if err != nil {
		fmt.Println("Error:", err)
		os.Exit(1)
	}

	cards := make([]Card, len(lines))
	for idx, line := range lines {
		c, err := ParseCard(line)
		if err != nil {
			fmt.Println("Error parsing card:", err)
			os.Exit(1)
		}
		cards[idx] = c
	}

	p1Pts := 0
	for _, card := range cards {
		p1Pts += card.Points()
	}
	fmt.Println("p1: ", p1Pts)

	p2Cards := Part2(cards)
	fmt.Println("p2: ", p2Cards)
}
