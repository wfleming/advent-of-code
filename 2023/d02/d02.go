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

	allGames := []game{}
	for _, l := range lines {
		g, err := parseGame(l)
		if err != nil {
			fmt.Println("Error:", err)
			os.Exit(1)
		}
		allGames = append(allGames, g)
	}

	p1Bag := ballset{"red": 12, "green": 13, "blue": 14}

	p1PossibleIds := []int{}
	for _, g := range allGames {
		if g.isPossible(p1Bag) {
			p1PossibleIds = append(p1PossibleIds, g.id)
		}
	}
	fmt.Println("p1:", util.Sum(p1PossibleIds))

	p2Powers := []int{}
	for _, g := range allGames {
		p2Powers = append(p2Powers, bagPower(g.minViableBag()))
	}
	fmt.Println("p2:", util.Sum(p2Powers))
}
