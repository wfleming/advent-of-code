package main

import (
	"fmt"
	"math"
	"os"
	"util"
)

func p1(almanac Almanac) int {
	minLoc := math.MaxInt

	for _, seed := range almanac.seeds {
		seedLoc := almanac.SeedLocation(seed)
		if seedLoc < minLoc {
			minLoc = seedLoc
		}
	}

	return minLoc
}

func p2(almanac Almanac) int {
	minLoc := math.MaxInt

	for idx := 0; idx < len(almanac.seeds); idx += 2 {
		seedStart := almanac.seeds[idx]
		seedRange := almanac.seeds[idx+1]

		for seed := seedStart; seed < seedStart+seedRange; seed++ {
			seedLoc := almanac.SeedLocation(seed)
			if seedLoc < minLoc {
				minLoc = seedLoc
			}
		}
	}

	return minLoc
}

func main() {
	lines, err := util.InputLines()
	if err != nil {
		fmt.Println("Error:", err)
		os.Exit(1)
	}

	almanac, err := ParseAlmanac(lines)
	if err != nil {
		fmt.Println("Error:", err)
		os.Exit(1)
	}

	fmt.Println("p1: ", p1(almanac))
	fmt.Println("p2: ", p2(almanac))
}
