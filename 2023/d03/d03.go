package main

import (
	"d03/engine"
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

	grid := util.NewCharGrid(lines)

	p1PartNums := engine.FindPartNumbers(grid)
	fmt.Println("p1:", util.Sum(p1PartNums))

	p2GearRatios := engine.FindGearRatios(grid)
	fmt.Println("p2:", util.Sum(p2GearRatios))
}
