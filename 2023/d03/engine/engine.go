package engine

import (
	"slices"
	"strconv"
	"unicode"
	"util"
)

type point struct {
	x, y int
}

// Given a coordinate with a digit, scan left/right for entire number
// return the parsed number and the coordinate it starts on
func FindNumber(grid util.CharGrid, x, y int) (int, point) {
	// walk left to first digit
	for x > 0 && unicode.IsDigit(grid.At(x-1, y)) {
		x--
	}

	// walk right, collecting digits
	startCoord := point{x: x, y: y}
	numStr := ""
	for unicode.IsDigit(grid.At(x, y)) {
		numStr += string(grid.At(x, y))
		x++
	}

	// convert stringy number to int
	num, err := strconv.Atoi(numStr)
	if err != nil {
		panic(err) // since we've been checking digits all the way and I don't feel like bubbling errors just panic
	}

	return num, startCoord
}

// Find all part numbers in the grid
func FindPartNumbers(grid util.CharGrid) []int {
	nums := []int{}
	numCoords := []point{}
	var char rune

	for x := 0; x < grid.Width(); x++ {
		for y := 0; y < grid.Height(); y++ {
			char = grid.At(x, y)
			if unicode.IsDigit(char) || char == '.' {
				continue
			}

			for _, xd := range []int{-1, 0, 1} {
				for _, yd := range []int{-1, 0, 1} {
					if xd == 0 && yd == 0 {
						continue
					}

					if unicode.IsDigit(grid.At(x+xd, y+yd)) {
						num, numStart := FindNumber(grid, x+xd, y+yd)
						if !slices.Contains(numCoords, numStart) {
							nums = append(nums, num)
							numCoords = append(numCoords, numStart)
						}
					}
				}
			}
		}
	}

	return nums
}

// Find all gears in the grid. Returns list of gear ratios.
func FindGearRatios(grid util.CharGrid) []int {
	var ratios []int
	var char rune

	for x := 0; x < grid.Width(); x++ {
		for y := 0; y < grid.Height(); y++ {
			char = grid.At(x, y)
			if char != '*' {
				continue
			}

			var nums []int
			var numCoords []point

			for _, xd := range []int{-1, 0, 1} {
				for _, yd := range []int{-1, 0, 1} {
					if xd == 0 && yd == 0 {
						continue
					}

					if unicode.IsDigit(grid.At(x+xd, y+yd)) {
						num, numStart := FindNumber(grid, x+xd, y+yd)
						if !slices.Contains(numCoords, numStart) {
							nums = append(nums, num)
							numCoords = append(numCoords, numStart)
						}
					}
				}
			}

			if len(nums) == 2 {
				ratios = append(ratios, nums[0]*nums[1])
			}
		}
	}

	return ratios
}
