package util

type CharGrid struct {
	data []string
}

func NewCharGrid(lines []string) CharGrid {
	if len(lines) > 0 {
		width := len(lines[0])
		for _, l := range lines {
			if len(l) != width {
				panic("All lines passed to Grid must be same length")
			}
		}
	}

	return CharGrid{data: lines}
}

func (grid CharGrid) Height() int {
	return len(grid.data)
}

func (grid CharGrid) Width() int {
	if len(grid.data) == 0 {
		return 0
	}

	return len(grid.data[0])
}

func (grid CharGrid) At(x, y int) rune {
	if y < 0 || x < 0 || y >= grid.Height() || x >= grid.Width() {
		return '\000'
	}

	return rune(grid.data[y][x])
}
