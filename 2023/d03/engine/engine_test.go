package engine

import (
	"reflect"
	"testing"
	"util"
)

var sampleLines = []string{
	"467..114..",
	"...*......",
	"..35..633.",
	"........17",
}

func TestFindNumber(t *testing.T) {
	grid := util.NewCharGrid(sampleLines)

	if x, _ := FindNumber(grid, 1, 0); x != 467 {
		t.Errorf("FindNumber(..., 0, 1): expected 467, got %v", x)
	}
	if x, _ := FindNumber(grid, 3, 2); x != 35 {
		t.Errorf("FindNumber(..., 3, 2): expected 35, got %v", x)
	}
}

func TestFindPartNumbers(t *testing.T) {
	grid := util.NewCharGrid(sampleLines)
	expected := []int{467, 35}

	if actual := FindPartNumbers(grid); !reflect.DeepEqual(actual, expected) {
		t.Errorf("FindPartNumbers: expected %v, got %v", expected, actual)
	}
}

func TestFindGearRatios(t *testing.T) {
	grid := util.NewCharGrid(sampleLines)
	expected := []int{16345}

	if actual := FindGearRatios(grid); !reflect.DeepEqual(actual, expected) {
		t.Errorf("FindGearRatios: expected %v, got %v", expected, actual)
	}
}
