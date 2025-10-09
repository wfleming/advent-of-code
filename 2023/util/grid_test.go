package util

import (
	"testing"
)

func TestCharGrid(t *testing.T) {
	grid := NewCharGrid([]string{
		"abc",
		"def",
	})

	if grid.Height() != 2 {
		t.Errorf("grid Height is wrong (expected 2, got %v)", grid.Height())
	}
	if grid.Width() != 3 {
		t.Errorf("grid Width is wrong (expected 3, got %v)", grid.Width())
	}

	c := grid.At(7, 7)
	if c != '\000' {
		t.Errorf("grid.At(7,7) should have returned not-ok, but found char %v", string(c))
	}

	c = grid.At(0, 0)
	if c != 'a' {
		t.Errorf("grid.At(0,0) unexpected return (expected a, got %v)", string(c))
	}

	c = grid.At(2, 1)
	if c != 'f' {
		t.Errorf("grid.At(2,1) unexpected return (expected e, got %v)", string(c))
	}
}
