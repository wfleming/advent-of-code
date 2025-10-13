package util

import (
	"reflect"
	"testing"
)

func TestSplitLines(t *testing.T) {
	txt := `foo
bar
baz`

	expected := []string{"foo", "bar", "baz"}
	actual := SplitLines(txt)

	if !reflect.DeepEqual(actual, expected) {
		t.Errorf("Got %v, expected %v", actual, expected)
	}
}

func TestSpaceSepInts(t *testing.T) {
	expected := []int{42, 37, 73}
	actual, err := SpaceSepInts("42 37 73")
	if err != nil {
		t.Fatalf("Unexpected SpaceSepInts error: %c", err)
	}
	if !reflect.DeepEqual(actual, expected) {
		t.Errorf("Got %v, expected %v", actual, expected)
	}

	_, err = SpaceSepInts("42 foo 73")
	if err == nil {
		t.Fatalf("SpaceSepInts should have returned error but did not")
	}
}
