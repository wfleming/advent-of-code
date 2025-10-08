package main

import (
	"testing"
)

func TestLineCalibration(t *testing.T) {
	expectCalibration := func(str string, expected int) {
		x, err := lineCalibration(str)
		if err != nil {
			t.Errorf("%v != %v (error: %v)", str, expected, err)
		} else if x != expected {
			t.Errorf("%v != %v (got %v)", str, expected, x)
		}
	}

	expectCalibration("1abc2", 12)
	expectCalibration("1a3bc2z", 12)
	expectCalibration("treb7uchet", 77)
}

func TestLineCalibration2(t *testing.T) {
	expectCalibration := func(str string, expected int) {
		x, err := lineCalibration2(str)
		if err != nil {
			t.Errorf("%v != %v (error: %v)", str, expected, err)
		} else if x != expected {
			t.Errorf("%v != %v (got %v)", str, expected, x)
		}
	}

	expectCalibration("1abc2", 12)
	expectCalibration("1a3bc2z", 12)
	expectCalibration("treb7uchet", 77)
	expectCalibration("1twothreez", 13)
	expectCalibration("eightwothree", 83)
	expectCalibration("twone", 21)
	expectCalibration("2one", 21)
	expectCalibration("2onezz", 21)
	expectCalibration("2onene", 21)
	expectCalibration("aaaaatwoonezzzz", 21)
}
