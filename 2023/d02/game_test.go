package main

import (
	"reflect"
	"testing"
)

func TestParseBallQty(t *testing.T) {
	expectColorQty := func(str string, expectedColor string, expectedQty int) {
		color, qty, err := parseBallQty(str)
		if err != nil {
			t.Errorf("parseBallQty error: %v", err)
		} else if color != expectedColor || qty != expectedQty {
			t.Errorf("parseBallQty(%v) expected (%v, %v), got (%v, %v)", str, expectedColor, expectedQty, color, qty)
		}
	}

	expectColorQty("3 blue", "blue", 3)
	expectColorQty("2 red", "red", 2)
	expectColorQty("13 green", "green", 13)
}

func TestParseBallset(t *testing.T) {
	expectSet := func(str string, expectedBallset map[string]int) {
		ballset, err := parseBallset(str)
		if err != nil {
			t.Errorf("parseBallset error: %v", err)
		} else if !reflect.DeepEqual(ballset, expectedBallset) {
			t.Errorf("parseBallQty(%v) expected %v, got %v", str, expectedBallset, ballset)
		}
	}

	expectSet("3 blue", map[string]int{"blue": 3})
	expectSet("3 blue, 13 green", map[string]int{"blue": 3, "green": 13})
	// I don't think real stuff repeats colors, but let's test it anyway
	expectSet("3 blue, 13 green, 2 blue", map[string]int{"blue": 5, "green": 13})
}

func TestParseGame(t *testing.T) {
	expectGame := func(str string, expectedGame game) {
		g, err := parseGame(str)
		if err != nil {
			t.Errorf("parseGame error: %v", err)
		} else if !reflect.DeepEqual(g, expectedGame) {
			t.Errorf("parseGame(%v) expected %v, got %v", str, expectedGame, g)
		}
	}

	expectGame(
		"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
		game{id: 1, rounds: []ballset{
			ballset{"blue": 3, "red": 4},
			ballset{"red": 1, "green": 2, "blue": 6},
			ballset{"green": 2},
		}},
	)
}

func TestIsPossible(t *testing.T) {
	g := game{id: 1, rounds: []ballset{
		ballset{"blue": 3, "red": 4},
		ballset{"red": 1, "green": 2, "blue": 6},
		ballset{"green": 2},
	}}

	if !g.isPossible(ballset{"blue": 7, "green": 3, "red": 5}) {
		t.Errorf("isPossible should have been true, is false")
	}

	if g.isPossible(ballset{"green": 1, "red": 1, "blue": 1}) {
		t.Errorf("isPossible should have been false, is true")
	}
}

func TestMinViableBag(t *testing.T) {
	g := game{id: 1, rounds: []ballset{
		ballset{"blue": 3, "red": 4},
		ballset{"red": 1, "green": 2, "blue": 6},
		ballset{"green": 2},
	}}

	expected := ballset{"blue": 6, "red": 4, "green": 2}
	bag := g.minViableBag()
	if !reflect.DeepEqual(bag, expected) {
		t.Errorf("minViableBag calculated %v, expected %v", bag, expected)
	}
}
