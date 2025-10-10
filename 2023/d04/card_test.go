package main

import (
	"reflect"
	"testing"
)

func TestParseCard(t *testing.T) {
	card, err := ParseCard("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

	if err != nil {
		t.Fatalf("ParseCard errored: %v", err)
	}

	expected := Card{
		id:          1,
		winningNums: []int{41, 48, 83, 86, 17},
		heldNums:    []int{83, 86, 6, 31, 17, 9, 48, 53},
	}

	if !reflect.DeepEqual(card, expected) {
		t.Errorf("ParsCard should have returned %v, expected %v", card, expected)
	}
}

func TestCardPoints(t *testing.T) {
	card := Card{id: 1, winningNums: []int{}, heldNums: []int{}}
	if pts := card.Points(); pts != 0 {
		t.Errorf("Card %v expected %v points, but has %v", card, 0, pts)
	}

	card = Card{id: 1, winningNums: []int{7, 8}, heldNums: []int{5}}
	if pts := card.Points(); pts != 0 {
		t.Errorf("Card %v expected %v points, but has %v", card, 0, pts)
	}

	card = Card{id: 1, winningNums: []int{7, 8}, heldNums: []int{7}}
	if pts := card.Points(); pts != 1 {
		t.Errorf("Card %v expected %v points, but has %v", card, 1, pts)
	}

	card = Card{id: 1, winningNums: []int{6, 7, 8, 9}, heldNums: []int{7, 8}}
	if pts := card.Points(); pts != 2 {
		t.Errorf("Card %v expected %v points, but has %v", card, 2, pts)
	}

	card = Card{id: 1, winningNums: []int{6, 7, 8, 9}, heldNums: []int{7, 8, 6}}
	if pts := card.Points(); pts != 4 {
		t.Errorf("Card %v expected %v points, but has %v", card, 4, pts)
	}
}
