package main

import (
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"
	"util"
)

type Card struct {
	id          int
	winningNums []int
	heldNums    []int
}

func ParseCard(line string) (Card, error) {
	line = strings.TrimPrefix(line, "Card ")
	parts := strings.Split(line, ":")

	if len(parts) != 2 {
		return Card{}, fmt.Errorf("Line '%v' does not look like a card", line)
	}

	cardId, err := strconv.Atoi(strings.TrimSpace(parts[0]))
	if err != nil {
		return Card{}, fmt.Errorf("Did not find card id at start of line '%v'", line)
	}

	parts = strings.Split(parts[1], "|")

	if len(parts) != 2 {
		return Card{}, fmt.Errorf("Numbers part of card may be malformed in '%v'", line)
	}

	winningNumStrs := strings.Fields(parts[0])
	heldNumStrs := strings.Fields(parts[1])
	winningNums := make([]int, len(winningNumStrs))
	heldNums := make([]int, len(heldNumStrs))

	for idx, numStr := range winningNumStrs {
		x, err := strconv.Atoi(numStr)
		if err != nil {
			return Card{}, fmt.Errorf("Failed to parse number from %v: %v", numStr, err)
		}
		winningNums[idx] = x
	}

	for idx, numStr := range heldNumStrs {
		x, err := strconv.Atoi(numStr)
		if err != nil {
			return Card{}, fmt.Errorf("Failed to parse number from %v: %v", numStr, err)
		}
		heldNums[idx] = x
	}

	return Card{id: cardId, winningNums: winningNums, heldNums: heldNums}, nil
}

func (card Card) Points() int {
	pts := 0

	for _, n := range card.heldNums {
		if !slices.Contains(card.winningNums, n) {
			continue
		}

		if pts == 0 {
			pts = 1
		} else {
			pts *= 2
		}
	}

	return pts
}

func (card Card) HeldWinners() []int {
	heldWinners := []int{}

	for _, n := range card.heldNums {
		if slices.Contains(card.winningNums, n) {
			heldWinners = append(heldWinners, n)
		}
	}

	return heldWinners
}

func Part2(cards []Card) int {
	cardsHeld := map[int]int{} // id of card => how many copies we have

	for idx, card := range cards {
		cardsHeld[card.id] += 1
		score := len(card.HeldWinners())

		if idx < len(cards)-1 && score > 0 {
			rewardCards := cards[idx+1 : idx+1+score]
			for _, rewardCard := range rewardCards {
				cardsHeld[rewardCard.id] += cardsHeld[card.id]
			}
		}
	}

	return util.Sum(slices.Collect(maps.Values(cardsHeld)))
}
