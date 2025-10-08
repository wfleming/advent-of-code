package main

import (
	"fmt"
	"strconv"
	"strings"
)

type ballset = map[string]int // map of color => qty

type game struct {
	id     int
	rounds []ballset
}

func parseBallQty(str string) (color string, qty int, err error) {
	pieces := strings.Split(strings.TrimSpace(str), " ")

	if len(pieces) != 2 {
		return "", 0, fmt.Errorf("Couldn't parse ball quantity from '%s'", str)
	}

	qty, err = strconv.Atoi(pieces[0])
	if err != nil {
		return "", 0, fmt.Errorf("Couldn't parse ball quantity from '%s' (%v)", str, err)
	}

	return strings.TrimSpace(pieces[1]), qty, nil
}

func parseBallset(str string) (balls ballset, err error) {
	balls = map[string]int{}

	for _, ballqty := range strings.Split(str, ",") {
		color, qty, err := parseBallQty(ballqty)
		if err != nil {
			return nil, err
		}
		balls[color] += qty
	}

	return balls, nil
}

func parseGame(str string) (g game, err error) {
	pieces := strings.Split(str, ":")

	if len(pieces) != 2 {
		return game{}, fmt.Errorf("Wrong format for game: %v", str)
	}

	gameIdStr := strings.TrimPrefix(pieces[0], "Game ")
	gameIdInt, err := strconv.Atoi(gameIdStr)
	if err != nil {
		return
	}

	rounds := []ballset{}
	for _, roundStr := range strings.Split(pieces[1], ";") {
		ballset, err := parseBallset(roundStr)
		if err != nil {
			return game{}, err
		}
		rounds = append(rounds, ballset)
	}

	return game{id: gameIdInt, rounds: rounds}, nil
}

func bagPower(b ballset) int {
	rv := 1
	for _, qty := range b {
		rv *= qty
	}

	return rv
}

func (g game) isPossible(bag ballset) bool {
	for _, round := range g.rounds {
		for color, qty := range round {
			bagQty, ok := bag[color]
			if !ok || bagQty < qty {
				return false
			}
		}
	}

	return true
}

func (g game) minViableBag() ballset {
	bag := ballset{}

	for _, round := range g.rounds {
		for color, qty := range round {
			curQty, ok := bag[color]
			if !ok || curQty < qty {
				bag[color] = qty
			}
		}
	}

	return bag
}
