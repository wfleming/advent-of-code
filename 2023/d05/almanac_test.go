package main

import (
	"reflect"
	"testing"
	"util"
)

const sampleInput string = `seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
`

func TestParseAlmanac(t *testing.T) {
	almanac, err := ParseAlmanac(util.SplitLines(sampleInput))

	if err != nil {
		t.Fatalf("failed to parse almanac: %v", err)
	}

	if expected := []int{79, 14, 55, 13}; !reflect.DeepEqual(almanac.seeds, expected) {
		t.Errorf("Alman seeds expected %v, got %v", expected, almanac.seeds)
	}

	if expected := []mapRange{
		mapRange{srcStart: 98, dstStart: 50, rangeLen: 2},
		mapRange{srcStart: 50, dstStart: 52, rangeLen: 48},
	}; !reflect.DeepEqual(almanac.seedToSoil.mappings, expected) {
		t.Errorf("Alman seeds expected %v, got %v", expected, almanac.seedToSoil)
	}

	if expected := []mapRange{
		mapRange{srcStart: 56, dstStart: 60, rangeLen: 37},
		mapRange{srcStart: 93, dstStart: 56, rangeLen: 4},
	}; !reflect.DeepEqual(almanac.humidityToLocation.mappings, expected) {
		t.Errorf("Alman seeds expected %v, got %v", expected, almanac.humidityToLocation)
	}
}

func TestSeedLocation(t *testing.T) {
	almanac, err := ParseAlmanac(util.SplitLines(sampleInput))

	if err != nil {
		t.Fatalf("failed to parse almanac: %v", err)
	}

	expectSeedLocation := func(seed, expectedLoc int) {
		loc := almanac.SeedLocation(seed)
		if loc != expectedLoc {
			t.Errorf("Expected seed %v to have location %v, but got %v", seed, expectedLoc, loc)
		}
	}

	expectSeedLocation(79, 82)
	expectSeedLocation(14, 43)
}
