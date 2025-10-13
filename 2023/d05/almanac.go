package main

import (
	"fmt"
	"strings"
	"util"
)

type mapRange struct {
	srcStart, dstStart, rangeLen int
}

type categoryMap struct {
	mappings []mapRange
}

func (cm categoryMap) mapSrc(src int) int {
	for _, mapping := range cm.mappings {
		if src >= mapping.srcStart && src < mapping.srcStart+mapping.rangeLen {
			// fmt.Printf("mapping category match: src=%v mapping=%v result=%v\n", src, mapping, mapping.dstStart + (src - mapping.srcStart))
			return mapping.dstStart + (src - mapping.srcStart)
		}
	}

	return src
}

type Almanac struct {
	seeds []int
	seedToSoil,
	soilToFertilizer,
	fertilizerToWater,
	waterToLight,
	lightToTemp,
	tempToHumidity,
	humidityToLocation categoryMap
}

func parseSeedLine(line string) ([]int, error) {
	line = strings.TrimPrefix(line, "seeds:")
	return util.SpaceSepInts(line)
}

func parseMapSection(lines []string) (categoryMap, error) {
	mappings := []mapRange{}

	for _, line := range lines {
		if len(line) == 0 { // hit end of map section
			break
		}

		nums, err := util.SpaceSepInts(line)
		if err != nil {
			return categoryMap{}, fmt.Errorf("Error getting nums from map line %v: %v", line, err)
		}
		if len(nums) != 3 {
			return categoryMap{}, fmt.Errorf("Unexpected number of nums from map line %v", line)
		}

		mappings = append(mappings, mapRange{srcStart: nums[1], dstStart: nums[0], rangeLen: nums[2]})
	}

	return categoryMap{mappings: mappings}, nil
}

func ParseAlmanac(lines []string) (Almanac, error) {
	seeds, err := parseSeedLine(lines[0])
	if err != nil {
		return Almanac{}, err
	}

	almanac := Almanac{seeds: seeds}

	for lineIdx, line := range lines {
		if strings.HasSuffix(line, "map:") {
			data, err := parseMapSection(lines[lineIdx+1:])
			if err != nil {
				return Almanac{}, err
			}

			switch line {
			case "seed-to-soil map:":
				almanac.seedToSoil = data
			case "soil-to-fertilizer map:":
				almanac.soilToFertilizer = data
			case "fertilizer-to-water map:":
				almanac.fertilizerToWater = data
			case "water-to-light map:":
				almanac.waterToLight = data
			case "light-to-temperature map:":
				almanac.lightToTemp = data
			case "temperature-to-humidity map:":
				almanac.tempToHumidity = data
			case "humidity-to-location map:":
				almanac.humidityToLocation = data
			default:
				return Almanac{}, fmt.Errorf("Unexpected map type in line: %v", line)
			}
		}
	}

	return almanac, nil
}

func (almanac Almanac) SeedLocation(seed int) int {
	soil := almanac.seedToSoil.mapSrc(seed)
	// fmt.Printf("seed %v -> soil %v\n", seed, soil)
	fertilizer := almanac.soilToFertilizer.mapSrc(soil)
	// fmt.Printf("soil %v -> fertilizer %v\n", soil, fertilizer)
	water := almanac.fertilizerToWater.mapSrc(fertilizer)
	// fmt.Printf("fertilizer %v -> water %v\n", fertilizer, water)
	light := almanac.waterToLight.mapSrc(water)
	// fmt.Printf("water %v -> light %v\n", water, light)
	temp := almanac.lightToTemp.mapSrc(light)
	// fmt.Printf("light %v -> temp %v\n", light, temp)
	humidity := almanac.tempToHumidity.mapSrc(temp)
	// fmt.Printf("temp %v -> humidity %v\n", temp, humidity)
	loc := almanac.humidityToLocation.mapSrc(humidity)
	// fmt.Printf("humidity %v -> location %v\n", humidity, loc)

	return loc
}
