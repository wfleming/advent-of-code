#!/usr/bin/env python

from sys import argv
from enum import Enum
import re

class Action(Enum):
    TOGGLE = "toggle"
    TURN_ON = "turn on"
    TURN_OFF = "turn off"

class Pos:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class Command:
    def __init__(self, action, from_pos, to_pos):
        self.action = action
        self.from_pos = from_pos
        self.to_pos = to_pos

PAT = re.compile("(toggle|turn off|turn on) (\d+),(\d+) through (\d+),(\d+)")

def new_grid(init_val):
    grid = []
    for i in range(0, 1_000):
        grid.append([init_val] * 1_000)
    return grid

def parse_cmd(cmd_str):
    m = PAT.match(cmd_str)
    if not m:
        raise Exception(f"pattern didn't match '{cmd_str}'")
    if m[1] == "toggle":
        act = Action.TOGGLE
    elif m[1] == "turn on":
        act = Action.TURN_ON
    elif m[1] == "turn off":
        act = Action.TURN_OFF
    else:
        raise Exception("invalid action from command")
    return Command(
            act,
            Pos(int(m[2]), int(m[3])),
            Pos(int(m[4]), int(m[5])),
            )

def apply_p1(grid, action, x, y):
    if action == Action.TOGGLE:
        grid[x][y] = not grid[x][y]
    elif action == Action.TURN_ON:
        grid[x][y] = True
    elif action == Action.TURN_OFF:
        grid[x][y] = False
    else:
        raise Exception("invalid action in apply_p1")

def apply_p2(grid, action, x, y):
    if action == Action.TOGGLE:
        grid[x][y] = grid[x][y] + 2
    elif action == Action.TURN_ON:
        grid[x][y] = grid[x][y] + 1
    elif action == Action.TURN_OFF:
        grid[x][y] = max(0, grid[x][y] - 1)
    else:
        raise Exception("invalid action in apply_p2")

def apply_range(grid, cmd, fn):
  for x in range(cmd.from_pos.x, cmd.to_pos.x + 1):
    for y in range(cmd.from_pos.y, cmd.to_pos.y + 1):
        fn(grid, cmd.action, x, y)

def apply_all(grid, commands, fn):
    for c in commands:
        apply_range(grid, c, fn)

def count_lit(grid):
    cols_sums = [
        sum([1 for cell in col if cell == True])
        for col in grid
        ]
    return sum(cols_sums)

def total_brightness(grid):
    cols_sums = [sum(col) for col in grid]
    return sum(cols_sums)

with open(argv[1], "r") as fh:
    cmds = [parse_cmd(l) for l in fh.readlines()]
    grid1 = new_grid(False)
    apply_all(grid1, cmds, apply_p1)
    print(f"p1: {count_lit(grid1)} lights are on")

    grid2 = new_grid(0)
    apply_all(grid2, cmds, apply_p2)
    print(f"p2: total brightness is {total_brightness(grid2)}")
