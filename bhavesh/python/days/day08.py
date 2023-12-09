import re
import itertools
import math

from util import get_question_data

def solve():
  data = get_question_data(2023, 8)

  return {
    "sampleDataPart1": sampleDataPart1(),
    "sampleDataPart2": sampleDataPart2(),
    "part1": part1(data),
    "part2": part2(data),
  }

def part1(data):
  data = data.split("\n\n")
  map = data[1].split("\n")
  map = [re.findall(r"\w+", x) for x in map]
  map = {x[0]: {"L": x[1], "R": x[2]} for x in map if x != []}
  instructions = re.findall(".", data[0])

  instruction = itertools.cycle(instructions)
  count = 0
  node = "AAA"

  while node != 'ZZZ':
    branch = next(instruction)
    count += 1
    node = map[node][branch]  
  return count

def part2(data):
  data = data.split("\n\n")
  map = data[1].split("\n")
  map = [re.findall(r"\w+", x) for x in map]
  map = {x[0]: {"L": x[1], "R": x[2]} for x in map if x != []}
  instructions = re.findall(".", data[0])

  nodes = [x for x in map.keys() if x[-1] == 'A']
  counts = [findGraph(x, map, instructions) for x in nodes]

  return math.lcm(*counts)

def sampleDataPart1():
  data = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"

  return part1(data)

def sampleDataPart2():
  data = "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)\n"

  return part2(data)

def findGraph(node, map, instructions):
  instruction = itertools.cycle(instructions)
  count = 0
  while node[-1] != 'Z':
    branch = next(instruction)
    count += 1
    node = map[node][branch]
  return count
