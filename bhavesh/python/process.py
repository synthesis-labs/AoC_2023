import re
import itertools
import math

with open("part1.txt") as inFile:  
  data = inFile.read().split("\n\n")

map = data[1].split("\n")
map = [re.findall("\w+", x) for x in map]
map = {x[0]: {"L": x[1], "R": x[2]} for x in map}
instructions = re.findall(".", data[0])

def findGraph(node):
  instruction = itertools.cycle(instructions)
  count = 0
  # while nodes != "ZZZ":
  while node[-1] != 'Z':
    branch = next(instruction)
    count += 1

    node = map[node][branch]

    print(f"{count}: {nodes}")  
  return count

nodes = [x for x in map.keys() if x[-1] == 'A']
counts = [findGraph(x) for x in nodes]

print(math.lcm(*counts))
