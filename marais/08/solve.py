import re
from math import gcd


def read_input(file='./test.txt') -> list[str]:
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()

    return lines


class Node:
    def __init__(self, name):
        self.name = name
        self.left = None
        self.right = None

    def __repr__(self):
        return f"Node {self.name}, left: {self.left.name if self.left else '*'}, right: {self.right.name if self.right else '*'}"


def make_graph(lines):
    graph_nodes = {}
    for line in lines:
        name, left, right = re.match(r'(\w+) = \((\w+), (\w+)\)', line).groups()
        if name not in graph_nodes:
            graph_nodes[name] = Node(name)
        if left not in graph_nodes:
            graph_nodes[left] = Node(left)
        if right not in graph_nodes:
            graph_nodes[right] = Node(right)
        graph_nodes[name].left = graph_nodes[left]
        graph_nodes[name].right = graph_nodes[right]
    return graph_nodes


def solve(lines):
    instructions = lines[0].strip()
    graph = make_graph(lines[2:])
    current = graph['AAA']
    steps = 0
    inst_pointer = 0
    while True:
        instruction = instructions[inst_pointer] if inst_pointer < len(instructions) else instructions[inst_pointer % len(instructions)]
        steps += 1
        inst_pointer += 1
        if instruction == 'L':
            current = current.left
            if current.name == 'ZZZ':
                return steps
        else:
            current = current.right
            if current.name == 'ZZZ':
                return steps


def solve2(lines):
    instructions = lines[0].strip()
    graph = make_graph(lines[2:])
    # all nodes with  names ending in A
    traversals = [node for node in graph.values() if node.name.endswith('A')]
    traversals_cycle_length = [0, 0, 0, 0, 0, 0]
    traversal_done = [False, False, False, False, False, False]
    steps = 0
    inst_pointer = 0
    while True:
        next_traversals = []
        instruction = instructions[inst_pointer] if inst_pointer < len(instructions) else instructions[
            inst_pointer % len(instructions)]
        for i, current in enumerate(traversals):
            if instruction == 'L':
                next_traversals.append(current.left)
            else:
                next_traversals.append(current.right)
            if current.name.endswith('Z'):
                print(f"Traversals {i} reached {current.name} after {traversals_cycle_length[i]} steps")
                traversal_done[i] = True
            if not traversal_done[i]:
                traversals_cycle_length[i] += 1
        steps += 1
        inst_pointer += 1
        traversals = next_traversals
        # if steps % 100000 == 0:
        #     print(f"Step {steps}, {','.join([traversal.name for traversal in traversals])}")
        if all(traversal_done):
            # calculate the lowest common multiple of the cycle lengths
            lcm = 1
            for cycle_length in traversals_cycle_length:
                lcm = lcm * cycle_length // gcd(lcm, cycle_length)
            print(f"Cycle lengths: {','.join([str(cycle_length) for cycle_length in traversals_cycle_length])}")
            print(f"LCM: {lcm}")
            return lcm


if __name__ == '__main__':
    lines = read_input('input1.txt')
    answ1 = solve(lines)
    print(f"Answer 1: {answ1}")

    answ2 = solve2(lines)
    print(f"Answer 2: {answ2}")
