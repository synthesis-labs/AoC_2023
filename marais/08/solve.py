import math
import re


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


def solve(instructions, current):
    steps = 0
    inst_pointer = 0
    while True:
        inst_pointer = inst_pointer % len(instructions)
        instruction = instructions[inst_pointer]
        steps += 1
        inst_pointer += 1
        if instruction == 'L':
            current = current.left
        else:
            current = current.right
        if current.name.endswith('Z'):
            return steps


if __name__ == '__main__':
    lines = read_input('input1.txt')
    instructions = lines[0].strip()
    graph = make_graph(lines[2:])
    # Part 1
    start_node = graph['AAA']
    answ1 = solve(instructions, start_node)
    print(f"Answer 1: {answ1}")
    # Part 2
    start_nodes = [node for node in graph.values() if node.name.endswith('A')]
    counts = [solve(instructions, start_node) for start_node in start_nodes]
    lcm = math.lcm(*counts)
    # print(f"Cycle lengths: {','.join([str(cycle_length) for cycle_length in counts])}")
    # print(f"LCM: {lcm}")
    print(f"Answer 2: {lcm}")
