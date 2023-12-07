from sympy import sympify, nroots
import re
import math

with open('input1.txt', 'r') as infile:
    data = infile.read().split('\n')


def solve_eq(eq: str) -> int:
    expr = sympify(eq)
    rr = nroots(expr, maxsteps=50)
    # print(f"{expr} Roots: {rr}")
    return math.ceil(rr[1]) - math.floor(rr[0]) - 1


times = [int(x) for x in re.findall(r'\d+', data[0])]
distances = [int(x) for x in re.findall(r'\d+', data[1])]

part1 = math.prod([solve_eq(f'x*({time}-x) - {distance}') for time, distance in zip(times, distances)])
print(f"Answer 1: {part1}")

times = [int(x) for x in re.findall(r'\d+', re.sub(r'\s', '', data[0]))]
distances = [int(x) for x in re.findall(r'\d+', re.sub(r'\s', '', data[1]))]

part2 = sum([solve_eq(f'x*({time}-x) - {distance}') for time, distance in zip(times, distances)])
print(f"Answer 2: {part2}")

