import re

def get_sum(file, pattern, numbermap: dict = {}):
    with open(file) as f:
        return sum([int([numbermap.get(m.group(1), m.group(1)) for m in re.finditer(pattern, line)][0] + [numbermap.get(m.group(1), m.group(1)) for m in re.finditer(pattern, line)][-1]) for line in f])

numbers = {'one': '1', 'two': '2', 'three': '3', 'four': '4', 'five': '5', 'six': '6', 'seven': '7', 'eight': '8', 'nine': '9'}
print('Part 1:', get_sum('input1.txt', r'(\d)'))
print('Part 2:', get_sum('input2.txt', r'(?=(\d|one|two|three|four|five|six|seven|eight|nine))', numbers))