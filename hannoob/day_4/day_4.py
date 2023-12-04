from toolz import thread_last
import re

def read_lines_from_file(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    return lines

def str_to_list(line):
    return [i for i in line.strip().split(' ') if i != '']

def parse_card(line):
    parts = line.split(':')[1].split('|')
    return list(map(str_to_list, parts))

def count_matches(card):
    return len(set(card[0]).intersection(card[1]))

def get_value(n):
    return int(2**(n - 1))

def count_cards(matches, index):
    score = matches[index]
    copy_index = range(index + 1,index + score + 1)

    return 1 + sum([count_cards(matches, i) for i in copy_index])

def calculate_problem1(lines):
    return thread_last(
        lines,
        (map, parse_card),
        (map, count_matches),
        (map, get_value),
        sum
    )
    

def calculate_problem2(lines):
    def count_cards_map(index):
        return count_cards(matches, index)

    cards = map(parse_card, lines)
    matches = list(map(count_matches, cards))
    count = list(map(count_cards_map, range(len(matches))))

    return sum(count)
    
    

lines = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".split('\n')

# lines = read_lines_from_file('input.txt')

lines = list(map(lambda l: l.strip(), lines))
print(f"problem 1: {calculate_problem1(lines)}")
print(f"problem 2: {calculate_problem2(lines)}")