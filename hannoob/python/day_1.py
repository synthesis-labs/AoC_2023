from toolz import thread_last
import re

def read_lines_from_file(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    return lines

def convert_strings_to_numbers(line):
    numbers = {"one": 1, "two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7, "eight": 8, "nine": 9, "zero": 0}
    
    for number,value in numbers.items():
        line = line.replace(number, f'{number}{value}{number}')

    return line

def calculate_number_from_string(line):
    digits = [c for c in line if c.isdigit()]
    return int(digits[0] + digits[-1])

def calculate_problem1(lines):
    return thread_last(
        lines,
        (map, calculate_number_from_string),
        sum
    )

def calculate_problem2(lines):
    return thread_last(
        lines,
        (map, convert_strings_to_numbers),
        (map, calculate_number_from_string),
        sum
    )

lines = read_lines_from_file('input.txt')
print(f"problem 1: {calculate_problem1(lines)}")
print(f"problem 2: {calculate_problem2(lines)}")