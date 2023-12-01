def read_input(file='./test.txt') -> list[str]:
    # Read a txt file from 'file' and iterate over the lines
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
    lines = [l.strip('\n\r') for l in lines if l.strip('\n\r') != '']
    return lines


strings_to_find = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', '1', '2', '3', '4', '5', '6', '7', '8', '9']


def find_first(string: str, match_list: list[str]) -> str:
    for i in range(len(string)):
        for m in match_list:
            if string[i:].startswith(m):
                return m


def find_last(string: str, match_list: list[str]) -> str:
    for i in range(len(string) - 1, -1, -1):
        for m in match_list:
            if string[i:].startswith(m):
                return m


def solve_part_1(strings):
    sum = 0
    for line in strings:
        first_digit = find_first(line, strings_to_find[9:])
        last_digit = find_last(line, strings_to_find[9:])
        num = int(f"{first_digit}{last_digit}")
        sum += num
    return sum


def solve_part_2(strings):
    sum = 0
    for line in strings:
        first = find_first(line, strings_to_find)
        first_int = convert_to_int(first)
        last = find_last(line, strings_to_find)
        second_int = convert_to_int(last)
        num = int(f"{first_int}{second_int}")
        sum += num
    return sum


def convert_to_int(string):
    if string.isdigit():
        return int(string)
    else:
        return strings_to_find.index(string) + 1


if __name__ == '__main__':
    # Answer 1
    strings = read_input('input1.txt')
    answ1 = solve_part_1(strings)
    print("Answer 1")
    print(answ1)

    # Answer 2
    strings = read_input('input2.txt')
    answ2 = solve_part_2(strings)
    print("Answer 2")
    print(answ2)
