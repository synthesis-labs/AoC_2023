import re


def read_input(file='./test.txt') -> list[str]:
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
    return lines

### Rows
# 467....114
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..

def solve_part_1(rows):
    sum = 0
    for i, line in enumerate(rows):
        # Find all number matches
        matches = re.finditer(r'(\d+)', line)
        rowcount = len(rows)
        colcount = len(line) - 1   # -1 because of newline
        # create a mask around the match
        for match in matches:
            # print(f"{match.group(1)}, {match.start(1)}, {match.end(1)}")
            match_left = max(0, match.start(1)-1)
            match_right = min(colcount, match.end(1)+1)
            concat = ""
            # cells above
            if i > 0:
                concat += rows[i-1][max(0,match_left):min(colcount, match_right)]
            # cells on the same row
            concat += rows[i][max(0,match_left):min(colcount, match_right)]
            # cells below
            if i < rowcount - 1:
                concat += rows[i+1][max(0,match_left):min(colcount, match_right)]
            if re.search(r'[^.\d]', concat):
                print(f"{concat} : True")
                sum += int(match.group(1))
                print(f"Sum: {sum}")
                # break
            else:
                print(f"{concat} : False")
    print(f"Sum: {sum}")
    return sum


if __name__ == '__main__':
    strings = read_input('input1.txt')
    answ1 = solve_part_1(strings)
    print(f"Answer 1: {answ1}")
    #
    # strings = read_input('input2.txt')
    # answ2 = solve_part_2(strings)
    # print(f"Answer 2: {answ2}")