import re


def read_input(file='./test.txt') -> list[str]:
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
    return lines

### Part 1
### The engine schematic (your puzzle input) consists of a visual representation of the engine.
# There are lots of numbers and symbols you don't really understand,
# but apparently any number adjacent to a symbol, even diagonally, is a "part number"
# and should be included in your sum. (Periods (.) do not count as a symbol.)
# 467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598..
def solve_part_1(strings):
    loc_to_id = {}
    id_to_val = {}
    num_id = 0
    for i, line in enumerate(strings):
        # Find all number matches
        matches = re.finditer(r'(\d+)', line)
        # Get the numbers
        for match in matches:
            num_id += 1
            # print(match.group(1))
            for x in range(match.span()[0], match.span()[1]):
                # print(range)
                loc_to_id[(x,i)] = str(num_id)
                id_to_val[str(num_id)] = int(match.group(1))

    # print(loc_to_id)
    acc = 0
    # match any symbol except period or digit
    for i, line in enumerate(strings):
        symbol_matches = re.finditer(r'\*', line)
        for match in symbol_matches:
            adjacent_numbers = set()
            ratio = 0
            # adjacent_numbers = set()
            # check left and right
            if (match.span()[0]-1, i) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0]-1, i)]}:{id_to_val[loc_to_id[(match.span()[0]-1, i)]]} to the left of {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0]-1, i)]])
            if (match.span()[0]+1, i) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0]+1, i)]}:{id_to_val[loc_to_id[(match.span()[0]+1, i)]]} to the right of {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0]+1, i)]])
            # check up and down
            if (match.span()[0], i-1) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0], i-1)]}:{id_to_val[loc_to_id[(match.span()[0], i-1)]]} above {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0], i-1)]])
            if (match.span()[0], i+1) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0], i+1)]}:{id_to_val[loc_to_id[(match.span()[0], i+1)]]} below {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0], i+1)]])
            # Check diagonals
            if (match.span()[0]-1, i-1) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0]-1, i-1)]}:{id_to_val[loc_to_id[(match.span()[0]-1, i-1)]]} above left of {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0]-1, i-1)]])
            if (match.span()[0]+1, i-1) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0]+1, i-1)]}:{id_to_val[loc_to_id[(match.span()[0]+1, i-1)]]} above right of {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0]+1, i-1)]])
            if (match.span()[0]-1, i+1) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0]-1, i+1)]}:{id_to_val[loc_to_id[(match.span()[0]-1, i+1)]]} below left of {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0]-1, i+1)]])
            if (match.span()[0]+1, i+1) in loc_to_id:
                print(f"Found number {loc_to_id[(match.span()[0]+1, i+1)]}:{id_to_val[loc_to_id[(match.span()[0]+1, i+1)]]} below right of {match.group(0)}")
                adjacent_numbers.add(id_to_val[loc_to_id[(match.span()[0]+1, i+1)]])
            # print(match.span())
            print(adjacent_numbers)
            if len(adjacent_numbers) > 1:
                ratio = 1
                for x in adjacent_numbers:
                    ratio *= x
                acc += ratio

    return acc


    # numbers = [int(match.group(1)) for match in matches]
    # print(numbers)

if __name__ == '__main__':
    strings = read_input('input1.txt')
    answ1 = solve_part_1(strings)
    print(f"Answer 1: {answ1}")
    #
    # strings = read_input('input2.txt')
    # answ2 = solve_part_2(strings)
    # print(f"Answer 2: {answ2}")