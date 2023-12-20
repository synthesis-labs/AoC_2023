import itertools
import math


def read_input(file='./test.txt'):
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
        lines = [line.strip() for line in lines]
    return lines


# ???.### 1,1,3
# .??..??...?##. 1,1,3
# ?#?#?#?#?#?#?#? 1,3,1,6
# ????.#...#... 4,1,1
# ????.######..#####. 1,6,5
# ?###???????? 3,2,1
def parse(lines):
    parsed = []
    for line in lines:
        line = line.split(' ')
        s = line[0]
        groups = [int(c) for c in line[1].split(',')]
        parsed.append({'s': s, 'g': groups})
    return parsed


def solve1(line) -> list[int]:
    #    ???.### 1,1,3
    # this is binary where . is 0 and # is 1
    exp = len(line['s'])
    # a mask is created where ? is 1 and . and # are 0
    # for ???.### the mask = 111....
    bin_s = int(line['s'].replace('.', '0').replace('#', '1').replace('?', '0'), 2)
    mask_s = line['s'].replace('.', '1').replace('#', '1').replace('?', '0')
    mask = int(mask_s, 2)
    # print(mask_s)
    # print(bin(mask))
    candidates = []
    solutions = []
    for i in range(2**exp):
        can = (i ^ bin_s) & mask
        # can = can_int & mask
        if can == 0:
            candidates.append(i)
            # print(f"candidate: {i:0{exp}b}")
        # print(f"{i:0{exp}b} ^ {bin_s:0{exp}b} -> {can_int:0{exp}b}  ", end="")
        # print(f"{can_int:0{exp}b} & {mask:0{exp}b} -> {can:0{exp}b}")
    for candidate in candidates:
        # print(f"{candidate:0{exp}b}")
        # count the groups of contiguous 1s
        groups = []
        for k, g in itertools.groupby(f"{candidate:0{exp}b}"):
            if k == '1':
                groups.append(len(list(g)))

        # print(groups)
        # print(line['g'])
        if groups == line['g']:
            print(f"found {candidate:0{exp}b}")
            solutions.append(candidate)

    # print(f"{line['s']} -> {len(solutions)}")
    return solutions


def expand(lines):
    for line in lines:
        join_char = '.' if line['s'].endswith('#') else '?'
        extra_char = '?' if line['s'].startswith('?') and line['s'].endswith('?') else ''
        line['s'] = [line['s']+extra_char] + [join_char+line['s']+extra_char for i in range(4)]
        line['g'] = [line['g'] for i in range(5)]
    return lines


def solve2(line):
    print(f"{line['s']} -> {line['g']}")
    if line['s'].startswith('?') and line['s'].endswith('?'):
        # get solutions so we can reason further
        solutions = solve1({'s': line['s']+'?', 'g': line['g']})
        exp = len(line['s']) + 1
        # if all solutions ends with a 1, the join char should be a .(0) and added to the beginning
        left_char = '.' if all([bin(s).endswith('1') for s in solutions]) else '?' if all([bin(s).endswith('0') for s in solutions]) else ''
        right_char = '' if left_char else '.' if all([f"{s:0{exp}b}".startswith('1') for s in solutions]) else '?' if all([f"{s:0{exp}b}".startswith('0') for s in solutions]) else ''

        if left_char == '' and right_char == '':
            raise Exception("I don't know what to do")

    else:
        # if line['s'] ends in #(1), the join char should be a .(0) and added to the beginning
        # if line['s'] ends in .(0), the join char should be a ? (1 or 0) and should be added to the beginning
        left_char = '.' if line['s'].endswith('#') else '?' if line['s'].endswith('.') else ''
        # if line['s'] starts with a #(1), the join char should be a .(0) and added to the end
        # if line['s'] starts with a .(0), the join char should be a ? (1 or 0) and should be added to the end
        right_char = '' if left_char else '.' if line['s'].startswith('#') else '?' if line['s'].startswith('.') else ''

        if left_char == '' and right_char == '':
            raise Exception("I don't know what to do")

    print(f"{line['s']} -> {left_char}*{line['s']}*{right_char}")

    expanded = [line['s'] + right_char] + [left_char + line['s'] + right_char for i in range(3)] + [left_char + line['s']]
    # line['g'] = [line['g'] for i in range(5)]
    solutions = [len(solve1({'s': s, 'g': line['g']})) for s in expanded]
    # print(f"{line['s']} expanded -> {solutions}")
    return math.prod(solutions)


if __name__ == '__main__':

    # solve1({'s': '???.###', 'g': [1, 1, 3]})

    lines = read_input('test.txt')
    lines = parse(lines)
    # answ1 = sum([len(solve1(line)) for line in lines])
    # print(f"Answer 1: {answ1}\n\n")

    # lines = expand(lines)
    # print(lines)
    answ2 = [solve2(line) for line in lines]
    print(f"Answer 2: {sum(answ2)}")
