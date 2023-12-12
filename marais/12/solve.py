import itertools


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


def solve1(line):
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
    sol_count = 0
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
            # print(f"found {candidate:0{exp}b}")
            sol_count += 1

    print(f"{line['s']} -> {sol_count}")
    return sol_count


def expand(lines):
    for line in lines:
        line['s'] = '?'.join(line['s'] for i in range(5))
        line['g'] = line['g'] * 5
    return lines


if __name__ == '__main__':

    # solve1({'s': '???.###', 'g': [1, 1, 3]})

    lines = read_input('test.txt')
    lines = parse(lines)
    answ1 = sum([solve1(line) for line in lines])
    print(f"Answer 1: {answ1}")

    lines = expand(lines)
    # print(lines)
    answ2 = sum([solve1(line) for line in lines])
    print(f"Answer 2: {answ2}")