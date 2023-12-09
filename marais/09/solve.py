def read_input(file='./test.txt') -> list[str]:
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
        lines = [line.strip() for line in lines]
    return lines

def parse(lines):
    seq = [[int(x) for x in line.split(' ')] for line in lines]
    # print(seq)
    return seq


def solve(seq, pos) -> int:
    # if all values in seq are 0, return 0
    # print(f"seq: {seq}")
    if not any(seq):
        return 0

    diffs = [seq[i + 1] - seq[i] for i in range(len(seq) - 1)]
    # print(diffs)
    next_val = seq[pos] + solve(diffs, pos) if pos else seq[pos] - solve(diffs, pos)
    # print(next_val)
    return next_val


if __name__ == '__main__':
    lines = read_input('input1.txt')
    parsed = parse(lines)
    # Part 1
    vals = [solve(seq, -1) for seq in parsed]
    print(f"Answer 1: {sum(vals)}")
    # Part 2
    vals = [solve(seq, 0) for seq in parsed]
    print(f"Answer 2: {sum(vals)}")
