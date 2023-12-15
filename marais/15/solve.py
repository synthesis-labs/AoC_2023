
def read_input(file='./test.txt'):
    with open(file, 'r') as inputfile:
        lines = inputfile.read()
    return lines


def my_hash(string):
    acc = 0
    for c in string:
        acc += ord(c)
        acc *= 17
        acc = acc % 256
    return acc


if __name__ == '__main__':

    input = read_input('input.txt')
    # lines = [block.split('\n') for block in input]
    strings = input.split(',')
    ans = sum([my_hash(string) for string in strings])
    print(f"Answer 1: {ans}")
    #
    # ans2 = solve2(lines, 1000000000)
    # print(f"Answer 2: {ans2}")
