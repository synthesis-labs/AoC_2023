
def read_input(file='./test.txt'):
    with open(file, 'r') as inputfile:
        lines = inputfile.read()
    return lines


def get_fold1(block, target=0):
    # Find mirror in rows
    for fold in range(1, len(block)):
        top = block[:fold]
        top.reverse()
        bottom = block[fold:]
        # print(f"top: {top}, bottom: {bottom}")
        score = 0
        for i in range(min(len(top), len(bottom))):
            for j in range(len(top[i])):
                if top[i][j] != bottom[i][j]:
                    score += 1
        if score == target:
            return fold
    return -1


def solve1(block, target=0):
    fold = get_fold1(block, target)
    if fold > -1:
        return fold * 100
    # Find mirror in columns
    transposed = [''.join([block[j][i] for j in range(len(block))]) for i in range(len(block[0]))]
    # print(f"transposed: {transposed}")
    fold = get_fold1(transposed, target)
    if fold > -1:
        return fold
    return 0


if __name__ == '__main__':
    input = read_input('./input.txt')
    blocks = input.split('\n\n')
    blocks = [block.split('\n') for block in blocks]

    ans = sum([solve1(b) for b in blocks])
    print(f"Answer 1: {ans}")

    ans2 = sum([solve1(b, 1) for b in blocks])
    print(f"Answer 2: {ans2}")

