import itertools
import math
import time
from typing import Tuple, List


def read_input(file='./test.txt'):
    with open(file, 'r') as inputfile:
        lines = inputfile.read()
        # lines = [line.strip() for line in lines]
    return lines


def longestPalSubstr(l: list):
    # Get length of input String
    n = len(l)

    # All subStrings of length 1
    # are palindromes
    maxLength = 1
    start = 0

    # Nested loop to mark start
    # and end index
    for i in range(n):
        for j in range(i, n):
            flag = 1

            # Check palindrome
            for k in range(0, ((j - i) // 2) + 1):
                if l[i + k] != l[j - k]:
                    flag = 0

            # Palindrome
            if flag != 0 and (j - i + 1) > maxLength:
                start = i
                maxLength = j - i + 1

    print(f"Longest palindrome substring is {maxLength}: {l[start:start + maxLength]} starting at {start}")
    # printSubStr(l, start, start + maxLength )

    # Return length of LPS
    return start, maxLength


def fold(nums: list) -> int:
    # let's use a stack to fold the list
    stack = []
    pops = 0
    col = 0
    pushing = True
    for i, num in enumerate(nums):
        if len(stack) == 0:
            stack.append(num)
            pushing = True
        elif stack[-1] == num:
            stack.pop()
            if pushing:
                col = i
                pushing = False
            pops += 1
        else:
            stack.append(num)
            pushing = True
    print(f"stack: {stack}")
    return col


def true_fold(nums: list) -> list[int]:
    folds = []
    # print(f"nums: {nums}")
    for fold in range(1, len(nums)):
        left = nums[:fold]
        left.reverse()
        right = nums[fold:]
        # print(f"left: {left}, right: {right}")
        length = min(len(left), len(right))
        for j in range(length):
            if left[j] != right[j]:
                break
        else:
            folds.append(fold)

    return folds


def solve(block):
    c_fold, r_fold = get_folds(block)

    if not r_fold and not c_fold:
        print("*** Tie ***")
        raise Exception("tie")

    if len(c_fold) > len(r_fold):
        return c_fold[0]
    else:
        return r_fold[0] * 100


def get_folds(block) -> tuple[list[int], list[int]]:
    # solve rows
    # print("Rows")

    b_nums = [int(l, 2) for l in block]
    # print(b_nums)
    r_folds = true_fold(b_nums)
    # print("Cols")
    # solve columns
    b_nums = [int(''.join([l[i] for l in block]), 2) for i in range(len(block[0]))]
    # print(b_nums)
    c_folds = true_fold(b_nums)
    return c_folds, r_folds


def solve2(block):
    # Find the original mirror line
    c_fold, r_fold = get_folds(block)
    # flip bits one by one until we find a different r_fold
    for r in range(len(block)):
        for c in range(len(block[0])):
            # copy the block
            copy = [l for l in block]
            # flip the bit at r,c
            copy[r] = copy[r][:c] + ('1' if copy[r][c] == '0' else '0') + copy[r][c+1:]
            # print(f"r: {r}, c: {c}")
            # [print(l) for l in copy]
            # print()
            c_fold2, r_fold2 = get_folds(copy)
            if r_fold2 and r_fold2 != r_fold:
                f2 = set(r_fold2)
                f1 = set(r_fold)
                new_folds = f1 ^ f2
                print(f"New row fold! {new_folds}. Original folds: col: {c_fold}, row: {r_fold}. New folds: col: {c_fold2}, row: {r_fold2}")
                return new_folds.pop() * 100
            if c_fold2 and c_fold2 != c_fold:
                f2 = set(c_fold2)
                f1 = set(c_fold)
                new_folds = f1 ^ f2
                print(f"New column Fold!. Original folds: col: {c_fold}, row: {r_fold}. New folds: col: {c_fold2}, row: {r_fold2}")
                return new_folds.pop()
    print("No solution found")
    [print(l) for l in block]


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


def solve1(block):
    fold = get_fold1(block)
    if fold > -1:
        return fold * 100
    # Find mirror in columns
    transposed = [''.join([block[j][i] for j in range(len(block))]) for i in range(len(block[0]))]
    # print(f"transposed: {transposed}")
    fold = get_fold1(transposed)
    if fold > -1:
        return fold
    return 0


def solve2_2(block):
    fold = get_fold1(block, 1)
    if fold > -1:
        return fold * 100
    # Find mirror in columns
    transposed = [''.join([block[j][i] for j in range(len(block))]) for i in range(len(block[0]))]
    # print(f"transposed: {transposed}")
    fold = get_fold1(transposed, 1)
    if fold > -1:
        return fold
    return 0


if __name__ == '__main__':
    # tests
    ind = true_fold([1, 1, 2, 3, 3, 2])
    print(f"ind: {ind}")
    assert ind[0] == 1, f"ind: {ind}"
    assert ind[1] == 4, f"ind: {ind}"
    ind = true_fold([1, 2, 3, 3, 2, 4])
    print(f"ind: {ind}")
    assert ind == [], f"ind: {ind}"
    ind = true_fold([1, 1, 2, 4, 5, 6])
    print(f"ind: {ind}")
    assert ind[0] == 1, f"ind: {ind}"
    ind = true_fold([int('100011001',2), int('000001001',2), int('001100111',2), int('111110110',2), int('111110110',2), int('001100111',2), int('100001001',2)])
    print(f"ind: {ind}")
    assert ind == [], f"ind: {ind}"

    input = read_input('./input.txt')
    blocks = input.split('\n\n')
    blocks = [block.split('\n') for block in blocks]
    blocks = [[l for l in block] for block in blocks]
    ans = sum([solve1(b) for b in blocks])
    print(f"Answer 1: {ans}")

    ans2 = sum([solve2_2(b) for b in blocks])
    print(f"Answer 2: {ans2}")

