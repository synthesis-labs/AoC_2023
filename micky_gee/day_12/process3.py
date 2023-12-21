import re
from tqdm import tqdm
import copy

with open('data/test.txt', 'r') as infile:
    data = infile.read().split('\n')


#sequence is a sequence of characters of either '.', '#', or '?'
#groups is a list of integers indicating the length of the number contiguous blocks of ['#' | '?'] (i.e. separated by '.')
#a '?' in the sequence can be either of '#' or '.'
#the depth first count finds the number of possible sequences that can be generated from the given sequence that satisfies the group definition
def depth_first_count(sequence, groups):
    #if we have more groups than sequence, then we can't satisfy the group definition
    if sum(groups) > len(sequence):
        return 0
    #gobble enough to satisfy the first group (what to do about the separating '.'?)

total = 0
for dat in data:
    s,d = dat.split(' ')
    sequence = '?'.join([s]*5)
    groups = [int(x) for x in re.findall('\d+', d)] * 5

    print(f'{mask} {groups}')
