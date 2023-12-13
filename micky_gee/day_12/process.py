import re
from tqdm import tqdm
import copy

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

def score(line):
    return [len(x) for x in line.split('.') if len(x) > 0]

def permute(line, key):
    l = re.sub('\?', '{}', line)
    r = len(l) - len(line) #number of replacements or number of wildcard characters
    s = re.sub('1', '#', re.sub('0', '.', f'{key:>0{r}b}'))
    return l.format(*[x for x in s])

#part 1
# acc = 0
# for line in tqdm(data):
#     s,d = line.split(' ')
#     s = ''.join([s for x in range(1)])
#     d = [int(x) for x in re.findall('\d+', d)]
#     for k in tqdm(range(2**len(re.findall('\?', s)))):
#         if score(permute(s, k)) == d:
#             acc += 1
    # print(f'{line} --> {s} {s==d}')

def leading_periods(leadings, length):
    lp = copy.deepcopy(leadings)
    if len(leadings) == 0:
        return ''.join(['.' for d in range(length)])
    l,c = lp.pop(0)
    return ''.join(['.' for d in range(l)] + ['#' for e in range(c)]) + leading_periods(lp, length-l-c)

def reducing_sum(k, i):
    return(([k-x for x in range(i)]))

def arithmetric_series(n):
    return n*(1+n)//2

#part 2

s,d = data[18].split(' ')
s = ''.join([s+'?' for x in range(5)])[:-1]
d = [int(x) for x in re.findall('\d+', d)]
d = d*5

leadings = [(1, c) for c in d]
leadings[0] = (0, leadings[0][1])

while True:
    # track level.
    level = -1
    if sum([a+b for a,b in leadings]) > len(s):
        for x in range(len(leadings)):
            leadings[-x] = (1, leadings[-x][1]) 



# leading_periods(leadings, len(s))