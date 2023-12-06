import re
import math

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

times = [int(x) for x in re.findall('\d+', re.sub('\s', '',data[0]))]
distances = [int(x) for x in re.findall('\d+', re.sub('\s', '',data[1]))]

r = [   sum([distance < x*(time - x) for x in range(0, time)])
     for time, distance in zip(times, distances)]

print(math.prod(r))