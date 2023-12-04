import re
import numpy as np
import pprint as pp

with open('data/input.txt', 'r') as infile:
    data = infile.read().split('\n')

symbols = [re.sub('\d', '.', x) for x in data]
listofmatches = []

acc = 0
for d, i in zip(data, range(len(data))):
    r = re.finditer('\d+', d)
    for a in r:
        # print(a)
        top = i-1 if i >= 1 else 0
        left = a.span(0)[0]-1 if a.span(0)[0]-1 >= 1 else 0
        bottom = i + 1 if i < len(data) else len(data)
        right = a.span(0)[1] if a.span(0)[1] < len(d) else len(d)
        listofmatches += [{'top':top, 'left':left, 'bottom':bottom, 'right':right, 'value':int(a.group(0))}]
        # print(f'({left},{top})-({right},{bottom})')
        part = False
        for l in symbols[top:bottom+1]:
            part |= len(l[left:right+1].strip('.')) > 0
        if part:
            acc += int(a.group(0))

pp.pprint(listofmatches)

acc2 = 0
for d, i in zip(symbols, range(len(symbols))):
    r = re.finditer('\*', d)
    for a in r:
        c = a.span(0)[0]
        vals = []
        for m in listofmatches:
            if m['top'] <= i <= m['bottom'] and m['left'] <= c <= m['right']:
                vals += [m['value']]
        if (len(vals) == 2):
            acc2 += (vals[0] * vals[1])
        print(f'({c}, {i}):{vals} : {acc2}')


    #5946096
    #5946096
    #6746090

    #try this one
    #84289137