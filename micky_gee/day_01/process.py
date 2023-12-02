import re

with open('data/input.txt', 'r') as infile:
    data = infile.readlines()

subs = {
    'one' : '1',
    'two' : '2',
    'three' : '3',
    'four' : '4',
    'five' : '5',
    'six' : '6',
    'seven' : '7',
    'eight' : '8',
    'nine' : '9',
}

p = []

for x in data:
    r = ''
    for char in x:
        r += char
        # print(r)
        for k, v in subs.items():
            r = r.replace(k, v)
    ar = r
    r = ''
    for char in reversed(x):
        r = char + r
        for k, v in subs.items():
            r = r.replace(k, v)
    p += [ar + r]

acc = 0
for r, x in zip(data, p):
    match = re.findall('(\d)', x)
    o = match[0] + match[-1]

    print(f'{r} :: {x} :: {o}')
    acc += int(o)


print(acc)

