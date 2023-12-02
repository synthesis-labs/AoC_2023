import re

with open('data/input.txt', 'r') as infile:
    data = [x for x in infile.readlines()]

max = {'red': 12, 'green': 13, 'blue': 14}

acc = 0
acc2 = 0
for entry in data:
    b = re.match('Game\s(\d*):(.+)', entry)
    id = b.group(1)
    body = b.group(2)
    s = body.split(';')
    # k = [[[int(x[0]) <= max[x[1]] for x in colours] for colours in re.findall('(\d*)\s(red|green|blue)', draw)] for draw in s]
    k = [[colours for colours in re.findall('(\d*)\s(red|green|blue)', draw)] for draw in s]
    r = [int(x[0]) <= max[x[1]] for y in k for x in y]
    if all(r):
        print(f':{id}:')
        acc += int(id)
    m = {'red': 0, 'green': 0, 'blue': 0}
    for y in k: 
        for x in y:
            m[x[1]] = m[x[1]] if int(x[0]) < m[x[1]] else int(x[0])
    pow = m['red'] * m['green'] * m['blue']
    acc2 += pow

# Game 2: 16 blue, 12 green, 3 red; 13 blue, 2 red, 8 green; 15 green, 3 red, 16 blue\n
# Game 99: 2 green, 20 blue; 12 blue; 3 red, 12 blue; 7 blue; 3 green, 10 blue, 2 red; 3 red, 2 green