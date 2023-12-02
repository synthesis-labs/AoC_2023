def read_input(file='./test.txt') -> list[str]:
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
    return lines


max = {'red': 12, 'green': 13, 'blue': 14}


def is_valid_game(game):
    rounds = game.split(':')[1].split(';')
    for r in rounds:
        colours = r.split(',')
        for c in colours:
            cc = c.strip().split(' ')
            c = cc[1]
            n = cc[0]
            if int(n) > max[c]:
                return False
    return True


def solve_part_1(games):
    acc = 0
    for game in games:
        g_num = game.split(':')[0].split(' ')[1]
        if is_valid_game(game):
            acc += int(g_num)
    return acc


def solve_part_2(games):
    acc = 0
    for game in games:
        min = {'red': 0, 'green': 0, 'blue': 0}
        rounds = game.split(':')[1].split(';')
        for r in rounds:
            colours = r.split(',')
            for c in colours:
                cc = c.strip().split(' ')
                c = cc[1]
                n = cc[0]
                if int(n) > min[c]:
                    min[c] = int(n)
        power = min['red'] * min['green'] * min['blue']
        acc += power
    return acc


if __name__ == '__main__':
    strings = read_input('input1.txt')
    answ1 = solve_part_1(strings)
    print(f"Answer 1: {answ1}")

    strings = read_input('input2.txt')
    answ2 = solve_part_2(strings)
    print(f"Answer 2: {answ2}")
