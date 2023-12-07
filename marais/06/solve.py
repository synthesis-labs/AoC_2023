import math


def get_wins(race):
    wins = 0
    for speed in range(race['time']):
        rem = race['time'] - speed
        dist = speed * rem
        if dist > race['distance']:
            wins += 1
    return wins


def solve_part_1(races):
    wins = [get_wins(race) for race in races]
    return math.prod(wins)


def solve_part_2(race):
    return get_wins(race)


if __name__ == '__main__':
    lines = [{'time': 56, 'distance': 499}, {'time': 97, 'distance': 2210}, {'time': 77, 'distance': 1097},{'time': 93, 'distance': 1440}]
    answ1 = solve_part_1(lines)
    print(f"Answer 1: {answ1}")

    answ2 = solve_part_2({'time': 56977793, 'distance': 499221010971440})
    print(f"Answer 2: {answ2}")
