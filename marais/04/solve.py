import re


def read_input(file='./test.txt') -> list[str]:
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
    return lines


def solve_part_1(lines):
    acc = 0
    # Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    # In the above example, card 1 has five winning numbers
    # (41, 48, 83, 86, and 17) and eight numbers you have (83, 86, 6, 31, 17, 9, 48, and 53).
    # Of the numbers you have, four of them (48, 83, 17, and 86) are winning numbers!
    # That means card 1 is worth 8 points (1 for the first match, then doubled three times for each of the three matches after the first).
    for line in lines:
        pow = 0
        card_lines = re.split(r':|\|', line)
        win = card_lines[1].split(' ')
        mine = card_lines[2].split(' ')
        win = [int(x.strip()) for x in win if x.strip().isnumeric()]
        mine = [int(x.strip()) for x in mine if x.strip().isnumeric()]
        for m in mine:
            if m in win:
                pow +=1
        print(pow)
        if pow > 0:
            acc += 2 ** (pow - 1)
    return acc

def card_wins(card) -> int:
    card_lines = re.split(r':|\|', card)
    win = card_lines[1].split(' ')
    mine = card_lines[2].split(' ')
    win = [int(x.strip()) for x in win if x.strip().isnumeric()]
    mine = [int(x.strip()) for x in mine if x.strip().isnumeric()]
    wins = 0
    for m in mine:
        if m in win:
            wins += 1
    return wins


# count = 0
def solve_part_2(name, cards, low, high) -> int:
    acc = len(cards[low:high])
    # print card numbers
    # child_wins = 0

    if acc == 0:
        return 0

    # print(f"{name}:{[card[:8] for card in cards[low:high]]}")
    for i, card in enumerate(cards[low:high]):
        wins = card_wins(card)
        acc += solve_part_2(cards[i+low][:8], cards, i+low+1, i+low+1+wins)
    return acc


if __name__ == '__main__':
    # strings = read_input('test.txt')
    # answ1 = solve_part_1(strings)
    # print(f"Answer 1: {answ1}")

    cards = read_input('input2.txt')
    answ2 = solve_part_2('first', cards, 0, len(cards))
    print(f"Answer 2: {answ2}")
