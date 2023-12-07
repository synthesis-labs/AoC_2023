def read_input(file='./test.txt') -> str:
    with open(file, 'r') as inputfile:
        lines = inputfile.read()
    return lines


class CamelHand:
    def __init__(self, cards: str, bid: int, normal=True):
        if normal:
            self.value_map = {'A': 14, 'K': 13, 'Q': 12, 'J': 11, 'T': 10}
        else:
            self.value_map = {'A': 14, 'K': 13, 'Q': 12, 'J': 1, 'T': 10}
        self.cards = cards
        self.card_values = self.get_card_values(cards)
        self.bid = bid
        if normal:
            self.type = self.get_type(self.cards)
        else:
            self.type = self.get_joker_type()

    def __repr__(self):
        return f"{self.cards}, {self.type}, {self.card_values}, {self.bid}\n"

    def __lt__(self, other):
        if self.type != other.type:
            return self.type < other.type
        else:
            for i in range(5):
                if self.card_values[i] != other.card_values[i]:
                    return self.card_values[i] < other.card_values[i]

    def get_card_values(self, cards):
        card_values = []

        for card in cards:
            if card in self.value_map:
                card_values.append(self.value_map[card])
            else:
                card_values.append(int(card))
        return card_values

    def get_type(self, cards):
        # split the cards into distinct sets based on their card type
        card_types = {}
        for card in cards:
            if card in card_types:
                card_types[card] += 1
            else:
                card_types[card] = 1
        # if there is only one card type, it is a five of a kind
        if len(card_types) == 1:
            return 7
        # if there are two card types, it is either a four of a kind or a full house
        if len(card_types) == 2:
            # if the card type with 4 cards has a count of 4, it is a four of a kind
            if 4 in card_types.values():
                return 6
            # otherwise it is a full house
            else:
                return 5
        # if there are three card types, it is either a three of a kind or a two pair
        if len(card_types) == 3:
            # if the card type with 3 cards has a count of 3, it is a three of a kind
            if 3 in card_types.values():
                return 4
            # otherwise it is a two pair
            else:
                return 3
        # if there are four card types, it is a one pair
        if len(card_types) == 4:
            return 2
        # if there are five card types, it is a high card
        if len(card_types) == 5:
            return 1

    def get_joker_type(self):
        if 'J' not in self.cards:
            return self.get_type(self.cards)
        else:
            cards = self.cards.replace('J', '')
            # if cards is now empty, all cards were J's, so this is a five of a kind
            if cards == '':
                return 7
            card_types = {}
            for card in cards:
                if card in card_types:
                    card_types[card] += 1
                else:
                    card_types[card] = 1
            most_frequent_card = max(card_types, key=card_types.get)
            replaced_cards = self.cards.replace('J', most_frequent_card)
            # print(f"Original Cards: {self.cards}, Replaced Cards: {replaced_cards}")
            return self.get_type(replaced_cards)


def read_hands(lines, normal=True):
    input = lines.split('\n')
    hands = []
    for line in input:
        cards = line.split(' ')
        hands.append(CamelHand(cards[0], int(cards[1]), normal))
    return hands


def solve(lines, normal):
    hands = read_hands(lines, normal)
    hands.sort()
    # print(hands)
    scores = []
    for i, hand in enumerate(hands):
        scores.append(hand.bid * (i + 1))
    return sum(scores)


if __name__ == '__main__':
    lines = read_input('input1.txt')
    answ1 = solve(lines, True)
    print(f"Answer 1: {answ1}")

    answ2 = solve(lines, False)
    print(f"Answer 2: {answ2}")
