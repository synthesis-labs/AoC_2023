
def read_input(file='./test.txt'):
    with open(file, 'r') as inputfile:
        lines = inputfile.read()
    return lines


def my_hash(string):
    acc = 0
    for c in string:
        acc += ord(c)
        acc *= 17
        acc = acc % 256
    return acc


class Lens:
    def __init__(self, label: str, fl: int):
        self.label = label
        self.fl = fl

    def __hash__(self):
        return my_hash(self.label)

    def __cmp__(self, other):
        return self.label == other.label

    def __str__(self):
        return f"{self.label}={self.fl}"

    def __repr__(self):
        return self.__str__()

    def __eq__(self, other):
        return self.label == other.label


class hash_map:
    def __init__(self, size=256):
        self.size = size
        self.map = [[] for i in range(size)]

    def add(self, l: Lens):
        box_idx = l.__hash__()
        if l in self.map[box_idx]:
            l_idx = self.map[box_idx].index(l)
            self.map[box_idx][l_idx] = l
        else:
            self.map[box_idx].append(l)

    def remove(self, l: Lens):
        box_idx = l.__hash__()
        if l in self.map[box_idx]:
            l_idx = self.map[box_idx].index(l)
            self.map[box_idx].pop(l_idx)

    def __str__(self):
        return str(self.map)

def solve2(strings):
    hm = hash_map()
    for s in strings:
        if '=' in s:
            label, fl = s.split('=')
            print(f"Label: {label}, fl: {fl}")
            l = Lens(label, int(fl))
            hm.add(l)
            # print(hm)
        else:
            label = s.split('-')[0]
            print(f"Label: {label}")
            hm.remove(Lens(label, 0))
            # print(hm)

    acc = 0
    for i, b in enumerate(hm.map):
        for j, l in enumerate(b):
            prod = 1
            prod *= (i+1)
            prod *= (j+1)
            prod *= l.fl
            acc += prod
            print(f"prod: {prod}")
    return acc

if __name__ == '__main__':

    input = read_input('input.txt')
    strings = input.split(',')
    ans = sum([my_hash(string) for string in strings])
    print(f"Answer 1: {ans}")

    ans2 = solve2(strings)
    print(f"Answer 2: {ans2}")
