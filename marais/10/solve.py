def read_input(file='./test.txt') -> list[str]:
    with open(file, 'r') as inputfile:
        lines = inputfile.readlines()
        lines = [line.strip() for line in lines]
    return lines


replace_pipes = {'7':'┐', 'L':'└', 'J':'┘', 'F':'┌', '.':'.', '|':'│', '-':'─', 'S':'S'}

n_pipes = ['|', '7', 'F', 'S']
s_pipes = ['|', 'L', 'J', 'S']
e_pipes = ['-', '7', 'J', 'S']
w_pipes = ['-', 'L', 'F', 'S']


def step(matrix, current, previous):
    # print matrix
    # print_matrix(current, matrix)

    current_pipe = matrix[current[0]][current[1]]

    # can I go north?
    if current_pipe in s_pipes and current[0] > 0:
        north = (current[0] - 1, current[1])
        north_pipe = matrix[north[0]][north[1]]
        if north_pipe in n_pipes and north != previous:
            return north

    # can I go south?
    if current_pipe in n_pipes and current[0] < len(matrix) - 1:
        south = (current[0] + 1, current[1])
        south_pipe = matrix[south[0]][south[1]]
        if south_pipe in s_pipes and south != previous:
            return south

    # can I go east?
    if current_pipe in w_pipes and current[1] < len(matrix[0]) - 1:
        east = (current[0], current[1] + 1)
        east_pipe = matrix[east[0]][east[1]]
        if east_pipe in e_pipes and east != previous:
            return east

    # can I go west?
    if current_pipe in e_pipes and current[1] > 0:
        west = (current[0], current[1] - 1)
        west_pipe = matrix[west[0]][west[1]]
        if west_pipe in w_pipes and west != previous:
            return west


def print_matrix(current, matrix):
    for r in range(len(matrix)):
        for c in range(len(matrix[r])):
            if r == current[0] and c == current[1]:
                print('*', end='')
            else:
                print(replace_pipes[matrix[r][c]], end='')
        print()
    print()


def solve(_matrix, _start) -> set:
    current = _start
    previous = _start
    next = step(_matrix, current, previous)
    polygon = set()
    polygon.add(current)
    polygon.add(next)
    while _matrix[next[0]][next[1]] != 'S':
        previous = current
        current = next

        next = step(_matrix, current, previous)
        polygon.add(next)
    return polygon


def find_start(parsed):
    for i, row in enumerate(parsed):
        for j, col in enumerate(row):
            if col == 'S':
                return i, j


def collapse_walls(walls):
    new_walls = []
    while len(walls) > 0:
        if len(walls) == 1:
            new_walls.append(walls[0])
            break
        if walls[0] == '|':
            new_walls.append('|')
            walls = walls[1:]
        elif walls[0] == 'F' and walls[1] == '7':
            walls = walls[2:]
        elif walls[0] == 'L' and walls[1] == 'J':
            walls = walls[2:]
        elif walls[0] == 'F' and walls[1] == 'J':
            walls = walls[2:]
            new_walls.append('|')
        elif walls[0] == 'L' and walls[1] == '7':
            walls = walls[2:]
            new_walls.append('|')
        else:
            print(f"We have an uncheck case!! {walls[0]}{walls[1]}")

    return new_walls


def is_inside_polygon(r, c, polygon, matrix: list[str]):
    if (r, c) in polygon:
        return False

    walls = matrix[r][c+1:]
    
    new_walls = walls.replace('-', '')
    new_walls = new_walls.replace('.', '')
    new_walls = new_walls.replace('FJ', '|')
    new_walls = new_walls.replace('L7', '|')
    new_walls = new_walls.replace('7', '')
    new_walls = new_walls.replace('L', '')
    new_walls = new_walls.replace('J', '')
    new_walls = new_walls.replace('F', '')

    # print(f"Point {r},{c}: walls={walls}")
    # print(f"Point {r},{c}: new_walls={new_walls}")

    if len(new_walls) % 2 == 0:
        return False
    else:
        return True


def solve2(polygon, matrix, max_r=None, max_c=None) -> list:
    # For each point in this range, check if it lies within the bounds of the polygon
    inside = []
    for r in range(max_r):
        for c in range(max_c):
            if is_inside_polygon(r, c, polygon, matrix):
                # print(f"Point {r},{c} is inside the polygon")
                inside.append((r, c))
    return inside


def print_inside(inside, polygon, matrix):
    for r in range(len(matrix)):
        for c in range(len(matrix[r])):
            if (r, c) in inside:
                print('o', end='')
            elif (r, c) in polygon:
                print(replace_pipes[matrix[r][c]], end='')
            else:
                print('.', end='')
        print()


def close_start(matrix: list[str], start):
    if matrix[start[0] - 1][start[1]] in n_pipes and matrix[start[0] + 1][start[1]] in s_pipes:
        matrix[start[0]] = matrix[start[0]].replace('S', '|')
    elif matrix[start[0]][start[1] - 1] in w_pipes and matrix[start[0]][start[1] + 1] in e_pipes:
        matrix[start[0]] = matrix[start[0]].replace('S', '-')
    elif matrix[start[0] - 1][start[1]] in n_pipes and matrix[start[0]][start[1] + 1] in e_pipes:
        matrix[start[0]] = matrix[start[0]].replace('S', 'L')
    elif matrix[start[0] - 1][start[1]] in n_pipes and matrix[start[0]][start[1] - 1] in w_pipes:
        matrix[start[0]] = matrix[start[0]].replace('S', 'J')
    elif matrix[start[0] + 1][start[1]] in s_pipes and matrix[start[0]][start[1] + 1] in e_pipes:
        matrix[start[0]] = matrix[start[0]].replace('S', 'F')
    elif matrix[start[0] + 1][start[1]] in s_pipes and matrix[start[0]][start[1] - 1] in w_pipes:
        matrix[start[0]] = matrix[start[0]].replace('S','7')
    return matrix


def clean_matrix(matrix, polygon):
    for r in range(len(matrix)):
        new_row = []
        for c in range(len(matrix[r])):
            if (r, c) not in polygon:
                new_row.append('.')
            else:
                new_row.append(matrix[r][c])
        matrix[r] = ''.join(new_row)
    return matrix


if __name__ == '__main__':

    matrix = read_input('input1.txt')
    start = find_start(matrix)
    # print(start)
    polygon = solve(matrix, start)
    ans1 = len(polygon) // 2
    print(f"Answer 1: {ans1}")

    matrix = close_start(matrix, start)
    matrix = clean_matrix(matrix, polygon)
    # print_matrix(start, matrix)
    inside = solve2(polygon, matrix, max_r=len(matrix), max_c=len(matrix[0]))
    print()
    print_inside(inside, polygon, matrix)
    print(f"Answer 2: {len(inside)}")

