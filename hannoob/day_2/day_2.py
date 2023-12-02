from toolz import thread_last

def read_lines_from_file(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    return lines

def get_color_info(color_string): # " 2 red"
    number, color = color_string.strip().split(' ') 
    return color, int(number)
    
def get_draw_info(draw_string): 
    return thread_last(
        draw_string.split(','), # "13 green, 2 red, 2 blue"
        (map, get_color_info),
        dict
    )

def get_game_info(line): 
    game_number_string, draw_info_string = line.split(':')
    
    game_number = int(game_number_string.strip('Game '))
    draws = map(get_draw_info, draw_info_string.split(';')) #2 blue, 5 green; 2 blue, 2 green

    return {"game": game_number, "draws": draws}
            

def is_possible(draws, total_red, total_green, total_blue):  
    def is_more_than_total(draw):
        return draw.get("red",0) > total_red or draw.get("green",0) > total_green or draw.get("blue",0) > total_blue

    return any(is_more_than_total(draw) for draw in draws)


def minimum_cubes_needed(game):
    minimums = {"red": 0, "green": 0, "blue": 0}

    for draw in game["draws"]:
        minimums["red"] = max(minimums["red"], draw.get("red", 0))
        minimums["green"] = max(minimums["green"], draw.get("green", 0))
        minimums["blue"] = max(minimums["blue"], draw.get("blue", 0))

    return minimums

def calculate_problem1(lines, red, green, blue):
 
    def is_possible_game(game): return is_possible(game["draws"], red, green, blue)

    def get_game_id(game): return game["game"]

    return thread_last(
        lines,
        (map, get_game_info),
        (filter, is_possible_game),
        (map, get_game_id),
        sum
    )

def calculate_problem2(lines):
    def power(minimums): return minimums.get("red")*minimums.get("green")*minimums.get("blue")

    return thread_last(
        lines,
        (map, get_game_info),
        (map, minimum_cubes_needed),
        (map, power),
        sum
    )

lines = read_lines_from_file('input.txt')

print(f"problem 1: {calculate_problem1(lines, 12, 13, 14)}")
print(f"problem 2: {calculate_problem2(lines)}")