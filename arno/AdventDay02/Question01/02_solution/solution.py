import json
import re

input_file = '../01_input/inputA.txt'
cube_data_path = '../01_input/inputB.txt'

# ------------------- #
# Cubes Available
# ------------------- #
with open(cube_data_path, 'r') as file:
    data = json.load(file)
cubes_available = data.get('cubesAvailable', {})

print("Cubes available:")
print(cubes_available)

# ------------------- #
# Game Data
# ------------------- #
# Define a dictionary to store game data
games_data = {}

# Read data from the file
with open(input_file, 'r') as file:
    # Read each line in the file
    for line in file:
        # Split the line into game number and colors data
        game_number, colors_data = line.strip().split(':')

        # Extract only the numeric part of the game number
        game_number = re.sub(r'[^0-9]', '', game_number)

        # Split the colors data into individual games
        individual_games = colors_data.split(';')

        # Create a dictionary to store color data for each game
        game_colors = {}

        # Process each individual game and store color data
        for game in individual_games:
            color_counts = game.strip().split(',')
            color_counts = [count.strip().split() for count in color_counts]
            
            # Create a dictionary to store count for each color
            color_dict = {color: int(count) for count, color in color_counts}
            
            # Use the game number as the key for the game_colors dictionary
            game_colors[str(individual_games.index(game) + 1)] = color_dict

        # Use the game number as the key for the games_data dictionary
        games_data[game_number.strip()] = game_colors

# Print the result to verify
print("\nGame data:")
for game_number, colors in games_data.items():
    print(f"'{game_number}': {colors}")

# ------------------- #
# Check if possible
# ------------------- #

possible_games = []
impossible_games = []

for game_number, colors in games_data.items():
    possible_games.append(game_number)
    for set in colors.values():
        for color, count in set.items():
            if color not in cubes_available:
                impossible_games.append(game_number)
                impossible_games.append(game_number)
                possible_games.remove(game_number)
                break
            elif cubes_available[color] < count:
                impossible_games.append(game_number)
                possible_games.remove(game_number)
                break
        else:
            continue
        break


print("\nPossibility Analysis:")
print(f"Possible games: {possible_games}")
print(f"Impossible games: {impossible_games}")

# ------------------------ #
# Sum of possible games
# ------------------------ #
sum_of_possible_games = 0
for game_number in possible_games:
    sum_of_possible_games += int(game_number)
print(f"\nSum of possible games:\n{sum_of_possible_games}")