import json
import re

input_file = '../01_input/inputA.txt'

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

# -------------------------------- #
# Minimum cubes required per game
# -------------------------------- #

# Create a dictionary to store the minimum cubes required for each game
min_cubes_required_per_game = {}

# Iterate through each game
for game_number, colors in games_data.items():
    # Create a dictionary to store the minimum cubes required for each color in the current game
    min_cubes_required_for_game = {}

    # Iterate through each individual game in the current game
    for individual_game, color_counts in colors.items():
        # Iterate through each color count in the individual game
        for color, count in color_counts.items():
            # Update the minimum cubes required for the current color in the current game
            min_cubes_required_for_game[color] = max(min_cubes_required_for_game.get(color, 0), count)

    # Store the minimum cubes required for each color in the current game
    min_cubes_required_per_game[game_number] = min_cubes_required_for_game

# Print the minimum cubes required for each game
print("\nMinimum cubes required for each game:")
for game_number, min_cubes_required_for_game in min_cubes_required_per_game.items():
    print(f"Game {game_number}: {min_cubes_required_for_game}")

# -------------------------------- #
# Power of minimum cubes required
# -------------------------------- #
print("\nPower of minimum cubes required for each game:")
summed_powers = 0
for game_number, min_cubes_required_for_game in min_cubes_required_per_game.items():
    power = 1
    calculation = '1'
    for min in min_cubes_required_for_game.values():
        power = power*min
        calculation += " * " + str(min)
    print(f"Game {game_number}: {power} ({calculation})")
    summed_powers += power

print(f"\nSummed powers: \n{summed_powers}")