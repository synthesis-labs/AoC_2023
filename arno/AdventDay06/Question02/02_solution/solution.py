import functools

# Open the file in read mode and read the lines, stripping any leading or trailing whitespaces
with open("../../Question02/01_input/input.txt", "r") as f:
    lines = [line.strip() for line in f.readlines()]

# Extract time and distance lines, convert them to integers, and store in input as a list of tuples
time_line, dist_line = [line_nums.split() for line_nums in [line.split(":")[1].strip() for line in lines]]
time_line = int("".join(time_line))
dist_line = int("".join(dist_line))
input = [(time_line, dist_line)]

# Function to calculate the winning distance for each hold time
def btn_hold_to_distance(race_time, record_dist):
    win_dictionary = {}
    for hold_time in range(race_time + 1):
        hold_dist = (race_time - hold_time) * hold_time
        if hold_dist > record_dist:
            win_dictionary[hold_time] = hold_dist
    return win_dictionary

# Function to calculate the solution using functools.reduce
def solution(input):
    # List to store the number of winning distances for each race
    ways_to_win = []
    
    # Iterate over each race in the puzzle input
    for race in input:
        # Calculate the winning distances and count the number of ways
        ways_to_win.append(len(btn_hold_to_distance(race[0], race[1]).keys()))
    
    # Use functools.reduce to calculate the product of all values in ways_to_win
    solution = functools.reduce(lambda x, y: x * y, ways_to_win, 1)
    
    # Print and return the final solution
    print(solution)

# Call the solution function with input
solution(input)
