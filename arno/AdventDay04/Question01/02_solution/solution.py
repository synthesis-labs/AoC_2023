# Open the input file and read its lines into a list
lines = open('/Users/arnostrydom/Dev/AoC_2023/arno/AdventDay04/Question01/01_input/input.txt', 'r').readlines()

def scratchCard():
    # Initialize a variable to store the total count
    total = 0
    
    # Iterate over each line in the list of lines from the input file
    for line in lines:
        # Split the line into two parts using the '|' character and map them to variables x and y
        x, y = map(str.split, line.split('|'))
        
        # Find the common elements between the sets x and y
        matches = set(x) & set(y)
        
        # Calculate the contribution to the total based on the number of common elements
        # If there are common elements, add 2^(number of common elements - 1) to the total
        total += 2 ** (len(matches) - 1) if matches else 0
    
    # Return the final total count
    return total

# Call the part1 function and print the result
print(scratchCard())