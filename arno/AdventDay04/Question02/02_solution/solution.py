def count_common_elements(list1, list2):
    # Count the number of common elements between two lists
    return len(set(list1) & set(list2))


def calculate_total_scratchcards(lines):
    # Calculate the total ways to copy scratch cards
    num_lines = len(lines)
    
    # Initialize a list to store the number of ways to copy each scratch card
    cards = [1] * num_lines
    
    for i, line in enumerate(lines):
        # Split the line into two lists of elements
        x, y = map(str.split, line.split('|'))
        
        # Count the number of common elements between the two lists
        common_elements_count = count_common_elements(x, y)
        
        # Update the number of copies for each subsequent scratch card
        for j in range(i + 1, min(i + 1 + common_elements_count, num_lines)):
            cards[j] += cards[i]
    
    # Return the total sum of ways to copy scratch cards
    return sum(cards)


# Read lines from the input file
file_path = '/Users/arnostrydom/Dev/AoC_2023/arno/AdventDay04/Question02/01_input/input.txt'
lines = open(file_path, 'r').readlines()

# Calculate and print the total scratch cards
print(calculate_total_scratchcards(lines))