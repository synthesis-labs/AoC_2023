# Function to concatenate the first and last digit of a string
def concatenate_digits(input_string):
    first_digit = None
    last_digit = None

    for char in input_string:
        if char.isdigit():
            if first_digit is None:
                first_digit = char
            last_digit = char

    return first_digit + last_digit if first_digit is not None and last_digit is not None else None

# Input and output file paths
input_file_path = '../01_input/input.txt'
output_file_path = '../03_answer/calculations.txt'

# Process input file and store results in output file
total_sum = 0

# Read the input file and store the results in the output file
with open(input_file_path, 'r') as input_file, open(output_file_path, 'w') as output_file:
    for line in input_file:
        input_string = line.strip()
        result = concatenate_digits(input_string)

        if result is not None:
            output_file.write(f"{input_string}: {result}\n")
            total_sum += int(result)

    output_file.write(f"Total: {total_sum}\n")

print(f"Results have been stored in {output_file_path}. Total sum: {total_sum}")