class Lines:
    def __init__(self, line_index, line_numbers, line_special_chars):
        self.line_index = line_index
        self.line_numbers = line_numbers
        self.line_special_chars = line_special_chars
class NumberInfo:
    def __init__(self, number, start_index, end_index, adjacent_to_special_char):
        self.number = number
        self.start_index = start_index
        self.end_index = end_index
        self.adjacent_to_special_char = adjacent_to_special_char

class SpecialCharInfo:
    def __init__(self, char, index):
        self.char = char
        self.index = index

def parse_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()

    parsed_lines = []

    for line_idx, curr_line in enumerate(range(len(lines))):
        line_numbers = []
        line_special_chars = []
        curr_char_index = 0
        while curr_char_index < len(lines[curr_line]):
            if lines[curr_line][curr_char_index].isdigit():
                start_index = curr_char_index
                while curr_char_index < len(lines[curr_line]) and lines[curr_line][curr_char_index].isdigit():
                    curr_char_index += 1
                end_index = curr_char_index
                current_line_number = int(lines[curr_line][start_index:end_index])
                line_numbers.append(NumberInfo(current_line_number, start_index, end_index, '?'))
            elif lines[curr_line][curr_char_index] in '*+#$!@%&=-_+/()^~:;?<>':
                curr_special_char = lines[curr_line][curr_char_index]
                line_special_chars.append(SpecialCharInfo(curr_special_char, curr_char_index))
                curr_char_index += 1
            else:
                curr_char_index += 1
        parsed_lines.append(Lines(line_idx, line_numbers, line_special_chars))

    return parsed_lines

# Example usage
filename = '/Users/arnostrydom/Dev/AoC_2023/arno/AdventDay03/Question01/01_input/input.txt'
parsed_lines = parse_input(filename)

# Printout parsed lines
print('\n-------------------')
print('Parsed Lines')
print('-------------------')

non_adjacent_numbers = []
adjacent_numbers = []
for parsed_line in parsed_lines:
    print ('-------------------')
    print('Line', parsed_line.line_index)
    print ('-------------------')
    for idx, number_info in enumerate(parsed_line.line_numbers):
        # print number info
        print('Current Num', number_info.number)

        # test if adjacent to special char horizontally
        for char in parsed_line.line_special_chars:
            if char.index >= number_info.start_index - 1 and char.index <= number_info.end_index:
                number_info.adjacent_to_special_char = True
                adjacent_numbers.append(number_info.number)
  
        # test if adjacent to special char in above line
        if parsed_line.line_index > 0:
            for char in parsed_lines[parsed_line.line_index - 1].line_special_chars:
                if char.index >= number_info.start_index - 1 and char.index <= number_info.end_index:
                    number_info.adjacent_to_special_char = True
                    adjacent_numbers.append(number_info.number)

        # test if adjacent to special char in below line
        if parsed_line.line_index < len(parsed_lines) - 1:
            for char in parsed_lines[parsed_line.line_index + 1].line_special_chars:
                if char.index >= number_info.start_index - 1 and char.index <= number_info.end_index:
                    number_info.adjacent_to_special_char = True
                    adjacent_numbers.append(number_info.number)

        # if not adjacent
        if number_info.adjacent_to_special_char == '?':
            number_info.adjacent_to_special_char = False
            non_adjacent_numbers.append(number_info.number)

        print('Line Numbers:', [(info.number, info.start_index, info.end_index, info.adjacent_to_special_char) for info in parsed_line.line_numbers])
        print('Special Characters:', [(char_info.char, char_info.index) for char_info in parsed_line.line_special_chars])
        print('')

print('Adjacent Numbers:\n',adjacent_numbers)
print('Non Adjacent Numbers:\n',non_adjacent_numbers)

# Sum of adjacent numbers
print('Sum of adjacent numbers:', sum(adjacent_numbers))