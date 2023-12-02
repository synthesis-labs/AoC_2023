def read_input_file_and_generate_integer_list():
    with open('Day1_Input.txt',
              "r") as file:
        contents = file.read()
        lines = contents.splitlines()
        array_of_lines_without_alphas = []
        for line in lines:
            temp_list = []
            for char in line:
                if char.isnumeric():
                    temp_list.append(int(char))
            array_of_lines_without_alphas.append(temp_list)

    return array_of_lines_without_alphas


def sum_list(list_of_lists):
    total = 0
    for current_list in list_of_lists:
        string_concat = ""
        if len(current_list) == 1:
            string_concat += str(current_list[0]) + str(current_list[0])
            total += int(string_concat)
        else:
            string_concat += str(current_list[0]) + str(current_list[-1])
            total += int(string_concat)

    return total


print(sum_list(read_input_file_and_generate_integer_list()))
