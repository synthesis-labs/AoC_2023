import re


# substitute words with their corresponding numbers
subs = ["on(?=e)", "tw(?=o)", "thre(?=e)", "four", "fiv(?=e)",\
        "six", "seven", "eigh(?=t)", "nin(?=e)"]
subdict = {"on": '1', "tw": '2', "thre": '3', "four": '4', "fiv": '5',\
            "six": '6', "seven": '7', "eigh": '8', "nin": '9'}

# read input and calculate total
with open("../01_input/input.txt") as f:
    lines = f.readlines()
total = 0
for line in lines:
    n = re.findall("[0-9]|" + "|".join(sub for sub in subs), line)
    total += int((n[0] if n[0].isdigit() else subdict[n[0]]) + (n[-1] if n[-1].isdigit() else subdict[n[-1]]))
print(total)