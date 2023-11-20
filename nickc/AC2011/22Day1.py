sonardata = open('sonar-data', 'r')

previous = ""
increases, count = 0

with open('sonar-data') as data:
    for line in data:
        count = count + 1
        if (line > previous):
            increases = increases + 1
        previous = line
    
print("Number of Points - ", count)    
print("Number of Points - ", increases)