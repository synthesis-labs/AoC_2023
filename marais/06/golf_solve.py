import math

print(f"Answer 1: {math.prod([sum([1 if speed * (race['time'] - speed) > race['distance'] else 0 for speed in range(race['time'])]) for race in [{'time': 56, 'distance': 499}, {'time': 97, 'distance': 2210}, {'time': 77, 'distance': 1097},{'time': 93, 'distance': 1440}]])}")
print(f"Answer 2: {math.prod([sum([1 if speed * (race['time'] - speed) > race['distance'] else 0 for speed in range(race['time'])]) for race in [{'time': 56977793, 'distance': 499221010971440}]])}")
