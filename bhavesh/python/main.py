def solve_day(day_module):
  try:
    return day_module.solve()
  except Exception as error:
    # print(error)
    return {"part1": "No impl found", "part2": "No impl found"}


def main():
  N = 25

  for i in range(1, N + 1):
    answer = {
      "sampleDataPart1": "",
      "sampleDataPart2": "",
      "part1": "",
      "part2": "",
    }
    question = str(i).zfill(2)
    try:
      day_module = __import__(f'days.day{question}', fromlist=['solve'])

      answer = solve_day(day_module)
    except ImportError:
      answer["part1"] = "No impl found"
      answer["part2"] = "No impl found"

    print(f"Day {question} - Sample Part 1: {answer['sampleDataPart1']}")
    print(f"Day {question} - Sample Part 2: {answer['sampleDataPart2']}")
    print(f"Day {question} - Part 1: {answer['part1']}")
    print(f"Day {question} - Part 2: {answer['part2']}")
    print()


if __name__ == "__main__":
  main()
