import strutils

proc calc(path: string ): int =
  var
    group: seq[int] = @[]
    max: int = 0
  for line in readFile(path).splitLines():
    if line.strip() == "":
      var current = 0
      for v in group:
        current += v
      if current>max: max = current
      group = @[]
    else:
      group.add(line.parseInt())

  return max

let tr = calc("test_input.txt")
echo tr
assert tr == 24000

let rr = calc("input.txt")
echo rr