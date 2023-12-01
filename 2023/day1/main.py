import re
import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Part 1

total = 0
for line in lines:
    digits = [c for c in line if c.isdigit()]
    total += int(digits[0] + digits[-1])

print("Part 1: " + str(total))

# ----------------------------------------------------------------------
# * Part 2

num = re.compile(r'one|two|three|four|five|six|seven|eight|nine|\d')
num_rev = re.compile(r'eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|\d')
value = {
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5, 
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
    "0": 0,
    "1": 1,
    "2": 2,
    "3": 3,
    "4": 4,
    "5": 5, 
    "6": 6,
    "7": 7,
    "8": 8,
    "9": 9
}

total = 0
for line in lines:
    nums = []
    first = num.search(line)
    second = num_rev.search(line[::-1])
    if first:
        nums.append(first.group())
    if second:
        nums.append(second.group()[::-1])
    total += int(str(value[nums[0]]) + str(value[nums[-1]]))
        

print("Part 2: " + str(total))
