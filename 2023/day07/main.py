import sys
from collections import Counter
from functools import cmp_to_key

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

hands = [line.split() for line in lines]

# ----------------------------------------------------------------------
# * Part 1


def five(hand):
    count = Counter(hand)
    return len(count.values()) == 1


def four(hand):
    count = Counter(hand)
    return 4 in count.values() and 1 in count.values()


def full(hand):
    count = Counter(hand)
    return 3 in count.values() and 2 in count.values()


def three(hand):
    count = Counter(hand)
    return sorted(count.values()) == [1, 1, 3]


def two(hand):
    count = Counter(hand)
    return sorted(count.values()) == [1, 2, 2]


def one(hand):
    count = Counter(hand)
    return sorted(count.values()) == [1, 1, 1, 2]


def high(hand):
    count = Counter(hand)
    return count.values() == [1, 1, 1, 1, 1]


def hand_type(hand):
    if five(hand):
        return 7
    elif four(hand):
        return 6
    elif full(hand):
        return 5
    elif three(hand):
        return 4
    elif two(hand):
        return 3
    elif one(hand):
        return 2
    else:
        return 1


def card_value(card):
    match card:
        case 'A':
            return 14
        case 'K':
            return 13
        case 'Q':
            return 12
        case 'J':
            return 11
        case 'T':
            return 10
        case '9':
            return 9
        case '8':
            return 8
        case '7':
            return 7
        case '6':
            return 6
        case '5':
            return 5
        case '4':
            return 4
        case '3':
            return 3
        case '2':
            return 2


def cmp(h1, h2):
    t1 = hand_type(h1)
    t2 = hand_type(h2)
    if t1 > t2:
        return 1
    elif t1 < t2:
        return -1
    else:
        for i in range(5):
            v1 = card_value(h1[i])
            v2 = card_value(h2[i])
            if v1 > v2:
                return 1
            elif v1 < v2:
                return -1
            else:
                continue


sorted_hands = sorted(hands, key=cmp_to_key(lambda h1, h2: cmp(h1[0], h2[0])))
total = 0
for i in range(len(hands)):
    total += (i+1) * int(sorted_hands[i][1])

print("Part 1: " + str(total))

# ----------------------------------------------------------------------
# * Part 2


def five(hand):
    count = Counter(hand)
    if count['J']:
        count.pop('J')
    return len(count.values()) <= 1


def four(hand):
    count = Counter(hand)
    if count['J']:
        count.pop('J')
    if len(count.values()) != 2:
        return False
    if sorted(count.values())[0] > 1:
        return False
    if sorted(count.values())[1] > 4:
        return False
    return True


def full(hand):
    count = Counter(hand)
    if count['J']:
        count.pop('J')
    if len(count.values()) != 2:
        return False
    if sorted(count.values())[0] > 2:
        return False
    if sorted(count.values())[1] > 3:
        return False
    return True


def three(hand):
    count = Counter(hand)
    if count['J']:
        count.pop('J')
    if len(count.values()) != 3:
        return False
    if sorted(count.values())[0] > 1:
        return False
    if sorted(count.values())[1] > 1:
        return False
    if sorted(count.values())[2] > 3:
        return False
    return True


def two(hand):
    count = Counter(hand)
    if count['J']:
        count.pop('J')
    if len(count.values()) != 3:
        return False
    if sorted(count.values())[0] > 1:
        return False
    if sorted(count.values())[1] > 2:
        return False
    if sorted(count.values())[2] > 2:
        return False
    return True


def one(hand):
    count = Counter(hand)
    if count['J']:
        count.pop('J')
    if len(count.values()) != 4:
        return False
    if sorted(count.values())[0] > 1:
        return False
    if sorted(count.values())[1] > 1:
        return False
    if sorted(count.values())[2] > 1:
        return False
    if sorted(count.values())[3] > 2:
        return False
    return True


def card_value(card):
    match card:
        case 'A':
            return 14
        case 'K':
            return 13
        case 'Q':
            return 12
        case 'J':
            return -1
        case 'T':
            return 10
        case '9':
            return 9
        case '8':
            return 8
        case '7':
            return 7
        case '6':
            return 6
        case '5':
            return 5
        case '4':
            return 4
        case '3':
            return 3
        case '2':
            return 2


sorted_hands = sorted(hands, key=cmp_to_key(lambda h1, h2: cmp(h1[0], h2[0])))
total = 0
for i in range(len(hands)):
    total += (i+1) * int(sorted_hands[i][1])

print("Part 2: " + str(total))
