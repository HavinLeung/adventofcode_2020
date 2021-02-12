e1 = [
    16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4,
]

e2 = [
    28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3,
]

inpt = [
    99,
    104,
    120,
    108,
    67,
    136,
    80,
    44,
    129,
    113,
    158,
    157,
    89,
    60,
    138,
    63,
    35,
    57,
    61,
    153,
    116,
    54,
    7,
    22,
    133,
    130,
    5,
    72,
    2,
    28,
    131,
    123,
    55,
    145,
    151,
    42,
    98,
    34,
    140,
    146,
    100,
    79,
    117,
    154,
    9,
    83,
    132,
    45,
    43,
    107,
    91,
    163,
    86,
    115,
    39,
    76,
    36,
    82,
    162,
    6,
    27,
    101,
    150,
    30,
    110,
    139,
    109,
    1,
    64,
    56,
    161,
    92,
    62,
    69,
    144,
    21,
    147,
    12,
    114,
    18,
    137,
    75,
    164,
    33,
    152,
    23,
    68,
    51,
    8,
    95,
    90,
    48,
    29,
    26,
    165,
    81,
    13,
    126,
    14,
    143,
    15,
]


def num_diffs(l, n):
    m = 0
    for i, j in zip(l, l[1:]):
        if j - i == n:
            m += 1
    return m


def part1(l):
    l = sorted(l)
    return (num_diffs(l, 1) + 1) * (num_diffs(l, 3) + 1)


def part2(l):
    l = set(l + [max(l) + 3])
    from functools import lru_cache

    @lru_cache(maxsize=1000)
    def F(x):
        if x == 0:
            return 1
        if x < 0 or x not in l:
            return 0
        fx1 = F(x - 1)
        fx2 = F(x - 2)
        fx3 = F(x - 3)
        return fx1 + fx2 + fx3

    return F(max(l))


print(part1(e1))
print(part1(e2))
print(part1(inpt))
print(part2(e1))
print(part2(e2))
print(part2(inpt))
