from my_funcs import *

ex = [
    "939",
    "7,13,x,x,59,x,31,19",
]
ex1 = [
    "foo",
    "17,x,13,19"
]
ex2 = [
    "foo",
    "67,7,59,61"
]
ex3 = [
    "foo",
    "67,x,7,59,61"
]
ex4 = [
    "foo",
    "67,7,x,59,61"
]
ex5 = [
    "foo",
    "1789,37,47,1889"
]
exs = [
    ex, ex1, ex2, ex3, ex4, ex5
]
data = [
    "1002460",
    "29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,601,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,19,x,x,x,x,x,x,x,x,x,x,x,463,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37",
]


def part1(data):
    time = int(data[0])
    buses = lmap(int, filter(lambda x: x.isnumeric(), data[1].split(',')))
    wait_time = lmap(lambda x: (x - (time % x)) % x, buses)
    return reduce(lambda a, b: a * b, min(zip(buses, wait_time), key=lambda x: x[1]))


def part2_v1(data):
    data = data[1].split(',')
    data = zip(data, range(len(data)))
    data = filter(lambda x: x[0].isnumeric(), data)
    data = map(lambda x: (int(x[0]), x[1]), data)
    data = {k: v for k, v in data}
    maxkey = max(data.keys())
    maxkey_t = 0
    while True:
        t = maxkey_t - data[maxkey]
        any_mismatch = False
        if t >= 0:
            # print(t)
            for k, v in data.items():
                offset = (k - (t % k)) % k
                if offset != v:
                    any_mismatch = True
                    break
            if not any_mismatch:
                return t
        maxkey_t += maxkey


def part2_v2(data):
    def get_inverse(mod, n):
        # only works when mod is prime
        if n / mod == 0:
            assert False
        # the inverse is n^(mod - 2)
        s = 1
        for _ in range(mod - 2):
            s *= n
            s = s % mod
        return s

    data = data[1].split(',')
    data = zip(data, range(len(data)))
    data = filter(lambda x: x[0].isnumeric(), data)
    data = lmap(lambda x: (int(x[0]), x[1]), data)
    data = {k: v for k, v in data}
    N = reduce(lambda a, b: a * b, data.keys())
    summation = 0
    for mod, i in data.items():
        N_i = N // mod
        remainder = ((mod - i) + mod) % mod
        inverse = get_inverse(mod, N_i)
        summation += remainder * N_i * inverse
        summation = summation % N
    return summation

def part2_v3(data):
    data = data[1].split(',')
    data = zip(data, range(len(data)))
    data = filter(lambda x: x[0].isnumeric(), data)
    data = map(lambda x: (int(x[0]), x[1]), data)
    data = {k: v for k, v in data}
    t = 0
    diff = 1
    for mod, offset in data.items():
        while True:
            if (t + offset) % mod == 0:
                # every solution must be of the form current_t + k * mod
                # this holds true for all the mods since they're prime
                diff *= mod
                break
            t += diff
    return t



print(part1(ex))
print(part1(data))
for ex in exs:
    print('should be equal')
    print(part2_v1(ex))
    print(part2_v2(ex))
    print(part2_v3(ex))
# part2_v1 doesn't work on data because it's too slow
print(part2_v2(data))
print(part2_v3(data))
