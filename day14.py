from my_funcs import *
import re


def normalize_inpt(line):
    # maps from str ->  | ("mask", bitmask)
    #                   | ("memset", position, value)
    if line[:4] == 'mask':
        return "mask", re.compile(r"mask = ([01X]*)").match(line).group(1)
    return ("memset",) + re.compile(r"mem\[(\d+)\] = (\d*)").match(line).groups()


inpt = lmap(lambda x: normalize_inpt(x.strip()), open('in14.txt', 'r').readlines())
ex1 = [
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
    "mem[8] = 11",
    "mem[7] = 101",
    "mem[8] = 0"
]
ex2 = [
    "mask = 000000000000000000000000000000X1001X",
    "mem[42] = 100",
    "mask = 00000000000000000000000000000000X0XX",
    "mem[26] = 1"
]
ex1 = lmap(lambda x: normalize_inpt(x.strip()), ex1)
ex2 = lmap(lambda x: normalize_inpt(x.strip()), ex2)


def part1(data):
    def do_mask(mask, val):
        mask1 = ''.join(map(lambda x: '1' if x == 'X' else x, mask))  # and mask
        mask2 = ''.join(map(lambda x: '0' if x == 'X' else x, mask))  # or mask
        mask1 = int(mask1, 2)
        mask2 = int(mask2, 2)
        val = val & mask1
        val = val | mask2
        return val

    mem = {}
    mask = None
    for instr in data:
        if instr[0] == 'mask':
            mask = instr[1]
        else:
            pos = instr[1]
            val = int(instr[2])
            val = do_mask(mask, val)
            mem[pos] = val
    return sum(mem.values())


def part2(data):
    def bin_36(val):
        b = bin(val)[2::]
        if len(b) > 36:
            assert False
        return ("0" * (36 - len(b))) + b

    def all_values(addr):
        # List[char] -> List[int]
        if 'X' not in addr:
            return [int(''.join(addr), 2)]
        else:
            i = addr.index('X')
            addr[i] = '0'
            l = all_values(addr)
            addr[i] = '1'
            r = all_values(addr)
            addr[i] = 'X'
            return l + r

    def mem_addr_decode(mask, addr):
        # returns all addresses after doing memory address decoding
        addr = bin_36(addr)
        new_addr = []
        for bit, m in zip(addr, mask):
            if m == '0':
                new_addr.append(bit)
            else:
                new_addr.append(m)
        return all_values(new_addr)

    mem = {}
    mask = None
    for instr in data:
        if instr[0] == 'mask':
            mask = instr[1]
        else:
            addr = int(instr[1])
            val = int(instr[2])
            addrs = mem_addr_decode(mask, addr)
            for addr in addrs:
                mem[addr] = val
    return sum(mem.values())


print(part1(ex1))
print(part1(inpt))
print(part2(ex2))
print(part2(inpt))
