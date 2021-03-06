example = [
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#",
]

inpt = [
    ".#.#....###..#.#..............#",
    "#......#####..##.##.#.......#.#",
    ".###.....#..#.#..#..#.#......#.",
    ".........##.#.....#.#..........",
    "........##....#.......#.#..#..#",
    "#.#..####...#.....#.#.#...#....",
    "#....#...#.........#.....#..#.#",
    ".#..........#..#.............#.",
    "...##..##..#...####.#.#.#.#....",
    ".#...####............##....#...",
    "..##.....#.#......#......#.#.#.",
    "..##......#..##.....#.#.....#.#",
    "..#...#....#.#.........##......",
    "#..##..##..#..##....#....##.#.#",
    "..###.#....#.#.#...#......#.#.#",
    "....#...#...#.........#.....##.",
    ".#..#.#..........#.##.....#.#..",
    ".#...#...###..#..#..####.#...#.",
    "##..............#..#.#...###..#",
    ".#..#.#.#...#..#...#..#........",
    "..#.#......#.#..##...#.#..#....",
    "...#.#.....#.##..#...#..#......",
    "...#...##....##..#....#..#...#.",
    "#......##.#.......#...#..#.#...",
    ".#..#......####...#............",
    "...#..##.#...#....#.#.#.#......",
    "....##...........##.#.#...##...",
    "#.##.###........#..###.#..##...",
    "....#......#....##...##.#......",
    "#.............#...#.........#..",
    "..##.......#.......#.#...#...#.",
    "...#....####...#...#....#.###..",
    "...##......#...###.#...#....#..",
    "...#.............#...#.......#.",
    "...#..#.##.##.#..#.##.#..#....#",
    "..####.....#..#............#...",
    "##....##..#.#.#.#..#..#.....#..",
    "......##...##......#.#.........",
    "#.#............#.#.#..#......#.",
    "...#.#.#.....#..#..#.#..##.....",
    ".#.#.............###..#....##..",
    "....#.###..#..#.....#..#.##....",
    "..#.#....#.......#.......##..#.",
    ".#.##.#.#..#..##.........##....",
    "...#...###.##....#####.......#.",
    "......#.....##...##...#....#.#.",
    "###.......##..#.....#......#.#.",
    "...#..#..#....#.#.....##..#...#",
    "..#....##.......#....#.........",
    "#....##.........##......#.#..#.",
    "#.....#.#.#..##..#.#.....##....",
    "......#....#...#...#.###....##.",
    "#...####...###.##..#.#.#..##...",
    "......###....###..##......#..##",
    ".#.####.###..#.....#...#..#...#",
    ".###.#.....#..#.#..#.....##..##",
    "...##...#.####....#......###...",
    "...##.......#.#..#......#.#....",
    "......##....#......#.........#.",
    "............#....#............#",
    "..#.#..........#......#..#.....",
    ".#...#.#.#......#..##..#....##.",
    "..##.#.#.#..#...###..#.#.##.#..",
    "..#......#.........#.......#...",
    "...#...##.#.##......#.....#....",
    "..#.....#..##....#..##..#.#.##.",
    "....##....#.#...#..##.##.##....",
    "..#.............#...#......#...",
    ".#.#.#.##..#.#..##...#.........",
    ".##...........#..#.#........#..",
    ".#..##.....#....#...#...#......",
    "#.#.....##.#..#...######....#..",
    "....#..#...##...#.........###..",
    "..##.#...##..#......#.##..#...#",
    "##..##...........#.......#.#...",
    ".......##..##...###.##.......#.",
    ".#.##...#.##...............#...",
    ".......#.............#.......##",
    "......#...........#...#..##....",
    ".#..#..#....#..........#......#",
    "...........#..#.....#....##....",
    "###....#....##..#..##.....##...",
    "#........#........#...#.##.##..",
    "##.#.#........#..#.#..#.......#",
    ".##.#.....#............#.......",
    ".....#........#..##......##....",
    ".#.####.#.##..................#",
    "#...##.......#...#....#.#.##...",
    "#.#.##...#.#......#.....#....#.",
    ".........#....#...#....#.....#.",
    "...#..#..#.#..#.##........##.#.",
    "..#.##.#...#...#....#....##..#.",
    ".#..#...####..........#.......#",
    "....#...#...#...##.#.##......#.",
    ".#....#...#.#..##..##.#.....#..",
    ".....#....#......#.#####...###.",
    "..#...##..#......#.#....#.....#",
    ".##....##..###.#.....##.##.##..",
    "#...#.#.........#....#....#....",
    "...#.........#.##....##.#.#.#..",
    "...#...#.#....#..#.#.......#.#.",
    "#......#..#....##....#.........",
    "...........#......#......###..#",
    "#..#...#..##..#....#.....#.#.#.",
    "#.#.....##..#..........###..#..",
    "#...#.....#.......#..##...#....",
    "...#....##.....#..##..#....#...",
    "#...#.#......#..#...#........#.",
    ".#....#...#...#.........##....#",
    "..##...#.........#.......##..#.",
    "......#.......#.....##....#..#.",
    ".....##..#...#............#.#.#",
    "...#....#.##..#...#.#....#.....",
    "...#...........#.##....#..##.##",
    "##......##....##...........#.#.",
    "..##..##......#...#.##.##..#...",
    ".#..##.#...##...#......###.##.#",
    "###.#....##..#..#.##..##...##..",
    "..#........##.#...#.......#....",
    ".....##....##.#.###.....#....#.",
    "#.##....#....##.....#..#.#.....",
    "#.........#..##...##.......##..",
    ".#....#......#.#...##..........",
    "##..##.....##....###..#...#.#.#",
    "..##.#.#..#......#.#....###..#.",
    ".#.##.....##.......##.#.##..#..",
    "..##...#........#.#.#.##.#..###",
    "........#.......#...##....#.#..",
    "...#..#...#.##..#...#.#.###.##.",
    "..#.#....#..#...#..##.........#",
    "#....#..##..##....#.........#..",
    ".......#.......#....#....#.#...",
    "...#.##...#...#..#....#.###.##.",
    "##.##...#..........#....#......",
    "#.##.#.....#..#............##.#",
    ".##...#.#.#.##...........#..##.",
    ".#...#....#.......##...##...#.#",
    ".#......#..#...#...#....#.#....",
    "...#..#..#...#..##..##.....#..#",
    ".#.##..#.#...#..#.#...#...#...#",
    "#.##..........#.##..#....#.....",
    "##....#.#..........#..#....#...",
    "..#..##.#.......#...#.##......#",
    "....##......#......#.#.#.##....",
    "###......#...##..#..........###",
    "##.#.##.....###.#..#.#......#..",
    "#.#.#........#..........#....#.",
    "...#.#..#.......#......##.#....",
    "......#.....#.#.#....###..#...#",
    ".........#...#..#####..#.#..#..",
    "..........#.#.#####.#..#.....#.",
    "....#.......#.#....#.....##..#.",
    "#...##.#..#.#........#.#..#..##",
    "#......#..#.#.....##......#.##.",
    ".##...#....#.##..#.....###..#..",
    "#....#.#..##....##..#.#####....",
    ".......##..........#......#....",
    "......#.#...#............#.....",
    ".......###.....##.#..#.#....#.#",
    "...#...#..........#....##...##.",
    ".#..#.#.#....#.#.....##..#.#..#",
    "......#.#..#....#..#...#.......",
    "##.#####............#.#.####.#.",
    "#.....###.#.......##...###....#",
    "......#.##..##.........#.#.....",
    ".#.#......#..#.##......#......#",
    ".#.#.#..#.#...##.....#..#.#..#.",
    ".#.#....#......................",
    "#.#..###...#...####.##.#....#.#",
    ".....#............#....#..#.##.",
    "#..#...#.#....#....#..#..#...#.",
    "...#.......#..#.#....#.......#.",
    ".#..#.#...#.#.####..#...#....##",
    "....#..#..............####....#",
    ".....#.#.###....#.#.#.#...#....",
    "..####..#.#.##.##.##....#..#...",
    ".#.#.#.###..#.##..............#",
    "..#.#..#...#.....#.......#.##..",
    ".#.#..#.....##...###.....#..#..",
    "..#..#......#.##..#......##..#.",
    ".....#.#.#..##..###.#..........",
    ".##......#...#.##.......#..#..#",
    ".......#...#.....###.##...##...",
    "..##..#.#.......#..............",
    "#.....#......#.#..#..#..#......",
    "..###.......##...#.##....#.....",
    ".....##...........##.....#...##",
    ".#.#.####....###.#.......#...##",
    "#.#..##.#.#.....#.#....#.......",
    ".........#.#..#...............#",
    "..##.#..#..#####.###.........#.",
    ".#........#...#...#...#.##.#..#",
    ".#.##..........#..##....#.#.#..",
    ".##......#....#.#....##.#.#.#..",
    ".......##.####..#..#.#..#.#...#",
    "...#.....#..##..###.#..##...#..",
    "#.......##..#####....#.......#.",
    "#.#.##.................#...###.",
    "................####...........",
    ".#..#......#...###.............",
    "......#.#.##.##.....#..........",
    ".......#..#.#............##....",
    "#........#..#....#......#.####.",
    "...#.#....##..#..#.............",
    "..#.#......#...#.#..#..........",
    "###...###...........#......#...",
    "#.###..###........###...#..###.",
    ".#.....#...#.#...........##....",
    "....#..##.....#..#......#......",
    "#.###.#........#.#.##..........",
    "#.#.#.#.#..#.#...#...##.#......",
    "..###.......###..#.#.#.#.#.....",
    "...#........#.......#.###..##..",
    ".#........#...#.#........#..##.",
    "#.......###..#....##.###...#..#",
    ".##....###..##...........##...#",
    "#...#..........#.....#..##..#..",
    "#..##..#..##.#.........##......",
    "..#.#..###..###.....#.......#..",
    "#...#...........##.#.#.###.....",
    "...#....#.....#.....#.##.#.##.#",
    "...........##.......####...#..#",
    "#.#...#..##..#.#..#..........#.",
    "..#...#.##........#.#..........",
    ".##.....#.#.#....#.#.......#.#.",
    ".......#.##...#.##....#.#...#..",
    "......#...##...###...#.....###.",
    "##......#.##.####.##...##......",
    "..#....#.#..###.#..##....#..#..",
    "...##..###.....###.....#.......",
    "...#.....#.#........#..#..##.#.",
    ".....................#.....#.#.",
    ".#...#...##.#..#..........#...#",
    "#.....#..#....#..#.......####..",
    ".##.......##......###.#..#...##",
    ".#.##..#...#..........##.......",
    "...##...........##..##......#..",
    "#....##.##...#......##.#.##.##.",
    "..##.##.#.#.#....#........#.#..",
    "....#......#......##..##.#.#...",
    ".............#.##...#..#...#...",
    ".#..#...#.........#...........#",
    "....#.....#..................#.",
    "........##............#...#..##",
    ".###.....##...#...#.##.....##..",
    "...##.#.........##.#.#..#......",
    "#...........##.#..#........#..#",
    "....#....#..##.#..##..#..#..#.#",
    "#..##..#............#...#.#.#..",
    "#......#..##......#...##..#...#",
    "....#.#..##.#.#...####...#.....",
    ".##..#..##....#...#....#...##.#",
    "##.....#.#........#....#.#.#...",
    "......#.#...##....#.###.....#.#",
    "..#..#............###.###.##.#.",
    "#..#.##.##.##..#...#.#.##..#...",
    "....#..#.#...#......#..###.....",
    ".#........#...###.....#...#....",
    "....##.##....#..#...#.#####.#.#",
    "...#..#...#.#.....#....#...###.",
    "..........####...##............",
    ".....#....##...##......#..#...#",
    "..#...#.####......#...#..#..###",
    ".#.....#....#..#...###..#.#....",
    "..#..#......#.#...#.....##....#",
    ".....##....#....#...#.....##...",
    "#............##.#....#.#.#..#..",
    "#......#......#....#.#..##.#...",
    "#.#......##.....#.#..##.#.#....",
    ".#.###..#.#......##...........#",
    "#.#.........##..#.#.##......#..",
    "##....####...##...........#....",
    "....###.#..##.#.#.##...##.....#",
    "..###.......##.......#......#..",
    "..#.###.##.#...................",
    "...#.#...#..#..#..##.###...#.#.",
    "#...#..#...#..#....#..#...#....",
    "....#........#.#.#.##.##.#..#.#",
    "...#....#.#...#..#....#.#.#....",
    "..#...#..##.#....##...###...##.",
    "#......#.....#.....#....####.#.",
    "...##.#..#.#.....#..#..##.#....",
    ".####.#..#...#.#......#......#.",
    "..#.#....#..#..##.#......##....",
    "....#...#.#..#...#...##........",
    "##..#.#....#..#.........#..##..",
    "...#.......#....#..##...###..##",
    "#......##.#..#..#..#..###.#.###",
    ".#..##.#...##...#.............#",
    "###.........#...###.#.#..#....#",
    ".#.....#..#........#.#.......#.",
    "#..#.#.....#.........###..#....",
    "#..##.....#.#....#.###.....#..#",
    "....#..#.......##..#.#..##....#",
    "##.##..#....#..#.#.###.........",
    "..##....#........#..#..#.##.#.#",
    ".#....#...##..#.#.....#..##..#.",
    "#..#.......#......#...#...#.##.",
    "...##.#......#.#..#......#.....",
    "......#...#.##.#....#...#.##.#.",
    "#.....#..#.#.#...##...#........",
    "....#.#..#.#.....#....#.#..#...",
    "....#.#...###............#.....",
    ".#.#...##.......#....#.##...#.#",
    ".....#.##......#.#..#...#....#.",
    ".###....#...#........#.........",
    "..#.....#..#.#.#..##...#..#....",
    "...###..#....#.....#.........##",
    "#....#....###...#.#............",
    ".#..##.....#...........#.#..#..",
    "..#.#.#.......##..#.#..........",
    ".#...#...####.#...#####.....#.#",
    "..#....##.....#..#...#.........",
    "#.#......#.##.........#......##",
    "..#.#...#.##..#....#....#.##...",
    "#....#......##.#..#......#.#.#.",
    "#.#.............##..#.#........",
    "..#.###.......##.....##.#..##.#",
    ".........#........#...#..#....#",
    ".........##.#.#..#..#....#....#",
    "##..#..#.#.....##.........#.#.#",
    "..##.##..#.##..........##.#..#.",
    "...#..#####.......#.........#..",
]


def part1(trees, xincr=3, yincr=1):
    x = 0
    n = 0
    for row in trees[::yincr]:
        if row[x] == "#":
            n += 1
        x = (x + xincr) % len(trees[0])
    return n


def part2(trees):
    directions = [
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2)
    ]
    encounters = map(lambda x: part1(trees, *x), directions)
    n = 1
    for encounter in encounters:
        n *= encounter
    return n


print(part1(example))
print(part1(inpt))
print(part2(example))
print(part2(inpt))
