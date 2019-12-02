#!/usr/bin/env python3

import sys

template = lambda year, day: f'''
module Advent.Year{year}.Day{str(day).zfill(2)} where

import Advent.Types

solutionA :: Solution
solutionA = undefined

solutionB :: Solution
solutionB = undefined
'''.lstrip("\n")

if len(sys.argv) >= 4:
    year = sys.argv[1]
    for day in range(int(sys.argv[2]), int(sys.argv[3]) + 1):
        file = f'src/Advent/Year{year}/Day{str(day).zfill(2)}.hs'
        with open(file, 'w') as f:
            f.write(template(year, day))
