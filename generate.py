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

if len(sys.argv) >= 2:
    year = sys.argv[1]
    for day in range(1, 26):
        file = f'src/Advent/Year{year}/Day{str(day).zfill(2)}.hs'
        with open(file, 'w') as f:
            f.write(template(year, day))
