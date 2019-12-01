#!/usr/bin/env python3

import sys

template = lambda year, day: f'import qualified Advent.Year{year}.Day{str(day).zfill(2)}'

if len(sys.argv) >= 2:
    year = sys.argv[1]
    for day in range(1, 26):
        print(template(year, day))
