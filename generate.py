#!/usr/bin/env python3

import os
import pathlib
import re
import sys

template = lambda year, day: f'''
module Advent.Year{year}.Day{day.zfill(2)}
    ( solutionA
    , solutionB
    ) where

import Advent.Solution

solutionA :: Solution
solutionA = undefined

solutionB :: Solution
solutionB = undefined
'''.lstrip("\n")

if len(sys.argv) >= 2:
    year = sys.argv[1]

    with open('app/Main.hs', 'r') as f:
        content = f.read()
    with open('app/Main.hs', 'w') as f:
        imports = ''
        for i in range(1, 26):
            day = str(i)
            s = f'import qualified Advent.Year{year}.Day{day.zfill(2)}'
            if s not in content:
                imports += '\n' + s
        rep = re.sub(r'(module\s+Main.+where)', f'\\1\n{imports}', content, count=1, flags=re.DOTALL)
        f.write(rep)

    with open('advent.cabal', 'r') as f:
        content = f.read()
    with open('advent.cabal', 'w') as f:
        exposed = ''
        for i in range(1, 26):
            day = str(i)
            s = f'Advent.Year{year}.Day{day.zfill(2)}'
            if s not in content:
                exposed += '\n    ' + s
        rep = re.sub(r'(exposed-modules:.+Advent.Solution)', f'\\1{exposed}', content, count=1, flags=re.DOTALL)
        f.write(rep)

    pathlib.Path(f'src/Advent/Year{year}/').mkdir(parents=True, exist_ok=True)
    for i in range(1, 26):
        day = str(i)
        file = f'src/Advent/Year{year}/Day{day.zfill(2)}.hs'
        if not pathlib.Path(file).exists():
            with open(file, 'w') as f:
                f.write(template(year, day))
