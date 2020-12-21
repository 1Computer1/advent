# Advent of Code Template

This workspace is sorta convenient:  

- `generate.py <year>` generates files in `src/Advent/Year<year>` and the imports in `app/Main.hs` and adds to `exposed-modules` in `advent.cabal`.

- `Advent.Runner.TH` generates a function that runs the solution given the year, day, and subproblem (A or B).

- `Advent.Solution` has a `Solution` type that encapsulates a solution.

- To run a solution, simply do `cabal run advent -- <day><part>...`, for example, `cabal run advent -- 1a 1b`.

Feel free to use this as a template for yourself, maybe add tests and benchmarks or something.  
My own solutions will be on a separate branch.  
