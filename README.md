# Program verifier

## installation 

1. install the Z3 SMT solver
1. Install the haskell z3 wrapper found on [link](http://hackage.haskell.org/package/z3)
1. Run `$ cabal install` in the root of the program directory
1. Run `$ happy Parser.y` in `./src/GCLParser/` directory
1. Run `$ alex Lexer.x` in `./src/GCLLexer/` directory
1. If everything succeeded, you should be able to run the program

## Running the program

##### benchmark runs
- Start ghci by with `$ ghci Main` in `./src/` directory 
- To run a full benchmark test run in ghci `$ benchmarkTest "memberOf" `. In this example "memberOf" 
could be replaced by any file name in `./examples/benchmark/`
- The function `$ benchmarkFile "memberOf" [10] [5] [50]` will run for N = [2..10] and 
heuristics [True,False]. thus run 18 times. In this example the loopdepth is 10, the guard depth is 5 
and the maximum path length is 50

##### Single run
1. To run a program a single time, you first have to load a program using `$ parseProgramFile {file path}`. File path 
is the path from `./src/` to your given file
1. Then run `$ testProgram {loaded program} ({n}, {loopDepth}, {guarddepth}, {heuristic}, {program path length})` 
1. Replace all the names between the curly brackets with values fitted for a specific run



