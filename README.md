# Advent of Code 2025 

Planning to do this years' puzzles in Clojure!

## Running days

To run a days' code:

`clj -M main.clj`

This will run the code, expecting an input.txt file in the directory. I've not included these files in the repo (as per AoC's rules)
So to switch to using the example input, change the line:

`;; (def input (slurp "test.txt"))`

to remove the comment, and comment out the "input.txt" line.

Most of the puzzles I have it print out the answer for parts 1 and 2 by default, but you can also explore in the REPL.
