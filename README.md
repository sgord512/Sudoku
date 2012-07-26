I can now read in Sudoku puzzles in certain formats, and solve them, or correctly report them as unsolvable if that is in fact the case. 

## Standard Sudoku (.su):
- A single sudoku puzzle per file.
- 9 lines, each with 9 characters seperated by spaces, either 1-9 or underscore.
- Separated by newlines, and there must be a terminating newline. (I want to fix this.)
```
5 3 _ _ 7 _ _ _ _
6 _ _ 1 9 5 _ _ _
_ 9 8 _ _ _ _ 6 _
8 _ _ _ 6 _ _ _ 3
4 _ _ 8 _ 3 _ _ 1
7 _ _ _ 2 _ _ _ 6
_ 6 _ _ _ _ 2 8 _
_ _ _ 4 1 9 _ _ 5
_ _ _ _ 8 _ _ 7 9
```

## Condensed Sudoku (.csu):
- A single sudoku puzzle per line, multiple puzzles per file.
- No spaces, every character is either period or 1-9. Arranged in reading order (top to bottom, left to right).
- Terminating newline.
- Designed for compactness and ease of parsing, not human readability.

    ..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9
    .......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...

# Everything works as it should!!!!!!! Yay!!!!!