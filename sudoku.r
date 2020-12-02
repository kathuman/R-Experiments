rm(list=ls())
install.packages("sudoku")
library(sudoku)
library(help=sudoku)
help(generateSudoku)
example(generateSudoku)
generateSudoku()

myPuzzle <- generateSudoku(Nblank = 20, print.it = F)
printSudoku(myPuzzle)

printSudoku(fetchSudokuUK())

generateSudoku(20,T)

playSudoku()
playSudoku(fetchSudokuUK())
?playSudoku
