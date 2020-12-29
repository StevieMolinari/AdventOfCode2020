library(magrittr)
library(dplyr)
library(readr)
library(stringr)

setwd("/Users/MiSelf2/Desktop/AdventOfCode/AdventOfCode2020")

## a function that takes in a 7 digit binary string and returns the integer for the row
binary.int.to.decimal <- function(x){
  xChar = as.character(x)
  nDigs = str_length(xChar)
  xDigs = str_split(xChar, pattern = "") %>% unlist() %>% as.numeric()
  sum(xDigs * 2^((nDigs - 1):0))
}

binary.int.to.decimal("101")

## a function that takes in a 3 digit binary string and returns the integer for the column
binary.to.col <- function(x){
  
}

# Instead of zones or groups, this airline uses binary space partitioning to seat people. A seat might be specified like FBFBBFFRLR, where F means "front", B means "back", L means "left", and R means "right".
# 
# The first 7 characters will either be F or B; these specify exactly one of the 128 rows on the plane (numbered 0 through 127). Each letter tells you which half of a region the given seat is in. Start with the whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the back (64 through 127). The next letter indicates which half of that region the seat is in, and so on until you're left with exactly one row.
# 
# For example, consider just the first seven characters of FBFBBFFRLR:
# 
# Start by considering the whole range, rows 0 through 127.
# F means to take the lower half, keeping rows 0 through 63.
# B means to take the upper half, keeping rows 32 through 63.
# F means to take the lower half, keeping rows 32 through 47.
# B means to take the upper half, keeping rows 40 through 47.
# B keeps rows 44 through 47.
# F keeps rows 44 through 45.
# The final F keeps the lower of the two, row 44.
# The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the plane (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. L means to keep the lower half, while R means to keep the upper half.
# 
# For example, consider just the last 3 characters of FBFBBFFRLR:
# 
# Start by considering the whole range, columns 0 through 7.
# R means to take the upper half, keeping columns 4 through 7.
# L means to take the lower half, keeping columns 4 through 5.
# The final R keeps the upper of the two, column 5.
# So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.
# 
# Every seat also has a unique seat ID: multiply the row by 8, then add the column. In this example, the seat has ID 44 * 8 + 5 = 357.
# 
# Here are some other boarding passes:
# 
# BFFFBBFRRR: row 70, column 7, seat ID 567.
# FFFBBBFRRR: row 14, column 7, seat ID 119.
# BBFFBBFRLL: row 102, column 4, seat ID 820.
# As a sanity check, look through your list of boarding passes. What is the highest seat ID on a boarding pass?


## pulls in raw data
boardPassRaw <- read_delim(file = "InputDay5.txt", delim = "\\n", 
                               col_names = FALSE, 
                           col_types = list(col_character())) %>% pull(1)

boardPassRaw %>% str_length() %>% unique()
boardPassMat <- boardPassRaw %>% 
  str_split_fixed(
    pattern = "", 
    n = 10
  ) 



boardPassDf <- tibble(
  RowString = boardPassRaw %>% str_sub(1, 7),
  ColString = boardPassRaw %>% str_sub(8, 10)
) %>% 
  mutate(
    RowBinString = RowString %>% 
      str_replace_all(pattern = "F", replacement = "0") %>% 
      str_replace_all(pattern = "B", replacement = "1"),
    ColBinString = ColString %>% 
      str_replace_all(pattern = "L", replacement = "0") %>% 
      str_replace_all(pattern = "R", replacement = "1"),
    RowNumber = sapply(RowBinString, binary.int.to.decimal),
    ColNumber = sapply(ColBinString, binary.int.to.decimal),
    SeatId = RowNumber*8 + ColNumber
  )

boardPassDf$SeatId %>% max()


#### Part 2 ####

maxSeatId <- boardPassDf$SeatId %>% max()
which(!((1:maxSeatId) %in% boardPassDf$SeatId))
## my seat = 743
sortedSeatIds = boardPassDf$SeatId %>% sort()
sortedSeatIds[c(sortedSeatIds %>% diff() != 1, FALSE)]
