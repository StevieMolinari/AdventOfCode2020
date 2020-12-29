library(magrittr)
library(dplyr)
library(stringr)
library(tictoc)
setwd("/Users/MiSelf2/Desktop/AdventOfCode/AdventOfCode2020")

##
treeMapRaw <- read.delim("InputDay3.txt", header = FALSE) %>% pull(1)
treeMapMat <- treeMapRaw %>% str_split_fixed(pattern = "", n = str_length(treeMapRaw[1]))

##
treeMapDim <- dim(treeMapMat)
nRows <- treeMapDim[1]
nCols <- treeMapDim[2]


##
mod.remainder <- function(dividend, divisor, zeroRep = FALSE){
  remainder = dividend %% divisor
  ifelse(!zeroRep & remainder == 0){
    divisor
  } else {
    remainder
  }
}

## 
nHitTrees = 0
for(iRow in 1:nRows){
  iCol = mod.remainder(dividend = 3*iRow - 2, divisor = nCols)
  nHitTrees = nHitTrees + (treeMapMat[iRow, iCol] == "#")
}
nHitTrees

#### Part 2 ####
## TO DO 
## a function that takes in the 'slope' vector and returns 'falls' and 'runs'
# decimal.to.fraction <- function(){}

##
slopes <- c(1, 3, 5, 7, .5)
falls <- c(1, 1, 1, 1, 2)
runs <- c(1, 3, 5, 7, 1)
slopes == runs/falls
nSlopes <- length(slopes)

##
mod.remainder.vec <- function(dividendVec, divisorVec, zeroRep = FALSE){
  nDivisions = length(dividendVec)
  
  ##
  if(length(divisorVec) == 1){
    divisorVec = rep(divisorVec, nDivisions)
  } else if(length(dividendVec) != length(divisorVec)){
    stop("$\\emph{divisorVec}$ and $\\emph{dividendVec}$ need to be the same length.")
  }
  
  ##
  remainderVec = dividendVec %% divisorVec
  ifelse(rep(!zeroRep, nDivisions) & remainderVec == 0, divisorVec, remainderVec)
}

#### Initial Attempt ####
nHitTreesVec = rep(0, nSlopes)
for(iRow in 1:nRows){
  iCols = mod.remainder.vec(dividendVec = runs*(iRow - 1) + 1, divisorVec = nCols)
  rowIndicator = !(rep(iRow, nSlopes) %% falls > 0)
  nHitTreesVec = nHitTreesVec + (treeMapMat[iRow, iCols] == "#")*rowIndicator
}

prod(nHitTreesVec)


#### Trouble Shooting ####
iRow = 1
iCols = mod.remainder.vec(dividendVec = runs*(iRow - 1) + 1, divisorVec = nCols)
rowIndicator = !(rep(iRow, nSlopes) %% falls > 0)
nHitTreesVec = nHitTreesVec + (treeMapMat[iRow, iCols] == "#")*rowIndicator

nHitTreesVec = rep(0, nSlopes)
for(iRow in 1:nRows){
  iCols = mod.remainder.vec(dividendVec = runs*(iRow - 1) + 1, divisorVec = nCols)
  rowIndicator = !((rep(iRow-1, nSlopes) %% falls) > 0)
  nHitTreesVec = nHitTreesVec + (treeMapMat[iRow, iCols] == "#")*rowIndicator
}

## checking the 1/1 route
bigSquareTreeMapMat <- matrix(rep(treeMapMat, ceiling(nRows/nCols)), nrow = nRows)[1:nRows, 1:nRows]
sum(diag(bigSquareTreeMapMat) == "#")
sum(diag(bigSquareTreeMapMat) == "#") == nHitTreesVec[1]


## checking the 1/2 route
## 
(1:nRows)[(1:nRows %% 2) == 1]
(1:ceiling(nRows/2))*2 - 1

##
nHitTrees12 = 0
for(iRow in (1:nRows)[(1:nRows %% 2) == 1]){
  iCol = mod.remainder(dividend = (iRow + 1)/2, divisor = nCols)
  nHitTrees12 = nHitTrees12 + (treeMapMat[iRow, iCol] == "#")
}
nHitTrees12
nHitTrees12 == nHitTreesVec[nSlopes]


for(iRow in 1:5){
  iCols = mod.remainder.vec(dividendVec = runs*(iRow - 1) + 1, divisorVec = nCols)
  rowIndicator = !((rep(iRow-1, nSlopes) %% falls) > 0)
  print(cbind(iRow, iCols, rowIndicator, slopes, falls, runs))
}

for(iRow in 1:10){
  mod.remainder.vec(dividendVec = slopes*(iRow - 1) + 1, divisorVec = nCols) %>% print()
}

nHitTreesVec = rep(0, nSlopes)
for(iRow in 1:nRows){
  iCols = mod.remainder.vec(dividendVec = slopes*(iRow - 1) + 1, divisorVec = nCols) %>% round()
  rowIndicator = !((rep(iRow-1, nSlopes) %% falls) > 0)
  nHitTreesVec = nHitTreesVec + (treeMapMat[iRow, iCols] == "#")*rowIndicator
}

for(iRow in 1:64){
  mod.remainder.vec(dividendVec = slopes*(iRow - 1) + 1, divisorVec = nCols) %>% round() %>% print()
}
round(1/2)

for(iRow in 1:64){
  mod.remainder.vec(dividendVec = slopes*(iRow - 1) + 1, divisorVec = nCols) %>% ceiling() %>% print()
}

#### Solution ####
nHitTreesVec = rep(0, nSlopes)
for(iRow in 1:nRows){
  iCols = mod.remainder.vec(dividendVec = slopes*(iRow - 1) + 1, divisorVec = nCols) %>% ceiling()
  rowIndicator = !((rep(iRow-1, nSlopes) %% falls) > 0)
  nHitTreesVec = nHitTreesVec + (treeMapMat[iRow, iCols] == "#")*rowIndicator
}

prod(nHitTreesVec)

