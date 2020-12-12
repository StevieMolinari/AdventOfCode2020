library(magrittr)
library(dplyr)
library(tictoc)
setwd("/Users/MiSelf2/Desktop/AdventOfCode")

#### Problem 1 ####
## which of these two expenses sum to 2020; provide their product
expenseReportEntries <- read.delim("InputDay1.txt", header = FALSE) %>% pull(1) %>% as.numeric()
class(expenseReportEntries)

#### a stochastic approach; easy but possibly non-terminating ####
PairFound <- FALSE
while(!PairFound){
  pairCantidate <- sample(expenseReportEntries, size = 2)
  if(sum(pairCantidate) == 2020){
    PairFound <- TRUE
  }
}
pairCantidate
prod(pairCantidate)


#### a more methodical approach ####
nExpenses <- length(expenseReportEntries)
nPairs <- choose(nExpenses, 2)
allPairs <- matrix(0, nrow = nPairs, ncol = 2)

iPair = 1
for(i in 1:(nExpenses - 1)){
  for(j in (i + 1):nExpenses){
    allPairs[iPair, ] <- c(expenseReportEntries[i], expenseReportEntries[j])
    iPair = iPair + 1
  }
}
pairSums <- rowSums(allPairs)
winningPair <- which(pairSums == 2020)
allPairs[winningPair, ]



#### Problem 2 ####
## which of these three expenses sum to 2020; provide their product

#### a stochastic approach; easy but possibly non-terminating ####
PairFound <- FALSE
## change 'size' parameter from 2 to 3
tic()
while(!PairFound){
  pairCantidate <- sample(expenseReportEntries, size = 3)
  if(sum(pairCantidate) == 2020){
    PairFound <- TRUE
  }
}
toc()
pairCantidate
prod(pairCantidate)

### took an average of 7.4 seconds
# nTrials <- 10
# trialTime <- numeric(nTrials)
# 
# for(iTrial in 1:nTrials){
#   PairFound = FALSE
#   tic(paste("Trial", iTrial))
#   while(!PairFound){
#     pairCantidate <- sample(expenseReportEntries, size = 3)
#     if(sum(pairCantidate) == 2020){
#       PairFound <- TRUE
#     }
#   }
#   toc(log = TRUE, quiet = TRUE)
# }
# 
# trialTimes <- tic.log(format = FALSE) %>% 
#   sapply(function(listyList){
#     listyList$toc - listyList$tic
#   })
# mean(trialTimes)

#### a more methodical approach ####
### changed 2 to three in choose function and have three rows in matrix
nExpenses <- length(expenseReportEntries)
nPairs <- choose(nExpenses, 3)
allPairs <- matrix(0, nrow = nPairs, ncol = 3)

iPair = 1
for(i in 1:(nExpenses - 2)){
  for(j in (i + 1):(nExpenses - 1)){
    for(k in (j+1):nExpenses){
      allPairs[iPair, ] <- c(expenseReportEntries[i], 
                             expenseReportEntries[j], 
                             expenseReportEntries[k])
      iPair = iPair + 1
    }
  }
}

pairSums <- rowSums(allPairs)
winningPair <- which(pairSums == 2020)
allPairs[winningPair, ]
