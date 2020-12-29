library(magrittr)
library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(tictoc)
setwd("/Users/MiSelf2/Desktop/AdventOfCode/AdventOfCode2020")

list.intersect <- function(list){
  nSets = length(list)
  if(nSets == 1){
    return(list[[1]])
  }
  intersection = list[[1]]
  for(iSet in 2:nSets){
    intersection = base::intersect(intersection, list[[iSet]])
  }
  return(intersection)
}

## pulls in raw data and inlcudes "breaks" i.e. empty rows
customDeclarationRaw <- read_delim(file = "InputDay6.txt", delim = "\\n", 
                               col_names = FALSE, col_types = list(col_character()),
                               skip_empty_rows = FALSE) %>% pull(1)
npCredentialsRaw <- read_delim(file = "InputDay4.txt", delim = "\\n", 
                               col_names = FALSE, col_types = list(col_character()),
                               skip_empty_rows = FALSE) %>% pull(1)

## adds breaks at the beginning and end; 
### allows us to avoid a nasty if statement for first and last entries
breakInds <- c(0, which(is.na(customDeclarationRaw)), length(customDeclarationRaw)+1)
nCdGroups <- length(breakInds) - 1

custDecAffirmAnswers <- character(nCdGroups)
iCdGroup = 25
for(iCdGroup in (1:nCdGroups)){
  custDecAffirmAnswers[iCdGroup] <- customDeclarationRaw[(breakInds[iCdGroup]+1):
                                              (breakInds[iCdGroup+1]-1)] %>% 
    paste(collapse = "") %>% 
    str_split(pattern = "") %>% 
    unlist() %>% 
    unique() %>% 
    sort() %>% 
    paste(collapse = "")
}

custDecAffirmAnswers %>% 
  str_length() %>% 
  sum()


#### Part 2 ####
custDecAnswerList <- vector(mode = "list", length = nCdGroups)

for(iCdGroup in (1:nCdGroups)){
  custDecAnswerList[[iCdGroup]] <- customDeclarationRaw[(breakInds[iCdGroup]+1):
                                                           (breakInds[iCdGroup+1]-1)] %>% 
    str_split(pattern = "")
}

custDecAnswerList %>% 
  lapply(list.intersect) %>% 
  sapply(length) %>% 
  sum()




