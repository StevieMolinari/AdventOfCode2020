library(magrittr)
library(dplyr)
library(stringr)
library(tictoc)
setwd("/Users/MiSelf2/Desktop/AdventOfCode")


passwordPolicy <- read.delim("InputDay2.txt", header = FALSE) %>% pull(1) %>% as.character()
class(passwordPolicy)

#### Problem 1 ####
passwordsPolicyMatrix <- passwordPolicy %>% 
  str_split_fixed(pattern = "-| |: ", n = 4) 
colnames(passwordsPolicyMatrix) <- c("MinNum", "MaxNum", "Letter", "Password")

passwordPolicyDf <- passwordsPolicyMatrix %>% 
  as_tibble(
  ) %>% 
  transmute(
    Password,
    Letter,
    MinNum = as.numeric(MinNum),
    MaxNum = as.numeric(MaxNum),
    StringCount = str_count(Password, Letter),
    IsValidPassword = (MinNum <= StringCount & StringCount <= MaxNum)
  )

passwordPolicyDf$IsValidPassword %>% sum()


#### Problem 2 ####
passwordPolicyUpdateDf <- passwordsPolicyMatrix %>% 
  as_tibble(
  ) %>% 
  transmute(
    Password,
    Letter,
    FirstHotLetterInd = as.numeric(MinNum),
    SecondHotLetterInd = as.numeric(MaxNum),
    PasswordHotLetters = paste0(str_sub(Password, start = FirstHotLetterInd, end = FirstHotLetterInd),
                                str_sub(Password, start = SecondHotLetterInd, end = SecondHotLetterInd)),
    IsValidPassword = str_count(PasswordHotLetters, Letter) == 1
  )

passwordPolicyUpdateDf$IsValidPassword %>% sum()









