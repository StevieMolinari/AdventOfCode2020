library(magrittr)
library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(tictoc)
setwd("/Users/MiSelf2/Desktop/AdventOfCode/AdventOfCode2020")

any.na <- function(x){
  any(is.na(x))
}

all.not.na <- function(x){
  all(!is.na(x))
}

sort.unique <- function(x){
  sort(unique(x))
}

# The automatic passport scanners are slow because they're having trouble detecting which passports have all required fields. The expected fields are as follows:
# 
# byr (Birth Year)
# iyr (Issue Year)
# eyr (Expiration Year)
# hgt (Height)
# hcl (Hair Color)
# ecl (Eye Color)
# pid (Passport ID)
# cid (Country ID)
# 
# The first passport is valid - all eight fields are present. The second passport is invalid - it is missing hgt (the Height field).
# 
# Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?


## pulls in raw data and inlcudes "breaks" i.e. empty rows
npCredentialsRaw <- read_delim(file = "InputDay4.txt", delim = "\\n", 
                               col_names = FALSE, col_types = list(col_character()),
                               skip_empty_rows = FALSE) %>% pull(1)

## adds breaks at the beginning and end; 
### allows us to avoid a nasty if statement for first and last entries
breakInds <- c(0, which(is.na(npCredentialsRaw)), length(npCredentialsRaw)+1)
nNpCreds <- length(breakInds) - 1

npCredsDf <- tibble(
  byr = rep(NA_character_, nNpCreds),
  cid = rep(NA_character_, nNpCreds),
  ecl = rep(NA_character_, nNpCreds),
  eyr = rep(NA_character_, nNpCreds),
  hcl = rep(NA_character_, nNpCreds),
  hgt = rep(NA_character_, nNpCreds),
  iyr = rep(NA_character_, nNpCreds),
  pid = rep(NA_character_, nNpCreds)
)
npCredNames <- colnames(npCredsDf)

for(iNpCred in (1:nNpCreds)){
  npCredEntryMatrix <- npCredentialsRaw[(breakInds[iNpCred]+1):(breakInds[iNpCred+1]-1)] %>% 
    paste(collapse = " ") %>% 
    str_split(pattern = " ") %>% 
    unlist() %>% 
    sort() %>% 
    str_split_fixed(pattern = ":", n = 2) %>% t()
  npCredColInds <- which(npCredNames %in% npCredEntryMatrix[1,])
  npCredsDf[iNpCred, npCredColInds] <- npCredEntryMatrix[2,]
}


# #### Find and fix this bug ####
# ## this construction gives the wrong answer
# breakInds <- c(0, which(is.na(npCredentialsRaw), length(npCredentialsRaw)+1))
# nNpCreds <- length(breakInds) - 1
# 
# npCredsDf <- tibble(
#   byr = rep(NA_character_, nNpCreds),
#   cid = rep(NA_character_, nNpCreds),
#   ecl = rep(NA_character_, nNpCreds),
#   eyr = rep(NA_character_, nNpCreds),
#   hcl = rep(NA_character_, nNpCreds),
#   hgt = rep(NA_character_, nNpCreds),
#   iyr = rep(NA_character_, nNpCreds),
#   pid = rep(NA_character_, nNpCreds)
# )
# npCredNames <- colnames(npCredsDf)
# 
# for(iNpCred in (1:nNpCreds)){
#   npCredEntryMatrix <- npCredentialsRaw[(breakInds[iNpCred]+1):(breakInds[iNpCred+1]-1)] %>% 
#     paste(collapse = " ") %>% 
#     str_split(pattern = " ") %>% 
#     unlist() %>% 
#     sort() %>% 
#     str_split_fixed(pattern = ":", n = 2) %>% t()
#   npCredColInds <- which(npCredNames %in% npCredEntryMatrix[1,])
#   npCredsDf[iNpCred, npCredColInds] <- npCredEntryMatrix[2,]
# }
# 
# ## suspicion tells me it may be the first or last entry and so we check those with
# iNpCred = 1
# head(npCredentialsRaw)
# npCredentialsRaw[(breakInds[iNpCred]+1):(breakInds[iNpCred+1]-1)] %>% 
#   paste(collapse = " ") %>% 
#   str_split(pattern = " ") %>% 
#   unlist() %>% 
#   sort() %>% 
#   str_split_fixed(pattern = ":", n = 2) %>% t()
# 
# iNpCred = nNpCreds
# tail(npCredentialsRaw)
# npCredentialsRaw[(breakInds[iNpCred]+1):(breakInds[iNpCred+1]-1)] %>% 
#   paste(collapse = " ") %>% 
#   str_split(pattern = " ") %>% 
#   unlist() %>% 
#   sort() %>% 
#   str_split_fixed(pattern = ":", n = 2) %>% t()



npCredsDf %>% 
  add_column(
    IsValid = npCredsDf %>% 
      select(
        -cid
      ) %>% 
      apply(
        MARGIN = 1,
        FUN = all.not.na
      ) 
  )

npCredsDf %>% 
  select(
    -cid
  ) %>% 
  apply(
    MARGIN = 1,
    FUN = any.na
  ) %>% sum()

npCredsDf %>% 
  select(
    -cid
  ) %>% 
  apply(
    MARGIN = 1,
    FUN = all.not.na
  ) %>% sum()


#### Problem 2 ####
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

### building in the logic
# npCredsAllDf <- npCredsDf %>% 
#   mutate(
#     byrNum = as.numeric(byr),
#     iyrNum = as.numeric(iyr),
#     eyrNum = as.numeric(eyr),
#     htUnit = ifelse(str_sub(hgt, -2, -1) %in% c("cm", "in"),
#                 str_sub(hgt, -2, -1), NA_character_),
#     htAmount = as.numeric(str_sub(hgt, 1, -3)),
#     hairColorHexDec = ifelse(str_sub(hcl, 1, 1) == "#",
#                              str_sub(hcl, 2, -1), NA_character_),
#     pidNum = as.numeric(pid),
#     pidLen = str_length(pid),
#     BirthYear = ifelse(between(byrNum, 1920, 2002),
#                        byrNum, NA_integer_),
#     CountryId = cid,
#     EyeColor = ifelse(
#       ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
#       ecl, NA_character_
#     ),
#     ExpYear = ifelse(between(eyrNum, 2020, 2030),
#                      eyrNum, NA_integer_),
#     HairColor = ifelse(grepl("(0|1|2|3|4|5|6|7|8|9|a|b|c|d|e|f){6}", hairColorHexDec), 
#                        hcl, NA_character_),
#     HeightCm = ifelse(htUnit == "cm" & between(htAmount, 150, 193),
#                       htAmount, ifelse(
#                         htUnit == "in" & between(htAmount, 59, 76),
#                         htAmount * 2.54, NA_real_
#                       )),
#     IssueYear = ifelse(between(iyrNum, 2010, 2020),
#                        iyrNum, NA_integer_),
#     PassportId = ifelse(pidLen == 9 & !is.na(pidNum), 
#                         pid, NA_character_)
#   )
# 
# npCredsAllDf %>% lapply(sort.unique)


npCredsCleanDf <- npCredsDf %>% 
  mutate(
    byrNum = as.numeric(byr),
    iyrNum = as.numeric(iyr),
    eyrNum = as.numeric(eyr),
    htUnit = str_sub(hgt, -2, -1),
    htAmount = as.numeric(str_sub(hgt, 1, -3)),
    hairColorHexDec = ifelse(str_sub(hcl, 1, 1) == "#",
                             str_sub(hcl, 2, -1), NA_character_),
    pidNum = as.numeric(pid),
    pidLen = str_length(pid)
  ) %>% 
  transmute(
    BirthYear = ifelse(between(byrNum, 1920, 2002),
                       byrNum, NA_integer_),
    CountryId = cid,
    EyeColor = ifelse(
      ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
      ecl, NA_character_
    ),
    ExpYear = ifelse(between(eyrNum, 2020, 2030),
                     eyrNum, NA_integer_),
    HairColor = ifelse(grepl("(0|1|2|3|4|5|6|7|8|9|a|b|c|d|e|f){6}", hairColorHexDec), 
                       hcl, NA_character_),
    HeightCm = ifelse(htUnit == "cm" & between(htAmount, 150, 193),
                      htAmount, ifelse(
                        htUnit == "in" & between(htAmount, 59, 76),
                        htAmount * 2.54, NA_real_
                      )),
    IssueYear = ifelse(between(iyrNum, 2010, 2020),
                       iyrNum, NA_integer_),
    PassportId = ifelse(pidLen == 9 & !is.na(pidNum), 
                        pid, NA_character_)
  )


npCredsCleanDf %>% 
  select(
    -CountryId
  ) %>% 
  apply(
    MARGIN = 1,
    FUN = all.not.na
  ) %>% sum()




