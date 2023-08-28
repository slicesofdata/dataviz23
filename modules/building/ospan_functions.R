# ospan functions
#

# libraries and functions
library(dplyr)
library(caret)
library(magrittr)
library(dplyr)
library(stringr)

source("https://pastebin.com/raw/8mXH02yg")   # run and comment out before knitting
source("https://pastebin.com/raw/97NNTTzu")   # run to include in function definitions

###############################################################################
###############################################################################
###############################################################################

pavlovia_Merge <- function(
    dir_path = NULL,
    pattern_select = ".csv",
    pattern_filter = "participant|test"
) {
  # 
  if (!is.null(dir_path)) {
    # select names containing patterns
    fileList = list.files(path = dir_path, pattern = pattern_select, full.names = T)
    
    # remove names containing patterns
    fileList = fileList[grepl(pattern_filter, fileList) == FALSE]
    
    ## read each
    #dat = fileList %>%
    #  tryCatch(
    #    purrr::map_dfr(., readr::read_csv),
    #    error = function(e) NULL
    #    )
    #
    # iterate reading csv and apply to list
    dat = lapply(fileList, function(x) {
      tryCatch(
        read.table(x, 
                   header = TRUE, 
                   sep = ','), 
        error = function(e) NULL)
    })
    
    # bind list items to rows of data frame
    dat = dat %>% dplyr::bind_rows() 
    return(dat)
    
  } else { message("Warning: dir_path is NULL ")}
}

# important functions
clean_cell <- function(x, 
                       brackets = T, 
                       quotes.sing = T,
                       quotes.doub = T,
                       commas = F
) {
  # clean the messy cells
  if (brackets) x = gsub("[][]","", x)
  if (quotes.sing) x = gsub("'","", x)
  if (quotes.doub) x = gsub('"',"", x)
  if (commas) x = gsub(",","", x)
  return(x)
}

mean_of_string_split <- function(string, pattern = ",") {
  return(
    mean(as.numeric(unlist(
      stringr::str_split(string, pattern = pattern))))
  )
} 


# remove the comma
#a = "left,left,right"
#b = "left,right,left"
#a = c("left","left","right")
#b = c("left","right","left")
#(a == b)

mean_split_str <- function(string, pattern = ",") {
  return(
    mean(as.numeric(unlist(stringr::str_split(string, pattern))))
  )
}

sum_split_str <- function(string, pattern = ",") {
  return(
    sum(as.numeric(unlist(stringr::str_split(string, pattern))))
  )
}

matches_to_string <- function(stringList1, stringList2) {
  stringList1 = tolower(as.vector(unlist(
    stringr::str_split(stringList1, ","))))
  stringList2 = tolower(as.vector(unlist(
    stringr::str_split(stringList2, ","))))
  # convert the logical to string then to numeric
  m = paste(stringList1 == stringList2, collapse = ",")
  m = gsub("TRUE", "1", m)
  m = gsub("FALSE", "0", m)
  return(m)
} 

mean_matches <- function(stringList1, stringList2) {
  
  stringList1 = tolower(as.vector(unlist(
    stringr::str_split(stringList1, ","))))
  stringList2 = tolower(as.vector(unlist(
    stringr::str_split(stringList2, ","))))
  
  return(mean(stringList1 == stringList2))
} 

letterRec <- function(studyletters = "", recletters = "") {
  # compute the count for letters recalled independed of order
  counter = 0   # initialize counter
  
  if(recletters != "") { 
    
    # iterate through letters
    for (charPos in 1:nchar(studyletters)) { # for each studied 
      if(grepl(
        substr(studyletters, charPos, charPos), 
        x = recletters)) { # is the letter recalled
        counter = counter + 1                                             # increment counter
      } } } 
  return(counter)
}

# 
Recall_by_Position <- function(studyletters = "", recletters = "") {
  # compute the count for letters recalled in the correct position
  counter = 0   # initialize counter
  
  if(recletters != "") {
    for (charPos in 1:nchar(studyletters)) { # # iterate through letters, for each studied letter
      
      if(grepl(  # if the recalled letter is in the study position
        substr(studyletters, charPos, charPos), 
        substr(recletters, charPos, charPos))
      ) { 
        counter = counter + 1 # increment counter
      } }    }  
  return(counter)
} #lettersRecalledinOrder(studyletters = "", recletters = "")
