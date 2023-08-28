
# function searches for a file name and extension and returns 
# a vector of full file names matching patters

find_file <- function(pattern = NULL,
                      start_dir = R.home(), # default starting directory
                      recursive = TRUE,
                      full.names = TRUE,
                      return_first = FALSE
                      ) {
  
  if (!is.null(pattern)) {
  
    
    l =       list.files(
      path      = start_dir,
      pattern   = pattern,
      recursive = recursive, 
      full.names = full.names)
    
    if (return_first) l = l[1] # return only first element
    return(l)
    
    } else {
    message("Error: missing pattern argument")
    }
}

#find_file("pets.csv", return_first = T)
