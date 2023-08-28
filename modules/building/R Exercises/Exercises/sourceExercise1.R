# Get files from DropBox
# Working dir should be desktop.

getnewdata <- function(){
#  if(.Platform$OS.type == "unix") {
#    mainDir <- paste("/users/", username, "/Desktop", sep = "")
#    setwd(paste("/users/", username, "/Desktop", sep = ""))
#    #subDir = "outputDirectory"
#  } 
#  if(.Platform$OS.type == "windows") {
#    mainDir <- paste("c:/users/", username, "/Desktop", sep = "")
#    setwd(paste("c:/users/", username, "/Desktop", sep = ""))
#    #subDir = "outputDirectory"
#    #print(mainDir)
#  }

  #getwd() # verify
  
  # Make Psyc109 if it is not under the working dir, make it so.
  psyc109Dir <- paste(getwd(), "Psyc109", sep = "/")
  #print(psyc109Dir)
  ifelse(!dir.exists(file.path(psyc109Dir)), dir.create(file.path(psyc109Dir)), FALSE)
  #  ifelse(!dir.exists(file.path(mainDir)), dir.create(file.path(mainDir)), FALSE)

#mainDir <- getnewdata("gcook")

# Make the subdirs if they are not there.
subDir <- "Scripts"
scriptDir <- paste(psyc109Dir, subDir, sep = "/")
ifelse(!dir.exists(file.path(psyc109Dir, subDir)), dir.create(file.path(psyc109Dir, subDir)), FALSE)

subDir <- "Data"
dataDir <- paste(psyc109Dir, subDir, sep = "/")
ifelse(!dir.exists(file.path(psyc109Dir, subDir)), dir.create(file.path(psyc109Dir, subDir)), FALSE)

subDir <- "Backups"
backupDir <- paste(psyc109Dir, subDir, sep = "/")
ifelse(!dir.exists(file.path(psyc109Dir, subDir)), dir.create(file.path(psyc109Dir, subDir)), FALSE)
ifelse(!dir.exists(file.path(psyc109Dir, subDir, "/Data")), dir.create(file.path(psyc109Dir, subDir, "/Data")), FALSE)
ifelse(!dir.exists(file.path(psyc109Dir, subDir, "/Scripts")), dir.create(file.path(psyc109Dir, subDir, "/Scripts")), FALSE)
# showWarnings = T, recursive = FALSE, mode = "0777")

# Save the data file(s)
# try capabilities see if it is supported on your build.
capabilities("libcurl") # This should return True in order to work

if (capabilities("libcurl") == FALSE) {
  cat("libcurl library not available. Need to install it.\n")
  install.packages("libcurl", repos = "http://cran.rstudio.com")
}

link = "https://www.dropbox.com/s/dxksv1vm470fh3g/OCD.csv?raw=1"
filename = "OCD.csv"
download.file(url = link, destfile = paste(dataDir, filename, sep = "/"), method="libcurl", quiet=F, mode="wb")
download.file(url = link, destfile = paste(backupDir, "Data", filename, sep = "/"), method="libcurl", quiet=F, mode="wb")
#from = paste(dataDir, filename, sep = "/"); to = paste(backupDir, "Data", filename, sep = "/")
#from; to
#file.copy(from, to, overwrite = recursive, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

# Save the Exercise script file
link = "https://www.dropbox.com/s/y81bai2vc0wo7vk/Exercise1.R?raw=1" 
#"http://www.openintro.org/stat/data/cdc.R"
filename = "Exercise1.R"
#print(paste("Your file is saved in: ", scriptDir, "/", filename, sep = ""))
download.file(url = link, destfile = paste(scriptDir, filename, sep = "/"), method="libcurl", quiet=F, mode="wb")
download.file(url = link, destfile = paste(backupDir, "Scripts", filename, sep = "/"), method="libcurl", quiet=F, mode="wb")
#file.copy(from, to, overwrite = recursive, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)

# Save the Homework script file
link = "https://www.dropbox.com/s/1g54c5uw5746e4i/HW1.R?raw=1" 
#"http://www.openintro.org/stat/data/cdc.R"
filename = "HW1.R"
#print(paste("Your file is saved in: ", scriptDir, "/", filename, sep = ""))
download.file(url = link, destfile = paste(scriptDir, filename, sep = "/"), method="libcurl", quiet=F, mode="wb")
download.file(url = link, destfile = paste(backupDir, "Scripts", filename, sep = "/"), method="libcurl", quiet=F, mode="wb")
#file.copy(from, to, overwrite = recursive, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)


cat("Your file(s) should be saved to their appropriate directories in:\n",
    psyc109Dir,"\n\nYou can now open your .R script file(s) in:\n", scriptDir)

#result <- list(psyc109Dir=psyc109Dir)
#return(result) # may not be needed because nothing is really returned
}
#print("Source() completed. Continue with code.")

getnewdata()