gsheet2xlsx <- function(gsheetname, xlsxname = gsheetname, db_public = "G:/Dropbox/Public/", url, ...) {
        # load libs load libs and allow for authentication
        library(googlesheets); suppressMessages(library(dplyr)) 
        file <- googlesheets::gs_title(gsheetname) # get the file by title
        file.key <- file$sheet_key # get the google filekey
        file.url <- file$browser_url #get the url
       # file %>% googlesheets::gs_download(to = paste0(xlsxname, ".xlsx"), overwrite = TRUE) # download as Exce.
       # file %>% googlesheets::gs_download(to = paste0("X:/Progs/_install/R/Exercises/", f$name,".xlsx"), overwrite = TRUE) 
        # copy the google drive file to .../DropBox/Public/... dir 
        file %>% googlesheets::gs_download(to = paste0(db_public, xlsxname,".xlsx"), overwrite = TRUE) 
        # make a list of file info 
        file.info = list(name = gsheetname, key = file.key, url = file.url)
        return(file.info)
}

# copy to .../DropBox/Public/... dir 
#gsheet2xlsx("RGSheetTest")  # copies to google drive and dropbox
#
gsheet2xlsx("Psyc109_2016") #

getClassData <- function(filename = NULL, sheet = "Sheet1", DropBoxUserId = "6036547") {
        # reads in an xlsx file from a dropbox location; currently my public folder
        if(is.null(filename)) { message("\nOoops! No file specified.") 
        } else { 
        library(repmis)
        DropBox.url <- paste0("https://dl.dropboxusercontent.com/u/",DropBoxUserId,"/",filename)
        df = repmis::source_XlsxData(paste0(DropBox.url, "?raw=1"), sheet = sheet)
        return(df)
        }
}
#
# source("https://www.dropbox.com/s/z7k0xoch7a83umy/getClassData.R?raw=1")

qnorm(.005, 0, 1)
qnorm(.005, 100, 15, lower.tail = F)
pnorm(-2, 0, 1)


#getClassData(filename = "RGSheetTest.xlsx", sheet = "Ex2")

#DF1 <- getClassData();

# assign data to data frames
#

gsheet2xlsx("Psyc109_2016") #
DF1 = getClassData(filename = "Psyc109_2016.xlsx", sheet = "Ex1"); DF1 #RGSheetTest.xlsx
DF2 = getClassData(filename = "Psyc109_2016.xlsx", sheet = "Ex2"); DF2 
DF3 = getClassData(filename = "Psyc109_2016.xlsx", sheet = "Ex5"); DF3 

#DF2 <- getClassData(filename = "RGSheetTest.xlsx", sheet = "Ex2"); DF2

library(xlsx)
library(XLConnect)
tmp = tempfile(fileext = ".xlsx")
#repmis::source_XlsxData(paste0(DropBox.url, "?raw=1")
#download.file(url = "https://dl.dropboxusercontent.com/u/6036547/Psyc109_2016.xlsx", destfile = tmp, mode="wb")
download.file(url = "https://dl.dropboxusercontent.com/u/6036547/Psyc109_2016.xlsx", destfile = "X:/Public/zzz.xlsx", mode = "wb")

gsheet2xlsx("RGSheetTest"); #DF? = getClassData(filename = "RGSheetTest.xlsx", sheet = "?"); DF?

download.ClassData <- function(filenames = "Psyc109_2016.xlsx", location = getwd()) {
        if(is.null(filenames)) { 
                message("Error: 'filenames' argument is not specfied in function.")
        } else {   
                download.file(url = paste0("https://dl.dropboxusercontent.com/u/6036547/", filenames), 
                              destfile = paste0(location, "/", filenames), mode = "wb")
        }
}


source("https://dl.dropboxusercontent.com/u/6036547/109_2016_f.txt")
download.ClassData()
download.ClassData("Psyc109_2016.xlsx")
download.ClassData("main.zip")

download.HWData <- function(files = NULL) {
        if (is.null(files)) { message("Error: 'files' argument not defined.") }
        for (file in files) {
                download.file(url = paste0("https://dl.dropboxusercontent.com/u/6036547/", file), destfile = file, mode = "wb")
        }
}


x = c("hi"); y = "there"
append(x, y)

for (file in c("main.zip", "zt.txt")) {
        download.file(url = paste0("https://dl.dropboxusercontent.com/u/6036547/", file), destfile = file, mode = "wb")
}
files = c("main.zip", "zt.txt")
sapply(files, function(x) { download.file(url = paste0("https://dl.dropboxusercontent.com/u/6036547/",x), destfile = x, mode = "wb")})


library(xlsx)
DF1 = read.xlsx("Psyc109_2016.xlsx", sheetName = "Ex1"); DF1
library(DT)
datatable(DF1)

write.xlsx(DF1, "zzz.xlsx")
shell("zzz.xlsx")

source("https://dl.dropboxusercontent.com/u/6036547/Psyc109_2016.xlsx")

?download.file
# http://www.sporcle.com/games/mk4293/antonymmovietitles
library(lubridate)
t = c("1:20", "2:31", "0:00")
time <- 300 - (lubridate::period_to_seconds(ms(t)))

        
        
# old  data
getwd(); setwd("C:/users/gcook/desktop/Psyc109")
library(foreign)
OLD <- read.spss("movie_titles.sav")
View(OLD)

with(OLD, mean(movietitles))

# the t test 
with(OLD, t.test(movietitles, mu = 78.18))

