library(rvest)
library(tidyverse)
library(magrittr)

url_list_outdoor <- list(
  out_2023 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608",
  #  out_2022 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3730&season_hnd=568",
  #  out_2021 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3200&season_hnd=530",
  #  out_2020 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2906&season_hnd=496",
  #  out_2019 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2573&season_hnd=453",
  #  out_2018 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2278&season_hnd=414",
  # NCAA?
  #  out_2017 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1915&season_hnd=377",
  # NCAA?
  #  out_2016 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1683&season_hnd=336",
  #  out_2015 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1552&season_hnd=303",
  #  out_2014 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1251&season_hnd=256",
  #  out_2013 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1047&season_hnd=221",
  #  out_2012 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=863&season_hnd=191",
  #  out_2011 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=695&season_hnd=158",
  out_2010 = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131"
)

year_code <- list(
  "2011" = "696",
  "2012" = "907",
  "2013" = "1130",
  "2014" = "1338",
  "2015" = "1513",
  "2016" = "1765",
  "2017" = "1974",
  "2018" = "2231",
  "2019" = "2604",
  "2020" = "3099",
  "2021" = "3429",
  "2022" = "4153" # verify
)

event_codes_men_track <- list(
  "100" = "6",
  "200" = "7",
#  "400" = "11",
#  "800" = "12",
#  "1500" = "13",
#  "Mile" = "15",
#  "3000" = "18",
#  "5000" = "21",
#  "10000" = "22",  # "100H" = "4",
#  "110H" = "5",
#  "400H" = "9",
  "3000S" = "19"
)

event_codes_men_track_relay <- list(
  "4x100" = "31",
  "4x400" = "33", 
  "SMR (800)" = "123",
  "DMR" = "38"
  )

event_codes_men_field <- list(
 # 'HJ' = "23",
  "PV" = "24",
  "LJ" = "25",
  "TJ" = "26",
  "SP" = "30",
  "DT" = "27",
  "HT" = "28",
  "JT" = "29",
  #  "Hep" = "40",
  "Dec" = "39"
)


event_codes_women_track <- list(
  "100" = "6",
  "200" = "7",
  "400" = "11",
  "800" = "12",
  "1500" = "13",
  "Mile" = "15",
  "3000" = "18",
  "5000" = "21",
  "10000" = "22",
  "100H" = "4",
#  "110H" = "5",
  "400H" = "9",
  "3000S" = "19"
)

event_codes_women_track_relay <- list(
  "4x100" = "31",
  "4x400" = "33", 
  "4x800" = "34", 
  "DMR"   = "38"
  )

event_codes_women_field <- list(
  'HJ' = "23",
  "PV" = "24",
  "LJ" = "25",
  "TJ" = "26",
  "SP" = "30",
  "DT" = "27",
  "HT" = "28",
  "JT" = "29"
  #  "Hep" = "40",
#  "Dec" = "39"
)

url_base         <- "https://www.tfrrs.org/"
url_perf         <- "all_performances/"
url_state        <- "CA_"
url_level        <- "college_"
url_gender       <- "m_"
url_college      <- "Claremont_Mudd_Scripps"
url_ext          <- ".html?"
url_list_query   <- "list_hnd="
url_year_code    <- "4153"
url_season_query <- "&season_hnd="
url_season_num   <- "608" 
url_event_code   <- "#event"
url_event_num    <- "6"

url_full <- paste(
  url_base, url_perf, url_state, url_level, url_gender, url_college, 
  url_ext, url_list_query, 
  url_year_code, 
  url_season_query, url_season_num,
  url_event_code, url_event_num, sep = ""
)

build_url <- function(
      url_base, url_perf, url_state, url_level, 
      url_gender, url_college, 
      url_ext, url_list_query, 
      url_year_code, 
      url_season_query, url_season_num,
      url_event_code, url_event_num
    ) {
  
  url_full = paste(
    url_base, url_perf, url_state, url_level, url_gender, url_college, 
    url_ext, url_list_query, 
    url_year_code, 
    url_season_query, url_season_num,
    url_event_code, url_event_num, 
    sep = ""
  )
  return(url_full)
}


get_html_data_from_url <- function(url) {
  html_data = url %>%
    rvest::read_html(.) %>%
    rvest::html_node("table") %>% 
    rvest::html_table()
  
  html_data = html_data[, -1] # clean up the data frame
  
  return(html_data)
}

add_new_variables <- function(
    data, 
    team = "CMS", 
    #year = "2022",
    #sex  = "m",
    event = "100"
    ) {
  
  #sex = ifelse(sex == "m", "Men", "Women")
  
  data = data %>%  # add key variables 
    dplyr::mutate(., 
                  team_var = team,
                  year_var = year,
                  sex_var = sex,
                  event_var = event
    )
  return(data)
}

build_file_name <- function(
    #year   = "2022", 
    team   = "CMS",
    event
    #gender = "m"
    ) {
  
  name = paste(year, "tfrrs", team, gender, event, sep = "_")
  file_name = paste(here::here("data", "tfrrs", name), ".csv", sep = "")
  return(file_name)  
}


write_data <- function(data, file) {
  message(paste("Saving:", file))
  write.csv(data, file = file, row.names = F)
}


###########################################
# Running the program
###########################################
###########################################

get_tfrrs <- function() {
  url_page = build_url
  ...
}

url_page <- build_url(
  url_base, url_perf, url_state, url_level, 
  url_gender, url_college, 
  url_ext, url_list_query, 
  url_year_code, 
  url_season_query, url_season_num,
  url_event_code, url_event_num
)

for (e in names(event_codes_men_track)) {
  message(e)
}
  
  

# loop through the urls and events
url_list = url_list_outdoor
event_codes = event_codes_men_track

for (u in 1:length(url_list)) {
  
  url = url_list[u]
#  message(url)
  

  # then build event page
  for (e in 1:length(event_codes)) {
    event_url = paste(url, "#event", event_codes[e], sep = '')
    #message(event_url)
    
    # event name and code
    event = names(event_codes)[e]
    #message(event)

    code  = event_codes[e]
    #message(code)
    
    # then read the page
    data <- get_html_data_from_url(event_url)
    
    Sys.sleep(.25 * 60) #delay timer
    
    # then add variables
    #data <- add_new_variables(data) # add new variables
    
    # set file name
    file_name <- here::here("data", "tfrrs", paste(
      paste(names(url_list)[u], "CMS", event, sep = "_"), 
      ".csv", sep = ""
      ))
    # write data to disk
    write_data(data = data, file = file_name)
    
  }
}
  
  
  
  



# read the data
data <- get_html_data_from_url(url_page) #%>% add_new_variables(data) # add new variables

data <- add_new_variables(data) # add new variables

view_html(add_new_variables(data))

# set file name
file_name <- build_file_name()

# write data to disk
write_data(data = data, file = file_name)

###########################################
# End running the program
###########################################


#url_list <- "/SCIAC_Outdoor_Performance_List"
#gender_list <- c("?gender=m", "?gender=f")

#page <- read_html("https://www.tfrrs.org/results_search.html")
#page <- rvest::read_html("https://tf.tfrrs.org/lists/3781/SCIAC_Performance_List?gender=m")

#url_base <- "https://www.tfrrs.org/lists/"
#url_list <- "/SCIAC_Outdoor_Performance_List"
#gender_list <- c("?gender=m", "?gender=f")

#build_url_list 
#https://www.tfrrs.org/lists/3429/SCIAC_Outdoor_Performance_List?gender=f#event6




# codes for tesing
tfrrs_year_code <- list("2011" = "696")
event_codes_men <- list("100" = "6")
  
# get data for men
for (year in 1:length(year_code)) {
  
  the_url = paste(url_base, 
                  year_code[year], 
                  url_list, sep = "")
  
  year = names(year_code)[year]
  
  # update the url
  the_url = paste(the_url, "?gender=m", sep = "")
  
  # then append the event
  for (e in 1:length(event_codes_men)) {
    
    # the url up with gender 
    in_url = the_url
    
    # get event 
    event = event_codes_men[e]
    event_name = names(event_codes_men)[e]
    # update the url
    out_url = paste(in_url, "#event", event, sep = "")
    
    message(out_url)
    
    # get the data
    the_page = rvest::read_html(out_url)
    
    dat = the_page %>% 
      rvest::html_node("table") %>% 
      rvest::html_table() 
    
    dat = dat[,-1]
     
    #print(names(dat))
    
    # add new variables for easy reading
    dat = dat %>% 
      dplyr::mutate(., 
                    year_name = year,
                    sex_name = "men",
                    event_name = event_name
                    )
    # clean names
    names(dat) = gsub(" ", "_", names(dat))
    
    #print(names(dat))
    # set the file name
    file_name = paste0(paste(year, "tfrrs", "men", event_name, sep = "_"), ".csv")
    
    #message(file_name)
    
    # write the data
    write.csv(dat, 
              here::here("data", "tfrrs", file_name),
              row.names = FALSE
              )
    }
message(paste("File:", file_name, "saved."))
}


#https://www.tfrrs.org/lists/696/SCIAC_Outdoor_Performance_List
#"https://www.tfrrs.org/lists/696/SCIAC_Outdoor_Performance_List"

