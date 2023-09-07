library(rvest)
library(tidyverse)
library(magrittr)

##########################################################
# Extract links from a url
##########################################################
scrap_links_from_url <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  Sys.sleep(10)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

#view_html(scrap_links_from_url(page))
##########################################################
##########################################################

##########################################################
##########################################################
#### get the links to save


##########################################################
##########################################################

# then create list and add names based on links
#event_list <- vector("list", length(event_links$link))
#names(event_list) <- event_links$link

# change the names

read_tables_from_page <- function(page) {
  # read tables from url to list
  tables = page %>% rvest::read_html() %>% rvest::html_table()
  
  Sys.sleep(10)
  
  return(tables)
}  

add_names_to_list_elements <- function(
    table_list,
    list_names
) {
  
  # set names to list elements
  names(table_list) = list_names
  
  return(table_list)
}

#length(table_list)

# then remove missing cols
add_new_cols <- function(table_list, 
                         team = "",
                         event = "",
                         season = "",
                         location = ""
                         ) {
  lapply(table_list, function(x) {
  #print(names(x))
  dat = x %>% # keep names that are not missing
    select(., names(x)[nzchar(names(x))])
  
  names(dat) = gsub(" ", "_", names(dat))
  #print(dat)
  # add new variables
  dat = dat %>% 
    mutate(., 
           Team = team,
           Event = event,
           Season = season,
           Location = location,
    )
  
})
}

###################################################################
# Modify and save data frame
###################################################################
modify_and_save <- function(table_list, location = "Outdoor") {

for (x in 1:length(table_list)) {
  
  #location = location
  
  dat = table_list[[x]]
  dat$Location = location
  dat$Season = stringr::str_split_fixed(dat$Meet_Date, " ", 3)[,3]
  dat$Event = names(table_list)[[x]]
  team      = unique(dat$Team)
  event     = unique(dat$Event)
  season    = unique(dat$Season)
  
  #print(dat)
  write.csv(dat,
            file = here::here("data", "tfrrs", 
                              paste(
                                paste(season, "CMS", team, location, event, sep = "_"), 
                                ".csv", sep = "")
            ),
            row.names = F
  )
}
}

##########################################################
# List of Urls ###########################################
##########################################################
# The outdoor URLs
stag_url_list_outdoor <- list(
  "2023" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608",
  "2022" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3730&season_hnd=568",
  "2021" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3200&season_hnd=530",
  "2020" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2906&season_hnd=496",
  "2019" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2573&season_hnd=453",
  "2018" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2278&season_hnd=414",
  # NCAA?
  "2017" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1915&season_hnd=377",
  # NCAA?
  "2016" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1683&season_hnd=336",
  "2015" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1552&season_hnd=303",
  "2014" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1251&season_hnd=256",
  "2013" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1047&season_hnd=221",
  "2012" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=863&season_hnd=191",
  "2011" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=695&season_hnd=158",
  "2010" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131"
)

athena_url_list_outdoor <- list(
  "2023" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608",
  "2022" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=3730&season_hnd=568",
  "2021" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=3200&season_hnd=530",
  "2020" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=2906&season_hnd=496",
  "2019" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=2573&season_hnd=453",
  "2018" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=2278&season_hnd=414",
  # NCAA?
  "2017" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1915&season_hnd=377",
  # NCAA?
  "2016" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1683&season_hnd=336",
  "2015" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1552&season_hnd=303",
  "2014" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1251&season_hnd=256",
  "2013" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1047&season_hnd=221",
  "2012" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=863&season_hnd=191",
  "2011" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=695&season_hnd=158",
  "2010" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131"
)

##########################################################
# RUNNING THE PROGRAM ####################################
##########################################################
# get url and links

for (page_url in stag_url_list_outdoor) {
  #page_url = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608"
  
  message(paste("Reading:", page_url))
  
  links <- scrap_links_from_url(page_url) %>% 
    filter(., stringr::str_detect(url, "event")) %>%
    mutate(.,
         link = gsub(" ", "", as.character(link)),
         link = gsub(",", "", as.character(link)),
  )
  

  # get links and update tables in list
  page_data <- read_tables_from_page(page_url)
  
  # read tables if present
  if (length(page_data > 0)) {
    
    # replace list names with link names
    table_list <- add_names_to_list_elements(page_data, links$link)
    
    # add new columns
    table_list_new <- add_new_cols(table_list, team = "Stag")
    
    # then modify and save
    modify_and_save(table_list_new, location = "Outdoor")
    
  } else {
        message(paste("No tables available at url."))
  }
}

###################################################################
# Get Athena Data
###################################################################

for (page_url in athena_url_list_outdoor) {

  message(paste("Reading:", page_url))
  
  links <- scrap_links_from_url(page_url) %>% 
    filter(., stringr::str_detect(url, "event")) %>%
    mutate(.,
           link = gsub(" ", "", as.character(link)),
           link = gsub(",", "", as.character(link)),
    )
  
  # get links and update tables in list
  page_data <- read_tables_from_page(page_url)
  
  # read tables if present
  if (length(page_data > 0)) {
    
    # replace list names with link names
    table_list <- add_names_to_list_elements(page_data, links$link)
    
    # add new columns
    table_list_new <- add_new_cols(table_list, team = "Athena")
    
    # then modify and save
    modify_and_save(table_list_new, location = "Outdoor")
    
  } else {
    message(paste("No tables available at url."))
  }
}


##########################################################
# END RUNNING THE PROGRAM ################################
##########################################################  
