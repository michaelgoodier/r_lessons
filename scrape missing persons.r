#scrape missing persons
library(tidyverse)
library(rvest) #for getting structured data from html pages

#first we need to get the urls of the results pages
#https://missingpersons.police.uk/en-gb/case-search/yk3j442?page=2&orderBy=dateDesc
#we generate urls with paste0()

results_pages <- paste0("https://missingpersons.police.uk/en-gb/case-search/yk3j442?page=",1:56,"&orderBy=dateDesc&setPerPage=100")

#we need function to scrape results pages and get the links to individual notices

scrape_results_page <- function(url){
  
  cat(url,"\n") #output the url we are scraping
  
  page <- read_html(url) #read the page
  
  thumbnail_links <- page %>%
    html_elements("a.CaseThumbnail") %>% #get all links with class CaseThumbnail
    html_attr("href") %>% # get the href
    paste0("https://missingpersons.police.uk",.) # add the beginning bit of the URL back
  
}


#we need to apply the function to the list of results pages
#we do this with map()
all_notice_links <- map(results_pages,scrape_results_page)
#we have a list() object of lists that we need to unlist()
all_notice_links_unlisted <- unlist(all_notice_links) %>%
  str_subset("case") #we only want the urls with "case" in the url

#now we need a function to scrape our notice pages
scrape_notice <- function(url){
  cat(url,"\n") #output url to console
  
  page <- read_html(url) #read the page
  
  #scrape the main table
  table_of_details <- page %>%
    html_element("div.CaseData") #get the container of the data
  
  keys <- table_of_details %>%
    html_elements("div.Key") %>% #there are boxes called key and others called value, lets get both
    html_text(trim=T) 
  
  values <- table_of_details %>%
    html_elements("div.Value") %>%
    html_text(trim=T)
  
  data <- tibble(keys,values) #join our keys and values
  
  #scrape the location
  road <- page %>%
    html_element("div.Road") %>%
    html_text(trim=T)
  
  county <- page %>%
    html_element("div.County") %>%
    html_text(trim=T)
  
  country <- page %>%
    html_element("div.Country") %>%
    html_text(trim=T)
  
  #we now need our data to all be in one row
  final_data <- data %>%
    pivot_wider(names_from = "keys",values_from = "values") %>%
    mutate(road,county,country)
  
  return(final_data)
} #end of function

#now we need to run our function on our notice urls
data <- map_dfr(all_notice_links_unlisted,scrape_notice)
