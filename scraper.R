# Parsing of HTML/XML files  
library(rvest)    

source('functions/new_site_functions.R')

root_url <- "https://apeswiki.eva.mpg.de"

all_site_urls <- get_all_site_urls(root_url)
basic_site_information_table <- get_basic_information_data(all_site_urls)







