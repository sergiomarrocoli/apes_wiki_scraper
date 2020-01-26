# Parsing of HTML/XML files  
library(rvest)
library(dplyr)
library(httr)

source('functions/new_site_functions.R')
# source('functions/login_function.R')
source('login_details.R')

root_url <- "https://apeswiki.eva.mpg.de"

all_tables <- get_all_site_tables(root_url)

basic_site_information_table <- all_tables[[1]]
ape_status_table <- all_tables[[2]]
threats_table <- all_tables[[3]]
conservation_activities_table <- all_tables[[4]]
impediments_table <- all_tables[[5]]
behaviours_table <- all_tables[[6]]






