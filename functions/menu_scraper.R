# library(rvest)

url <- "http://apes.eva.mpg.de/apeswiki/index.php/Main_Page"

# test <- apes_scraper(url)


##############  Top level scraper - starter function   #############
apes_scraper <- function(url){#
  start_time <- Sys.time()
  
  site_page <- read_html(url)
  
  region_list <- site_page %>%
  html_nodes("#p-navigation") %>%
    html_nodes("a")
  region_list <- region_list[2:5]
  
  wiki_list <- list()
  
  for (region in region_list){
     wiki_list[[trimws(html_text(region))]] <-  region_scraper(paste("http://apes.eva.mpg.de", 
                                                                     html_attr(region, "href"), sep = ""))
  }
  
  print (Sys.time() - start_time)
  wiki_list
  
}

# region_url <- "http://apes.eva.mpg.de/apeswiki/index.php/Central_Africa"


##############  Region scraper, iterates over countries in a region  #############
region_scraper <- function(region_url){
  
  region_page <- read_html(region_url)
  countries <- region_page %>%
    html_node("#bodyContent") %>%
    html_nodes("li") %>%
    html_nodes("a")
  
  country_list <- list()
  
  for(country in countries){
    country_list[[trimws(html_text(country))]] <- country_scraper(paste("http://apes.eva.mpg.de", 
                                                                     html_attr(country, "href"), sep = ""))
  }
  
  country_list
}

country_url <- "http://apes.eva.mpg.de/apeswiki/index.php/Borneo"

##############  Country scraper, iterates over sites in a country  #############
country_scraper <- function(country_url){
  country_page <- read_html(country_url)
  sites <- country_page %>%
    html_node("#bodyContent") %>%
    html_nodes("li")
    
  sites <- sites[!grepl("\\.\\.\\.", html_text(sites))]
  sites <- html_nodes(sites, "a")
  
  site_list <- list()
  
  for(site in sites){
    site_list[[trimws(html_text(site))]] <- site_scraper(paste("http://apes.eva.mpg.de", 
                                                               html_attr(site, "href"), sep = ""))
  }
  
  site_list
  
}



