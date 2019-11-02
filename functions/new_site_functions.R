# functions to get urls

get_all_site_urls <- function(root_url){
  all_urls <- vector()
  regions <- get_region_urls(root_url)
  
  for(region in regions){
    country_urls <- get_country_urls(root_url, region)
    for(country in country_urls){
      site_urls <- get_site_urls(root_url, country)
      all_urls <- c(all_urls, site_urls)
    }
  }
  closeAllConnections()
  all_urls
}


get_region_urls <- get_country_urls <- function(url){
  main_page <- read_html(url)
  region_links <- main_page %>% 
    html_nodes('.body')
  region_links[2] %>% 
    html_nodes('a') %>% 
    html_attr('href')
}

get_country_urls <- function(root_url, region_url){
  region_page <- tryCatch(read_html(paste(root_url, region_url, sep = "")), error=function(e) FALSE)
  if(length(region_page) > 1){
    region_page %>% 
      html_nodes('.mw-parser-output li a') %>% 
      html_attr('href')
  }
}

get_site_urls <- function(root_url, url){
  country_page <- tryCatch(read_html(paste(root_url, url, sep = "")), error=function(e) FALSE)
  if(length(country_page) > 1){
    country_page %>% 
      html_nodes('.mw-parser-output li a') %>% 
      html_attr('href')
  }
}


# functions to get data

get_basic_information_data <- function(all_site_urls){
  
  site_characteristics_table <- data.frame(region=character(),
                                           country=character(),
                                           site=character(),
                                           area=character(),
                                           coordinates=character(),
                                           designation=character(),
                                           habitat_types=character(),
                                           stringsAsFactors=FALSE)
  
  for(site_url in all_site_urls){
    site_page <- tryCatch(read_html(paste(root_url, site_url, sep = "")), error=function(e) FALSE)
    if(length(site_page) > 1){
      location <- get_region_country_and_site(site_page)
      site_characteristics <- get_site_characteristics(site_page)
      if(length(site_characteristics) > 1){
        site_characteristics_table[nrow(site_characteristics_table) + 1, ] <- c(location, site_characteristics)
      } else {
        print(paste(site_url, "basic information table not added"))
      }
    }
    closeAllConnections()
  }
  
  site_characteristics_table
}


# get location

get_region_country_and_site <- function(page){
  location <- page %>% 
    html_nodes('.mw-parser-output p a') %>% 
    html_text()
  location[1:3]
}


# basic site information

get_site_characteristics <- function(page){
  basic_information_table <- page %>% 
    html_nodes('.basic-information') %>%
    html_table
  if(length(basic_information_table) > 0){
    basic_information_table[[1]][0:4, 2]
  }
}






