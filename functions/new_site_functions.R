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
  # main_page <- GET("https://apeswiki.eva.mpg.de", authenticate(username, password))
  main_page <- content(GET(url, authenticate(username, password)))
  region_links <- main_page %>% 
    html_nodes('.body')
  region_links[2] %>% 
    html_nodes('a') %>% 
    html_attr('href')
}

get_country_urls <- function(root_url, region_url){
  region_page <- tryCatch(content(GET(paste(root_url, region_url, sep = ""), authenticate(username, password))), error=function(e) FALSE)
  if(length(region_page) > 1){
    region_page %>% 
      html_nodes('.mw-parser-output li a') %>% 
      html_attr('href')
  }
}

get_site_urls <- function(root_url, url){
  # country_page <- tryCatch(read_html(paste(root_url, url, sep = "")), error=function(e) FALSE)
  # print('getting site urls')
  country_page <- tryCatch(content(GET(paste(root_url, url, sep = ""), authenticate(username, password))), error=function(e) FALSE)
  if(length(country_page) > 1){
    country_page %>% 
      html_nodes('.mw-parser-output li a') %>% 
      html_attr('href')
  }
}


# get location_data

get_region_country_and_site <- function(page){
  location_data <- page %>% 
    html_nodes('.mw-parser-output p a') %>% 
    html_text()
  location_data[1:3]
}


# basic site information

get_site_characteristics <- function(page){
  basic_information_table <- page %>% 
    html_nodes('.basic-information') %>%
    html_table(fill = TRUE)
  if(length(basic_information_table) > 0){
    basic_information_table[[1]][0:4, 2]
  }
}


# switch columns
switch_columns <- function(data_table){
  data_table[c((ncol(data_table)-2):ncol(data_table), 1:(ncol(data_table)-3))]
}


# function to get data

get_all_site_tables <- function(root_url){
  
  all_site_urls <- get_all_site_urls(root_url)
  
  site_characteristics_table <- data.frame(Region=character(),
                                           Country=character(),
                                           Site=character(),
                                           Area=character(),
                                           Coordinates=character(),
                                           Designation=character(),
                                           'Habitat types'=character(),
                                           check.names=FALSE,
                                           stringsAsFactors=FALSE)
  
  ape_status_table <- data.frame(Species=character(),
                                 Year=character(),
                                 'Abundance estimate (95% confidence interval)'=character(),
                                 'Density estimate (per km²)'=character(),
                                 'Encounter rate (nests/km)'=character(),
                                 Area=character(),
                                 Method=character(),
                                 Source=character(),
                                 Comments=character(),
                                 'A.P.E.S. database ID'=character(),
                                 Region=character(),
                                 Country=character(),
                                 Site=character(),
                                 check.names=FALSE,
                                 stringsAsFactors=FALSE)
  
  threats_table <- data.frame(Category=character(),
                              'Specific threats'=character(),
                              'Threat level'=character(),
                              'Quantified severity'=character(),
                              'Year of threat'=character(),
                              Region=character(),
                              Country=character(),
                              Site=character(),
                              check.names=FALSE,
                              stringsAsFactors=FALSE)
  
  conservation_activities_table <- data.frame(Category=character(),
                                              'Specific activity'=character(),
                                              'Description'=character(),
                                              'Year of activity'=character(),
                                              Region=character(),
                                              Country=character(),
                                              Site=character(),
                                              check.names=FALSE,
                                              stringsAsFactors=FALSE)

  impediments_table <- data.frame(Impediment=character(),
                                  Source=character(),
                                  Region=character(),
                                  Country=character(),
                                  Site=character(),
                                  check.names=FALSE,
                                  stringsAsFactors=FALSE)
  
  behaviours_table <- data.frame(Behaviour=character(),
                                 Source=character(),
                                 Region=character(),
                                 Country=character(),
                                 Site=character(),
                                 check.names=FALSE,
                                 stringsAsFactors=FALSE)

  for(site_url in all_site_urls){
    site_page <- tryCatch(content(GET(paste(root_url, site_url, sep = ""), authenticate(username, password))), error=function(e) FALSE)
    if(length(site_page) > 1){
      location_data <- get_region_country_and_site(site_page)
      if(!is.na(location_data[1])){
        print(paste("getting tables for", location_data[3]), sep = " ")
        
        site_characteristics <- get_site_characteristics(site_page)
        if(length(site_characteristics) > 1){
          site_characteristics_table[nrow(site_characteristics_table) + 1, ] <- c(location_data, site_characteristics)
        } else {
          print(paste(location_data[3], "basic information table not added"))
        }
        
        ape_status_table <- rbind(ape_status_table, get_table(site_page, '.population-estimate-table', ape_status_table, location_data))
        site_threats_table <- rbind(threats_table, get_table(site_page, '.threats-table', threats_table, location_data))
        site_impediments_table <- rbind(impediments_table, get_table(site_page, '.impediments-table', impediments_table, location_data))
        conservation_activities_table <- rbind(conservation_activities_table, get_table(site_page, '.conservation-actions-table', conservation_activities_table, location_data))
        behaviours_table <- rbind(behaviours_table, get_table(site_page, '.behaviors-table', behaviours_table, location_data))
        impediments_table <- rbind(impediments_table, get_table(site_page, '.impediments-table', impediments_table, location_data))
      
      }
    }
    closeAllConnections()
  }
  list(site_characteristics_table,
      switch_columns(ape_status_table),
      switch_columns(site_threats_table),
      switch_columns(conservation_activities_table), 
      switch_columns(impediments_table), 
      switch_columns(behaviours_table))
}





get_table <- function(site_page, selector, main_table, location_data){
  table_data <- site_page %>%
    html_nodes(selector) %>%
    html_table(fill = TRUE)
  if(length(table_data) > 0){
    table_data <- table_data[[1]] %>%
      mutate_all(as.character)
    colnames(table_data) <- colnames(main_table)[1:(ncol(main_table)-3)]

    table_data$Region <- location_data[1]
    table_data$Country <- location_data[2]
    table_data$Site <- location_data[3]
    table_data
    } else {
    print(paste(location_data[3], selector, "table not added"), sep = "")
    NA
  }
}
