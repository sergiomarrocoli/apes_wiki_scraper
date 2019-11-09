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
    html_table
  if(length(basic_information_table) > 0){
    basic_information_table[[1]][0:4, 2]
  }
}


# ape status

get_ape_status <- function(page){
  ape_status_table <- page %>% 
    html_nodes('.population-estimate-table') %>%
    html_table
  if(length(ape_status_table) > 0){
    ape_status_table <- ape_status_table[[1]] %>%
      mutate_all(as.character)
    colnames(ape_status_table) <- c("Species", "Year","Abundance estimate (95% confidence interval)",
                                    "Density estimate (per km²)", "Encounter rate (nests/km)", "Area",                                        
                                    "Method", "Source", "Comments", "A.P.E.S. database ID")
    ape_status_table
  }
}

# threats

get_threats_table <- function(page){
  threats_table <- page %>% 
    html_nodes('.threats-table') %>%
    html_table
  if(length(threats_table) > 0){
    threats_table <- threats_table[[1]] %>%
      mutate_all(as.character)
    colnames(threats_table) <- c("Category", "Specific threats", "Threat level", 
                                 "Quantified severity", "Description", "Year of threat")
    threats_table
  }
}


# conservation activities

get_conservation_activities_table <- function(page){
  conservation_activities_table <- page %>% 
    html_nodes('.conservation-actions-table') %>%
    html_table
  if(length(conservation_activities_table) > 0){
    conservation_activities_table <- conservation_activities_table[[1]] %>%
      mutate_all(as.character)
    colnames(conservation_activities_table) <- c("Category", "Specific activity",
                                                 "Description", "Year of activity")
    conservation_activities_table
  }
}


# impediments activities

get_impediments_table <- function(page){
  impediments_table <- page %>% 
    html_nodes('.impediments-table') %>%
    html_table
  if(length(impediments_table) > 0){
    impediments_table <- impediments_table[[1]] %>%
      mutate_all(as.character)
    colnames(impediments_table) <- c("Impediment", "Source")
    impediments_table
  }
}

# behaviours activities

get_behaviours_table <- function(page){
  behaviours_table <- page %>% 
    html_nodes('.behaviors-table') %>%
    html_table
  if(length(behaviours_table) > 0){
    behaviours_table <- behaviours_table[[1]] %>%
      mutate_all(as.character)
    colnames(behaviours_table) <- c("Behaviour", "Source")
    behaviours_table
  }
}

# switch columns
switch_columns <- function(data_table){
  data_table[c((ncol(data_table)-2):ncol(data_table), 1:(ncol(data_table)-3))]
}



# functions to get data

get_all_site_tables <- function(root_url){
  
  print("getting site urls")
  all_site_urls <- get_all_site_urls(root_url)
  
  site_characteristics_table <- data.frame(Region=character(),
                                           Country=character(),
                                           Site=character(),
                                           Area=character(),
                                           Coordinates=character(),
                                           Designation=character(),
                                           Habitat_types=character(),
                                           stringsAsFactors=FALSE)
  
  ape_status_table <- data.frame(Region=character(),
                                 Country=character(),
                                 Site=character(),
                                 Species=character(),
                                 Year=character(),
                                 'Abundance estimate (95% confidence interval)'=character(),
                                 'Density estimate (per km²)'=character(),
                                 'Encounter rate (nests/km)'=character(),
                                 Area=character(),
                                 Method=character(),
                                 Source=character(),
                                 Comments=character(),
                                 'A.P.E.S. database ID'=character(),
                                 stringsAsFactors=FALSE)
  
  threats_table <- data.frame(Region=character(),
                              Country=character(),
                              Site=character(),
                              Category=character(),
                              'Specific threats'=character(),
                              'Threat level'=character(),
                              'Quantified severity'=character(),
                              'Year of threat'=character(),
                              stringsAsFactors=FALSE)
  
  conservation_activities_table <- data.frame(Region=character(),
                              Country=character(),
                              Site=character(),
                              Category=character(),
                              'Specific activity'=character(),
                              'Description'=character(),
                              'Year of activity'=character(),
                              stringsAsFactors=FALSE)

  impediments_table <- data.frame(Region=character(),
                                              Country=character(),
                                              Site=character(),
                                              Impediment=character(),
                                              Source=character(),
                                              stringsAsFactors=FALSE)
  
  behaviours_table <- data.frame(Region=character(),
                                  Country=character(),
                                  Site=character(),
                                  Behaviour=character(),
                                  Source=character(),
                                  stringsAsFactors=FALSE)

  for(site_url in all_site_urls){
    site_page <- tryCatch(read_html(paste(root_url, site_url, sep = "")), error=function(e) FALSE)
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
        
        ape_status <- get_ape_status(site_page)
        if(length(ape_status) > 1){
          ape_status$Region <- location_data[1]
          ape_status$Country <- location_data[2]
          ape_status$Site <- location_data[3]
          ape_status_table <- rbind(ape_status, ape_status_table)
        } else {
          print(paste(location_data[3], "ape status table not added"))
        }
        
        threats <- get_threats_table(site_page)
        if(length(threats) > 1){
          threats$Region <- location_data[1]
          threats$Country <- location_data[2]
          threats$Site <- location_data[3]
          threats_table <- rbind(threats, threats_table)
        } else {
          print(paste(location_data[3], "threats table not added"))
        }
        
        conservation_activities <- get_conservation_activities_table(site_page)
        if(length(conservation_activities) > 1){
          conservation_activities$Region <- location_data[1]
          conservation_activities$Country <- location_data[2]
          conservation_activities$Site <- location_data[3]
          conservation_activities_table <- rbind(conservation_activities, conservation_activities_table)
        } else {
          print(paste(location_data[3], "conservation activities table not added"))
        }
        
        impediments <- get_impediments_table(site_page)
        if(length(impediments) > 1){
          impediments$Region <- location_data[1]
          impediments$Country <- location_data[2]
          impediments$Site <- location_data[3]
          impediments_table <- rbind(impediments, impediments_table)
        } else {
          print(paste(location_data[3], "impediments table not added"))
        }
        
        behaviours <- get_behaviours_table(site_page)
        if(length(behaviours) > 1){
          behaviours$Region <- location_data[1]
          behaviours$Country <- location_data[2]
          behaviours$Site <- location_data[3]
          behaviours_table <- rbind(behaviours, behaviours_table)
        } else {
          print(paste(location_data[3], "behaviours table not added"))
        }
      }
    }
    closeAllConnections()
  }
  list(site_characteristics_table,
      switch_columns(ape_status_table),
      switch_columns(threats_table),
      switch_columns(conservation_activities_table), 
      switch_columns(impediments_table), 
      switch_columns(behaviours_table))
}
