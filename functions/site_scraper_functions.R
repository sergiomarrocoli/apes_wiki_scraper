# to do:
# get rid of "e.g."s - turn to error, or if empty, just don't add



# list to tables




site_scraper <- function(url){
  
  site_page <- read_html(url)
  # keyword lists
  site_items[["threats_list"]] <- tryCatch(node_to_keywords(site_page, ".threats-list"), error=function(e) NA)
  site_items[["conservation_activities_list"]] <- tryCatch(node_to_keywords(site_page, ".conservation-activities-list"), error=function(e) NA)
  site_items[["impediments_list"]] <- tryCatch(node_to_keywords(site_page, ".impediments-list"), error=function(e) NA)
  site_items[["behaviours_list"]] <- tryCatch(node_to_keywords(site_page, ".behaviours-list"), error=function(e) NA)
  
  # tables
  site_items[["threats_table"]] <- tryCatch(table_to_list(site_page, ".threats-table", c(1, 2), "threat"), error=function(e) NA)
  site_items[["conservation_actions_table"]] <- tryCatch(table_to_list(site_page, ".conservation-actions-table", c(1, 2), "conservation_action"), error=function(e) NA)
  site_items[["surveys_table"]] <- tryCatch(table_to_list(site_page, ".population-estimate-table", c(1:4), "survey"), error=function(e) NA)
  site_items[["site_characteristics"]] <- tryCatch(characteristics_to_list(site_page, ".basic-information"), error=function(e) NA)

  site_items
  
}





characteristics_to_list <- function(page, node){
  
  table_list <- list()
  table_rows <- page %>% html_node(node) %>% html_nodes("tr")

  # iterate over rows
  for(i in 1:length(table_rows)){
    items <- tolower(gsub(":", "", strsplit(html_text(table_rows[i]), "\n")[[1]]))
    table_list[[items[1]]] <- trimws(items[2])
  }
   
  table_list
  
}






table_to_list <- function(page, node, indexes, hash_id){
  
  table_list <- list()
  table_rows <- page %>% html_node(node) %>% html_nodes("tr")
  
  keys <- trimws(tolower(html_text(html_nodes(table_rows[1], "th")[indexes])))
  
  # iterate over rows
  for(row in 2:length(table_rows)){
    # iterate over keys (columns)
    for(i in 1:length(keys)){
      table_list[[paste(hash_id, "_", row-1, sep = "")]][[keys[i]]] <- tolower(trimws(html_text(html_nodes(table_rows[row], "td")[indexes[i]])))
    }
  }
  
  table_list
 
}







# site scraper functions
node_to_keywords <- function(page, node){
  keywords <- page %>%
    html_node(node) %>%
    html_node("li") %>%
    html_text() 
  keywords <- strsplit(trimws(keywords), ":")[[1]][-1]
  # keywords <- strsplit(keywords, ",")[[1]]
  # tolower(trimws(keywords))
}

