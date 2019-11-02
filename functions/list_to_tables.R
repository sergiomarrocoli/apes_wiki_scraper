########     processing scraped data into tables    ################

mini_table <- function(data, col_name){
  output <- data_frame(protected_area = unique(data), value = 1)
  colnames(output)[2] <- col_name
  output
}

page_completion <- function(data){
  threats_list <- output_keywords("threats_list", data)
  conservation_list <- output_keywords("conservation_activities_list", data)
  behaviour_list <- output_keywords("behaviours_list", data)
  impediments_list <- output_keywords("impediments_list", data)
  
  conservation <- cons_table(data)
  threats <- threats_table(data)
  site_chars <- site_characteristics_table(data)

  # get a list of sites  
  sites <- as.data.frame(site_list(data), stringsAsFactors = F)
  
  
  sites <- sites %>%
    left_join(mini_table(threats_list[, 3], "threat_list"), by = "protected_area") %>%
    left_join(mini_table(conservation_list[, 3], "conservation_list"), by = "protected_area") %>%
    left_join(mini_table(impediments_list[, 3], "impediments_list"), by = "protected_area") %>%
    left_join(mini_table(behaviour_list[, 3], "behaviours"), by = "protected_area") %>%
    left_join(mini_table(conservation[, 3], "conservation_table"), by = "protected_area") %>%
    left_join(mini_table(threats[, 3], "threat_table"), by = "protected_area") %>%
    left_join(mini_table(site_chars[, 3], "site_characteristics_table"), by = "protected_area")
  
  sites[is.na(sites)] <- 0
  
  sites$completion <- round((rowSums(sites[4:ncol(sites)])/(ncol(sites) - 3)) *100, 1)
  
  sites
}


site_list <- function(data){
  
  output <- list()
  list_id <- 1
  
  for(i in 1:length(data)){
    
    for(j in 1:length(data[[i]])){
      
      for(k in 1:length(data[[i]][[j]])){
        # browser()
        output[[list_id]] <- c(names(data[i]), names(data[[i]][j]),
                                   names(data[[i]][[j]][k]))
            list_id <- list_id + 1
      }
    }
  }
  output <- matrix(unlist(output), ncol = 3, byrow = T)
  colnames(output) <- c("region", "country", "protected_area")
  output
}
  





keyword_matrix <- function(data){
  
  # browser()
  
  key_word_lists <- c("threats_list", "conservation_activities_list", "impediments_list", "behaviours_list")

  output <- list()
  list_id <- 1
  
  # browser()
  
  for(i in 1:length(data)){

    for(j in 1:length(data[[i]])){

      for(k in 1:length(data[[i]][[j]])){

        for(key in key_word_lists){
          print(paste(i, k, key))
          tryCatch(
            if(!is.na(data[[i]][[j]][[k]][[key]])){
              output[[list_id]] <- c(names(data[i]), names(data[[i]][j]),
                names(data[[i]][[j]][k]),
                names(data[[i]][[j]][[k]][key]),
                data[[i]][[j]][[k]][[key]])
            list_id <- list_id + 1
            })
          
        }
      }
    }
    
  }
  
  browser()
  output <- matrix(unlist(output), ncol = 5, byrow = T)
  colnames(output) <- c("region", "country", "site", "type", "keyword")
  
  # drop e.g.
  # output <- output[-c(grep("^e.g.", trimws(output[, 5]))), ]
  # remove citations
  output[, 5] <- gsub("\\(.*\\)", "", output[, 5])
  

  output

}



output_keywords <- function(type, data){
  
  
  output <- keyword_matrix(data)
  
  # browser()
  
  output <- output[output[, 4] == type, -4]
  
  keywords <- trimws(unique(tolower(unlist(strsplit(output[, 4], ",")))))
  keyword_storage <- matrix(nrow = nrow(output), ncol = length(keywords))
  colnames(keyword_storage) <- keywords
  
  browser()
  
  for(i in 1:nrow(output)){
    for (keyword in keywords){
      if(grepl(keyword, tolower(output[i, 4]))){
        keyword_storage[i, keyword] <- 1
      }
    }
    keyword_storage[is.na(keyword_storage)] <- 0
  }
  
  output <- as.data.frame(cbind(output[, 1:3], keyword_storage), stringsAsFactors = F)

}







cons_table <- function(data){

  output <- list()
  list_id <- 1
  
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      for(k in 1:length(data[[i]][[j]])){
          if(!is.na(data[[i]][[j]][[k]][["conservation_actions_table"]][1])){
            for(activity in data[[i]][[j]][[k]][["conservation_actions_table"]]){
              if(activity[[2]] != ""){
                output[[list_id]] <- c(names(data[i]), names(data[[i]][j]),
                                       names(data[[i]][[j]][k]),
                                       activity[[1]], activity[[2]])
                list_id <- list_id + 1
              }
            }
          }
        }
    }
  }
  output <- matrix(unlist(output), ncol = 5, byrow = T)
  colnames(output) <- c("region", "country", "site", "conservation_activity", "presence")
  output
}





threats_table <- function(data){
  
  output <- list()
  list_id <- 1
  
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      for(k in 1:length(data[[i]][[j]])){
        if(!is.na(data[[i]][[j]][[k]][["threats_table"]][1])){
          for(activity in data[[i]][[j]][[k]][["threats_table"]]){
            if(activity[[2]] != ""){
              output[[list_id]] <- c(names(data[i]), names(data[[i]][j]),
                                     names(data[[i]][[j]][k]),
                                     activity[[1]], activity[[2]])
              list_id <- list_id + 1
            }
          }
        }
      }
    }
  }
  output <- matrix(unlist(output), ncol = 5, byrow = T)
  colnames(output) <- c("region", "country", "site", "threat", "presence")
  output
}






site_characteristics_table <- function(data){
  
  output <- list()
  list_id <- 1
  
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      for(k in 1:length(data[[i]][[j]])){
        for(l in 1:length(data[[i]][[j]][[k]][["site_characteristics"]])){
          if(!is.na(data[[i]][[j]][[k]][["site_characteristics"]][[l]])){
            # browser()
            output[[list_id]] <- c(names(data[i]), names(data[[i]][j]),
                                   names(data[[i]][[j]][k]),
                                   names(data[[i]][[j]][[k]][["site_characteristics"]][l]),
                                   data[[i]][[j]][[k]][["site_characteristics"]][[l]])
            list_id <- list_id + 1
          }
        }
      }
    }
  }

  output <- matrix(unlist(output), ncol = 5, byrow = T)
  colnames(output) <- c("region", "country", "site", "characteristic", "value")
  output
}




# 
# 
# 
# # , "surveys_table"
# # , "site_characteristics"
# wiki_table_data(data)
# 
# names(data[["West Africa"]][["Liberia"]][["Test"]][["site_characteristics"]])[[1]]
# names(data[["West Africa"]][["Liberia"]][["Test"]][["site_characteristics"]])
# data[["West Africa"]][["Liberia"]][["Test"]][["site_characteristics"]]


# data[["West Africa"]][["Liberia"]][[1]][["site_characteristics"]]

# names(data[["West Africa"]][["Liberia"]][["Test"]][["surveys_table"]])
# data[["West Africa"]][["Liberia"]][["Test"]][["surveys_table"]]
# 
# 
# names(data[["West Africa"]][["Liberia"]][["Test"]][["surveys_table"]])
# data[["West Africa"]][["Liberia"]][["Test"]][["surveys_table"]]
# 
# d
# 
# 
# 
# 


survey_table <- function(data){
  
  output <- list()
  list_id <- 1
  
  for(i in 1:length(data)){
    for(j in 1:length(data[[i]])){
      for(k in 1:length(data[[i]][[j]])){
        if (!is.na(data[[i]][[j]][[k]][["surveys_table"]][1])){
          for(entry in data[[i]][[j]][[k]][["surveys_table"]]){
            output[[list_id]] <- c(names(data[i]), names(data[[i]][j]),
                                   names(data[[i]][[j]][k]),
                                   entry[[1]], entry[[2]], entry[[3]], entry[[4]])
            list_id <- list_id + 1
          }
        }
      }
    }
  }
  output <- matrix(unlist(output), ncol = 7, byrow = T)
  colnames(output) <- c("region", "country", "site", "species", "date", "estimate", "CIs")
  output
}







# 
# c
# 
# 
# 
# 
# 
# 
# test_matrix <- matrix(unlist(data), ncol = 5, byrow = T)
# 
# rbind(test, 5)
# 
# names(data[[1]])[[1]]
# 
# list_lengths <- vector(length = length(test))
# for(i in 1:length(test)){ 
#   list_lengths[i] <- length(test[[i]])
#   if(length(test[[i]]) > 5) browser()
# }
# table(list_lengths)
# 
# 
# 
# 
# names(data)
# names(data[["West Africa"]][[1]])
# 
# 
# data[["West Africa"]][["Liberia"]][["Test"]][["threats_list"]]
# data[["West Africa"]][["Liberia"]][["Test"]][["conservation_activities_list"]]
# data[["West Africa"]][["Liberia"]][["Test"]][["impediments_list"]]
# data[["West Africa"]][["Liberia"]][["Test"]][["behaviours_list"]]
# 
# 
# 
# 
# 
# # # tables
# # [["threats_table"]]
# # [["conservation_actions_table"]]
# # [["surveys_table"]]
# # [["site_characteristics"]]