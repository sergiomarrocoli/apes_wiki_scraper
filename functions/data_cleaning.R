
library(tidyr)
library(dplyr)


# data <- apes_scraper(url)

sites_completion <- page_completion(data)
hist(sites_completion$completion)



threats_list <- output_keywords("threats_list", data)
conservation_list <- output_keywords("conservation_activities_list", data)
behaviour_list <- output_keywords("behaviours_list", data)
impediments_list <- output_keywords("impediments_list", data)

conservation <- cons_table(data)
threats <- threats_table(data)
site_chars <- site_characteristics_table(data)
sites <- site_list(data)

hist(keyword_output)

colSums(keyword_output[4:ncol(keyword_output)])