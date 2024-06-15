# this script scrapes the country profiles at 
# https://www.international.ucla.edu/asc/countries

library(tidyverse)
library(polite)
library(rvest)


session <- bow(
  url = "https://www.international.ucla.edu/asc/countries",
  force = TRUE
)

html_page <- scrape(session)

profile_urls <- html_page %>% 
  html_elements(".titleStyle") %>% 
  html_attr("href")

profile_urls <- paste0("https://www.international.ucla.edu", profile_urls)

# Function to clean string by removing \n and \r
clean_string <- function(input_string) {
  
  cleaned_string <- gsub("\n|\r", "", input_string)
  return(cleaned_string)
}

get_profiles <- function(profile_url) {
  
  profile_session <- bow(profile_url)
  profile_page <- scrape(profile_session)
  
  country_name <- profile_page %>% 
    html_element(".miniHeaders") %>% 
    html_text2()
  
  country_abstract <- profile_page %>% 
    html_element(".abstract") %>% 
    html_text2()
  if (country_name == "Egypt") {
    background_history <- profile_page %>% 
      html_element("#printTools > div > div > p:nth-child(8) > span") %>% 
      html_text2()
  } else {
    background_history <- profile_page %>% 
      html_element("#printTools > div > div > div:nth-child(8)") %>% 
      html_text2()
  }
  
  country_abstract <- clean_string(country_abstract)
  background_history <- clean_string(background_history)
  
  result <- list(
    country_name = country_name,
    country_abstract = country_abstract,
    background_history = background_history
  )
  
}

country_profiles <- map(profile_urls, get_profiles)

country_profiles_tbl <- bind_rows(country_profiles)

write_csv(country_profiles_tbl, "data/country_profiles.csv")









