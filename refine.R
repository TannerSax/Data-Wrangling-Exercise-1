library(dplyr)
library(tidyverse)


refine <- read_csv("SpringBoard Projects/Refine.csv")


refine <- refine %>% 
  mutate(company = ifelse(grepl("^Ph|^ph|^fi", company, ignore.case = FALSE), "philips", company))  %>%
  mutate(company = ifelse(grepl("^ak|^Ak|^AK", company, ignore.case = FALSE), "akzo", company))     %>%
  mutate(company = ifelse(grepl("^Van|^van", company, ignore.case = FALSE), "van_houten", company)) %>%
  mutate(company = ifelse(grepl("^uni|^Uni", company, ignore.case = FALSE), "unilever", company))

refine <- refine %>%  separate(`Product code / number`, c("product_code", "product_number"), "-")

refine <- refine %>% 
  mutate(product_category = ifelse(grepl("p", product_code), "Smartphone", product_code)) %>% 
  mutate(product_category = ifelse(grepl("v", product_code), "TV", product_category)) %>% 
  mutate(product_category = ifelse(grepl("x", product_code), "Laptop", product_category)) %>% 
  mutate(product_category = ifelse(grepl("q", product_code), "Tablet", product_category))
         
         
refine <- refine %>% mutate(full_address = paste(address, city, country, sep = ', '))

refine <- refine %>% 
  mutate(company_philips = ifelse(company == "philips", 1, 0)) %>% 
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = ifelse(company == "van_houten", 1, 0)) %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0)) %>%
  mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>%
  mutate(product_TV = ifelse(product_category == "TV", 1, 0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>%
  mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0)) 





