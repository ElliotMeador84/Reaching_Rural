# Library,data load in ---------------------------------------------------

library(tidyverse)
library(visNetwork)
# working.cd.data <- read_csv('C:/Users/emeador/Desktop/R/SNA/csv/working.cd.data.csv')
# save(working.cd.data,file = 'data/working.cd.data.RData')
load('data/working.cd.data.RData')
source('C:/R/functions.R')
source('C:/R/Helper_Functions/dplyr_helpers.R')



# Clean data -------------------------------------------------------------

working.cd.data %>% head()
working.cd.data %>% names()


bravo <- working.cd.data %>% 
    set_names(clean(names(.))) %>% 
    mutate_if(is.character,~clean(.)) %>% 
    select(-x1)

#====
bravo





