# Library,data load in ---------------------------------------------------

library(tidyverse)
library(visNetwork)
working.cd.data <- read_csv('data/Org_Data_original_Guidestar.csv')
save(working.cd.data,file = 'data/working.cd.data.RData')
load('data/working.cd.data.RData')

files <- paste0('functions/',list.files('functions/'))
map(files,source)
# Clean data -------------------------------------------------------------

(bravo <- working.cd.data[c(1,16:length(working.cd.data))] %>% 
    set_names(clean(names(.))) %>% 
    mutate_if(is.character,~clean(.)))

