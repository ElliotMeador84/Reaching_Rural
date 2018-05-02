

library(tidyverse)
library(zipcode)
library(visNetwork)
source('functions/dplyr_helpers.R')
source('functions/general_functions.R')
source('scripts/graph/i_graph.R')
load('data/zip_county_fips.RData')
load('data/codes.RData')

# file.edit('scripts/graph/i_graph.R')


# Merge workingdata with nodes ---------------------------------------

#########    Always join by more
## tip ##     than one variable
#########       when possible


# join attributes -----------------------------------------------------
names(working.cd.data)
names(zipcode)

org_data_att <- working.cd.data %>%
    distinct() %>%
    select(org_name, zip = zip_code, TOT_REV_PRIOR,
           CONTRIB_ALL,
           city = CITY) %>%
    mutate(zip = as.numeric(zip),
           label = clean(org_name),
           city = clean(city)) %>%
    select(-org_name) %>%
    distinct()

common_names(df_org_vis$nodes,org_data_att)

df_org_vis$nodes <- df_org_vis$nodes %>% 
    mutate(city = clean(city))

nodes <- left_join(df_org_vis$nodes,org_data_att)

edges <- df_org_vis$edges

# break --------------------------------------------------------------

alfa <- zip_county_fips %>%
    rename(County_Name = COUNTYNAME) %>%
    filter(STATE == 'MO') %>% 
    mutate(County_Name = clean(County_Name))

foxtrot <- codes %>%
    filter(State == 'MO') %>% 
    mutate(County_Name = clean(County_Name))

codes_zip <- left_join(foxtrot, alfa)


# break --------------------------------------------------------------


tango <- codes_zip %>%
    select(zip = ZIP,
           Description)

zip_double <- tango %>% 
    distinct() %>% 
    group_by(zip) %>% 
    count() %>% 
    filter(n >1) %>% 
    pull(zip)    
  

tango %>% 
    filter(zip %in% zip_double) %>% 
    group_by(zip,Description) %>% 
    count()
    
    
# breaks -------------------------------------------------------------
common_names(tango,nodes)
names(nodes)
whisky <- left_join(
    distinct(nodes),
    distinct(tango)
) 

# break --------------------------------------------------------------
names(whisky)

id_double <- whisky %>% 
    group_by(id) %>% 
    count() %>% 
    filter(n >1) %>% 
    pull(id)

whisky %>% 
    filter(id %in% id_double) %>% 
    group_by(zip) %>% 
    count(Description) %>% 
    arrange(desc(n))
    
    
    
    
    
    

# break ---------------------------------------------------------------




visNetwork(edges = edges, nodes = nodes) %>%
    visInteraction(navigationButtons = TRUE)
