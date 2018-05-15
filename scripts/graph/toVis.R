

library(tidyverse)
library(zipcode)
library(visNetwork)
library(sjPlot)
source('functions/dplyr_helpers.R')
source('functions/general_functions.R')
source('scripts/graph/i_graph.R')
load('data/zip_county_fips.RData')
load('data/codes.RData')
# file.edit('scripts/graph/i_graph.R')
load('data/zip_code_database.R')

# Merge workingdata with nodes ---------------------------------------

#########    Always join by more
## tip ##     than one variable
#########       when possible


# join attributes -----------------------------------------------------

visNetwork(df_org_vis$nodes,df_org_vis$edges)

#====
# names(zip_code_database) <- clean(names(zip_code_database))
# 
# bravo <- bravo %>% 
#     rename(zip = zip_code)

# 
# org_data_att <- left_join(bravo[-1],zip_code_database)
# org_data_att <- distinct(org_data_att)


#=====



org_data_att_clean <- org_data_att %>% 
    mutate_if(is.character,clean) %>% 
    select(zip,county,latitude,longitude)


#====

df_org_vis$nodes <- df_org_vis$nodes %>% 
    mutate_if(is.character,clean)

nodes <- left_join(df_org_vis$nodes,org_data_att_clean) %>% distinct()

edges <- df_org_vis$edges

# get clean description vary --------------------------------------------------------------

alfa <- zip_county_fips %>%
    rename(County_Name = COUNTYNAME) %>%
    filter(STATE == 'MO') %>% 
    mutate(County_Name = clean(County_Name))

foxtrot <- codes %>%
    filter(State == 'MO') %>% 
    mutate(county = clean(County_Name)) %>% 
    select(-County_Name) %>% 
    set_names(clean(names(.)))


nodes <- left_join(nodes,foxtrot) 

    

# merge attributes to 'nodes' ---------------------------------------------


#====== colors


color_df <- tibble(Description = c(
    'Metro > 1m',
    'Metro 250k - 1m',
    'Metro < 250k',
    'Urban > 20k',
    'Urban > 20k*',
    'Urban 2.5k - 19.9k',
    'Urban 2.5k - 19.9k*',
    'Rural < 2.5k',
    'Rural < 2.5k*')) %>% 
    mutate(color = RColorBrewer::brewer.pal(9,'Blues'))%>% 
    set_names(clean(names(.)))



nodes <- left_join(nodes,color_df) %>% 
    as_tibble()

# size


size_df <- tibble(id = V(g_nonproft)$name,
       degree = degree(g_nonproft))

nodes <- left_join(nodes,size_df) %>% as_tibble()


# size rescale -------


nodes$size <- rescale(nodes$degree,30,100)

# add cliques -------------------------------------------------------------
file.edit('scripts/cliques.R')
source('scripts/cliques.R')

# Plot ---------------

# think about plotting 
# using only three colors
# the 9 colors does not show up well,
# maybe white, grey and black.

visNetwork(edges = edges, nodes = nodes) %>%
    visNodes(physics = T) %>% 
    visInteraction(navigationButtons = TRUE)

# Table 2 Network characterstics  ====
nodes$description <- factor(nodes$description,
levels = c('Metro > 1m',
'Metro 250k - 1m',
'Metro < 250k',
'Urban > 20k',
'Urban > 20k*',
'Urban 2.5k - 19.9k',
'Urban 2.5k - 19.9k*',
'Rural < 2.5k',
'Rural < 2.5k*'))
# ====

Table_2 <- nodes %>% 
    mutate(degree_bin = ifelse(.$degree >0,1,0)) %>% 
    group_by(description) %>% 
    summarise('Total organisations' = n(),
              'Proportion with at least one tie' = scales::percent(mean(degree_bin)),
              'Average degree' = round(mean(degree),1),
              'Average clique size' = mean(max)) 
    
# write_csv(Table_2,'data/Table_2.csv')    
# 
# clique for major groups ==== 

rural_clique <- mean(c(0.4,0.57))
urban_clique <- mean(c(0.67,0.81,0.40))
metro_clique <- mean(c(0.63,0.54,0.57))

nodes %>% 
    mutate(bin = ifelse(max > 0,1,0)) %>% 
    select(description,bin) %>% 
    group_by(description) %>% 
    summarise(Average = mean(bin)) %>% 
    mutate(aggregate = c(rep('Metro',3),rep('Urban',4),rep('Rural',2))) %>% 
    ungroup() %>% 
    group_by(aggregate) %>% 
    mutate(`Average by aggregate` = scales::percent(mean(Average)))

# lat_long plot -----------------------------------------------------------


nodes %>% 
    unite('lat_long',c('latitude','longitude'),remove = F) %>% 
    group_by(lat_long) %>% 
    mutate(total = n()) %>% 
    ggplot(aes(longitude,latitude))+
    geom_point(aes(size = total,color = description),show.legend = T,alpha = .8)+
    coord_map()+
    theme_void()








    
    
    
    







