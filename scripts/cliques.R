

library(tidyverse)
library(zipcode)
library(visNetwork)
source('functions/dplyr_helpers.R')
source('functions/general_functions.R')
source('scripts/graph/i_graph.R')
load('data/zip_county_fips.RData')
load('data/codes.RData')
load('data/zip_code_database.R')

# clique analysis ---------------------------------------------------------

num_cliques <- clique.number(g_nonproft)

graph_cliques <- cliques(g_nonproft)

# clique 2 ----------------------------------------------------------------

clique_len_2 <- map(graph_cliques,function(x){
    length(x) == 2
})%>% flatten_lgl()

clique_2 <- graph_cliques[clique_len_2]%>% flatten_df() %>% names()

# clique 3 ----------------------------------------------------------------

clique_len_3 <- map(graph_cliques,function(x){
    length(x) == 3
})%>% flatten_lgl()

clique_3 <- graph_cliques[clique_len_3] %>% flatten_df() %>% names()

# clique 4 ----------------------------------------------------------------

clique_len_4 <- map(graph_cliques,function(x){
    length(x) == 4
})%>% flatten_lgl()

clique_4 <- graph_cliques[clique_len_4]%>% flatten_df() %>% names()

# clique 5 ----------------------------------------------------------------

clique_len_5 <- map(graph_cliques,function(x){
    length(x) == 5
})%>% flatten_lgl()

clique_5 <- graph_cliques[clique_len_5] %>% flatten_df() %>% names()

# integrate into tibble ---------------------------------------------------

clique_df <- tibble(
    id = V(g_nonproft)$name
    ) %>% 
    mutate(clique2  = ifelse(id %in% clique_2,1,0),
           clique3  = ifelse(id %in% clique_3,1,0),
           clique4  = ifelse(id %in% clique_4,1,0),
           clique5  = ifelse(id %in% clique_5,1,0)) %>% 
    rowwise() %>% 
    mutate(max = sum(clique2,clique3,clique4,clique5))

# join --------------------------------------------------------------------

nodes <- left_join(nodes,clique_df)

