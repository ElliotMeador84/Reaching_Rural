
library(igraph)
library(tidyverse)
library(visNetwork)
source('scripts/data_import.R')

# working file is bravo

# Create Bipartite Graph --------------------------------------------------

g_org <- graph.data.frame(bravo, directed = FALSE)

#====

V(g_org)$type <- bipartite.mapping(g_org)$type #also creates the bipartite network
proj_net <- bipartite.projection(g_org)

#====

g_nonproft <- proj_net$proj2

#====
#

plot(g_nonproft,vertex.label = NA)
#node attributes ====

org_attributes <- data_frame(org_name = V(g_nonproft)$name)
# only need the org attributes, not the edge (name) attributes
zulu <- bravo %>% 
  select(org_name, 
         city,
         zip = zip_code) %>% 
  distinct(.)

Nodes <- left_join(org_attributes,zulu)

V(g_nonproft)$city <- Nodes$city
V(g_nonproft)$zip <- Nodes$zip

# create VisNetwork Graphs ====

df_org_vis <- toVisNetworkData(g_nonproft)


































