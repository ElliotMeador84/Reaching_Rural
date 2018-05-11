
library(igraph)
library(tidyverse)
library(visNetwork)
source('scripts/data_import.R')
file.edit('scripts/data_import.R')


# working file is bravo

# Create Bipartite Graph --------------------------------------------------

g_org <- graph.data.frame(bravo, directed = FALSE)

#====

V(g_org)$type <- bipartite.mapping(g_org)$type #also creates the bipartite network
proj_net <- bipartite.projection(g_org)

# ====

g_nonproft <- proj_net$proj2


# create VisNetwork Graphs ====



df_org_vis <- toVisNetworkData(g_nonproft)




