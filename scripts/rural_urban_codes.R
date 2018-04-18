
# LIbraries  --------------------------------------------------------------

library(tidyverse)
library(visNetwork)
library(zipcode)
# files are df_org_vis
source('scripts/i_graph.R')
data("zipcode")



# Join df_org_vis & zip_code ----------------------------------------------

names(zipcode)
df_org_vis$nodes$zip
