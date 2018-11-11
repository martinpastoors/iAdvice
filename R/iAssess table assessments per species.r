# -----------------------------------------------------------------------------------------------
# iAssess table assessments per species
#
# 25/09/2017 taken from ices SAG data checker.r
# -----------------------------------------------------------------------------------------------

setwd("D:/Dropbox/ICES Assessment database")

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours

# Load utils code
source("D:/XXX/PRF/r/my_utils.r")

# Load dataset
load(file="rdata/sagdb.RData")

# ---------------------------------------------------------------------------------------------
# generate table
# ---------------------------------------------------------------------------------------------

sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  filter(assessmentyear >= 2001) %>% 
  mutate(species = paste0(commonname," (", faocode,")"), sep="") %>% 
  group_by(species, assessmentyear) %>% 
  summarise(nassess = n()) %>% 
  
  dcast(species ~ assessmentyear, sum, value.var="nassess", margins = TRUE) %>% 
  no.emphasis.table() %>% 
  select(-1) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "right",
               missing=".",
               round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
               caption="number of assessments per species")



