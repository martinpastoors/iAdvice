# -----------------------------------------------------------------------------------------------
# calculate Fcv and Fphi from retrospective data
#
# 27/02/2019 first coding
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(directlabels)  # for printing labels at end of geom lines
library(scales)    # scales and formatting

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

load(file=paste(advicedir, "/rdata/iAssess.RData",sep=""))


# -----------------------------------------------------------------------------------------
# Select stocks to work on
# -----------------------------------------------------------------------------------------

iAssess %>% filter(grepl("her", stockkeylabelold)) %>% distinct(stockkeylabelold)

d <-
  iAssess %>%
  
  filter(grepl("her-vian|her-67bc", stockkeylabelold)) %>% 
  filter(assessmentyear >= 2000) %>% 
  filter(!grepl("benchmark|exploratory", purpose)) %>% 
  filter(fishingpressureunits %in% c("per year","year-1")) 

## NEED TO BE FINISHED FROM HERE


