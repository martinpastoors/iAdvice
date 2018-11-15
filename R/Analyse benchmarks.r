# -----------------------------------------------------------------------------------------------
# analyse benchmarks
#
# 15/11/2018 first coding
# -----------------------------------------------------------------------------------------------

library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)      # reshaping data; e.g. dcast
library(pander)        # for print tables
library(readxl)        # read excel files
library(lubridate)     # dates
library(cowplot)       # multiplots
library(directlabels)  # for printing labels at end of geom lines

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

# load(file=paste(advicedir, "/rdata/iStock.RData",sep=""))
# load(file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
# load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))

# -----------------------------------------------------------------------------------------
# number of benchmarks
# -----------------------------------------------------------------------------------------

iAdvice %>% 
  filter(grepl("benchmark", purpose) ) %>% 
  group_by(assessmentyear, purpose) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=assessmentyear, y=n, group=purpose)) +
  theme_publication() +
  geom_bar(aes(fill=factor(purpose)), stat="identity", position="stack")

