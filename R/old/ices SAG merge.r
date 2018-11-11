# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph merge with 'old' dataset
#
# 15/07/2017 integrated the new and old stocknames; only data download and storage as rdata files
# -----------------------------------------------------------------------------------------------

library(dplyr)     # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files

setwd("D:/Dropbox/ICES Assessment database")

# Load utils code
source("D:/XXX/PRF/r/my_utils.r")

# load csv with old and new stocknames
stocknames <-
  read.csv("D:/Dropbox/ICES Assessment database/ICES StocksOldCodesNewCodesEGs.csv", stringsAsFactors = FALSE) %>% 
  select(1,2) %>% 
  setNames(., c("fishstockold","fishstock")) 

# load SAG database and combined database and then stop from here
load(file="rdata/sagdb.RData")
load(file="rdata/stockdb.RData")
load(file="rdata/combdb.RData")

# ----------------------------------------------------------------------

# keep only fishstock and assessment year of the SAG database
x <-
  sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  select(fishstock, assessmentyear)

# read old database
rbya <-
  read_excel("ICES Assessment Summary database.xlsx",
             sheet = "DATA",
             col_names = TRUE,
             col_types = "text",
             skip = 0) %>%
  lowcase %>%
  rename(assessmentyear = assyear) %>% 
  mutate(fishstock = tolower(fishstock)) %>%
  mutate_at(vars("year","assessmentyear"), funs(as.integer)) %>% 
  mutate_at(vars("lowrecruitment", "recruitment","highrecruitment",
                 "lowssb","ssb","highssb",
                 "lowf", "f","highf",
                 "landings","catches","discards","ibc",
                 "flim","fpa","fmsy", "fmanagement",
                 "blim","bpa","msybtrigger","bmanagement",
                 "recruitmentage","recruitmentlength"), funs(as.numeric)) %>% 
  select(-contains("custom")) %>% 
  rename(scientificname = speciesname,
         commonname     = sgname,
         fmgt           = fmanagement,
         bmgt           = bmanagement,
         faocode        = species) %>% 
  select(-assessmentkey, -icesareas, -report, -(unitofrecruitment:tbiomassunit), -catcheslandingsunits, -officiallandings,
         -(ibc:yieldssb), -(flandings:funallocated), -(flength:area)) %>% 
  filter(year <= assessmentyear) %>% 
  
  # dealing with old and new stocknames
  left_join(stocknames, by = c("fishstock" = "fishstockold")) %>% 
  rename(fishstocknew = fishstock.y) %>% 
  left_join(stocknames, by = c("fishstock")) %>% 
  mutate(fishstocknew = ifelse(is.na(fishstocknew) & !is.na(fishstockold), fishstock, fishstocknew),
         fishstockold = ifelse(is.na(fishstockold) & !is.na(fishstocknew), fishstock, fishstockold),
         fishstockold = ifelse(is.na(fishstockold) & is.na(fishstocknew) , fishstock, fishstockold)) 

# Extract list of fishstock and assessment years from old database
y <-
  rbya %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  select(fishstock, assessmentyear)

# Construct an rbya dataset to add to the SAG dataset, i.e. unique combinations of fishstock and assessmentyear
rbya_toadd <-
  setdiff(y,x) %>% 
  left_join(rbya, by=c("fishstock","assessmentyear")) %>% 
  data.frame()

# Add the rbya_toadd to the sagdb
comb <- rbind.all.columns(sagdb, rbya_toadd) 

save(comb, file="rdata/combdb.RData")

# ---------------------------------------------------------------------------------------------
# Provide overview of assessments
# ---------------------------------------------------------------------------------------------

comb %>% 
  filter(grepl("old", fishstockold) ) %>% 
  View()

comb %>% 
  group_by(fishstockold, assessmentyear) %>%
  summarise(nyears = n()) %>% 
  dcast(fishstockold ~ assessmentyear, fun.aggregate=sum, margins=TRUE) %>% 
  View()

# check for empty fishstockold
sagdb %>% 
  filter(is.na(fishstockold)) %>% 
  View()


  

