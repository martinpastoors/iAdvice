# -----------------------------------------------------------------------------------------------
# exceldata.r
#
# 30/03/2017 first coding during HAWG
# 07/07/2017 adapted for downloading as csv file
# 14/07/2017 integrated the new and old stocknames; only data download and storage as rdata files
# 20/07/2017 thorough checking of databases and making sure all items are filled and OK
# 01/08/2017 added a number of diagnostic graphs; redone the excel database for nephrops
# 01/08/2017 added the stock assessment methods from stock advice database
# 10/08/2017 moved checking routines to separate code
# 11/08/2017 adapted for R3.4.1 and Tidyverse
# 14/08/2017 added assessmentmodel and assessmenttype categories
# 04/09/2017 updated for western horse mackerel 2017
# 06/09/2017 added automatic link to dropbox folder
# 10/09/2017 split all data into new databases: iAssess, iAdvice, iManage, iForecast, iSpecies, iStock
# 04/10/2017 updated with new ICES data on WGWIDE assessments
# 06/10/2017 added assessmenttype2 column in excel spreadsheet
# 09/10/2017 added qcsdata integration and added istock read from spreadsheet
# 16/10/2017 final check before meeting in Reykjavik
# 17/10/2017 updates during workshop in Reykjavik
# 23/10/2017 moved iStock.r to separate code file
# 03/11/2017 split excel datafile into separate code file
# -----------------------------------------------------------------------------------------------

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# load iRename
load(file=paste(dropboxdir, "/rdata/iRename.RData", sep=""))

# -----------------------------------------------------------------------------------------
# exceldata: Old excel assessment database and prepare for combining
# -----------------------------------------------------------------------------------------

exceldata <-
  readxl::read_excel(paste(dropboxdir, "/ICES Assessment Summary database.xlsx",sep=""),
             sheet = "DATA", 
             col_names = TRUE, 
             col_types = "text", 
             skip = 0) %>%
  lowcase %>%
  rename(assessmentyear = assyear, 
         stockkeylabel = fishstock) %>% 
  mutate(stockkeylabel = tolower(stockkeylabel),
         datepublished = as.Date(as.numeric(datepublished), origin="1899-12-30")) %>%
  mutate_at(vars("year","assessmentyear"), funs(as.integer)) %>% 
  mutate_at(vars("lowrecruitment", "recruitment","highrecruitment",
                 "lowssb","ssb","highssb",
                 "lowf", "f","highf",
                 "landings","catches","discards","ibc"), 
            funs(as.numeric)) %>%
  mutate_at(vars("stocksizedescription","stocksizeunits","fishingpressuredescription",
                 "fishingpressureunits"), 
            funs(tolower)) %>% 
  mutate(assessmenttype2 = ifelse(assessmentyear >= 2011 & assessmenttype2 == "assess", 
                                 "update",assessmenttype2)) %>% 
  ungroup() %>% 
  
  # select only the relevant fields (could be more though !!)
  select(stockkeylabel, assessmentyear, assessmenttype2, year, datepublished,
         lowrecruitment, recruitment, highrecruitment, 
         lowssb, ssb, highssb, 
         lowf, f, highf, 
         landings, catches, discards, ibc, 
         stocksizeunits, stocksizedescription, fishingpressuredescription, fishingpressureunits,
         fage, stockpublishnote = published) %>% 
  
  # dealing with old and new stocknames
  left_join(iRename, by = c("stockkeylabel")) %>% 
  filter(year <= assessmentyear) %>% 
  
  mutate(source = "excel",
         units = "")

# save dataset
save(exceldata, file=paste(dropboxdir, "/rdata/exceldata.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/exceldata.RData",sep=""))

# filter(exceldata, grepl("whb", stockkeylabel)) %>% View()
# setdiff(names(sagdata), names(exceldata))
# setdiff(names(exceldata), names(sagdata))
# filter(exceldata_toadd, grepl("whb", stockkeylabel)) %>% View()
# glimpse(exceldata)
# sortunique(exceldata$assessmenttype2)

