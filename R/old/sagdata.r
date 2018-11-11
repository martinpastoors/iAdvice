# -----------------------------------------------------------------------------------------------
# sagdata.r
#
# download SAG data
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
# 03/11/2017 split off into separate SAG download code file
# -----------------------------------------------------------------------------------------------

# ICES Stock database

# library(devtools)
# devtools::install_github("ices-tools-prod/icesSD")
# devtools::install_github("ices-tools-prod/icesSAG")
library(icesSD)  # ICES Stock database
library(icesSAG)  # ICES Stock Assessment Graphs

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# -----------------------------------------------------------------------------------------
# Download or load the ICES webservices data
# -----------------------------------------------------------------------------------------

# Stock database
getSD         <- icesSD::getSD()
save(getSD        , file=paste(dropboxdir, "/rdata/getSD.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/getSD.RData",sep=""))

# List stocks
getListStocks <- icesSAG::getListStocks(year=0)
save(getListStocks, file=paste(dropboxdir, "/rdata/getListStocks.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/getListStocks.RData",sep=""))

# SAG
getSAG        <- icesSAG::getSAG(stock=NULL, year=0, data="summary", combine=TRUE)
save(getSAG       , file=paste(dropboxdir, "/rdata/getSAG.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/getSAG.RData",sep=""))

# filter(getSAG, fishstock %in% c("her-noss","her.27.1-24a514a"), AssessmentYear==2017) %>% 
# write.csv(., file="her-noss2017.csv")

# refpoints        <- icesSAG::getSAG(stock=NULL, year=0, data="refpoints", combine=TRUE)
# refpoints <- icesSAG::getFishStockReferencePoints(icesSAG::findAssessmentKey(year=0))

# -------------------------------------------------------------------------------------------------
# iStock, iStockkey, iRename
# Generate iStock (by stock, assessment year and date) - information on stock assessment properties
# Note: iStock read from Excel file because substantial amount of information added, e.g. areas
# -------------------------------------------------------------------------------------------------

# load iStock etc datafiles
# load(file=paste(dropboxdir, "/rdata/iStock.RData", sep=""))
# load(file=paste(dropboxdir, "/rdata/iStockkey.RData", sep=""))
load(file=paste(dropboxdir, "/rdata/iRename.RData", sep=""))

# -----------------------------------------------------------------------------------------
# iAssess_part1: download standard graph data SAG and prepare for combining
# -----------------------------------------------------------------------------------------

sagdata <- 
  getSAG %>% 
  lowcase() %>% 
  dplyr:: select(stockkeylabel=fishstock, assessmentyear, year,
                 recruitment, highrecruitment, lowrecruitment,
                 ssb, highssb, lowssb,
                 f, highf, lowf, 
                 catches, landings, discards,
                 fage, 
                 units, 
                 stocksizedescription, stocksizeunits,
                 fishingpressuredescription, fishingpressureunits,
                 stockpublishnote) %>% 
  
  mutate(assessmenttype2 = "update",
         datepublished   = as.Date(NA)) %>% 
    
  # dealing with old and new stocknames
  left_join(iRename, by = c("stockkeylabel")) %>% 
  
  # mutate tolower
  mutate_at(vars("stocksizedescription","stocksizeunits","fishingpressuredescription","fishingpressureunits"), 
            funs(tolower)) %>% 
  
  mutate(source = "sag",
         ibc    = NA )%>% 
  ungroup() %>% 
  data.frame() 

# save dataset
save(sagdata, file=paste(dropboxdir, "/rdata/sagdata.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/sagdata.RData",sep=""))

# filter(iAssess_part1, grepl("whb", stockkeylabel)) %>% View()
# filter(iAssess_part1_unique, grepl("nop", stockkeylabel)) %>% View()
# glimpse(iAssess_part1)

