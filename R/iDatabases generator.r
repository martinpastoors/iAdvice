# -----------------------------------------------------------------------------------------------
# IDatabases generator.r
#
# iSpecies  : properties of species (names etc.)
# iStock    : properties of stocks and assessment
# iAssess   : stock assessment database (by stock, assessmentyear and year and date)
# iAdvice   : ICES advice and advice basis
# iForecast : short term forecasts
# iManage   : management actions and implementation
# iStockkey : link stockkeylabel with stockkey (number)
# iRename   : link stockkeylabelold and stockkeylabelnew to stockkey
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
# 06/10/2017 added assessmenttype column in excel spreadsheet
# 09/10/2017 added qcsdata integration and added istock read from spreadsheet
# 16/10/2017 final check before meeting in Reykjavik
# 17/10/2017 updates during workshop in Reykjavik
# 23/10/2017 moved iStock.r to separate code file
# 03/11/2017 now merges from separate datasets: qcsdata, exceldata, sagdata
# 04/11/2017 temporary special handling of her-noss 2017 revision
# 06/11/2017 removed datepublished from merging (need to check!!)
# 23/10/2018 full checking and updating of the code; now also using ICES SAGfull
# 08/11/2018 updated files on qcsexcel and iadvice; no longer need iStock
# -----------------------------------------------------------------------------------------------

rm(list=ls())

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

# -----------------------------------------------------------------------------------------
# load the Advice database
# -----------------------------------------------------------------------------------------

iAdvice <-
  readxl::read_excel(
    path= paste(advicedir, "/Excel/ICES Scientific Advice database 20181111.xlsx", sep=""), 
    sheet     = "DATA",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() %>% 
  rename(purpose = assessmentpurpose) %>% 
  
  mutate_at(c("advisedlandingsmin", "advisedcatchmin", 
              "advisedlandingsmax", "advisedcatchmax",
              "tal", "tac",
              "officiallandings", "landings", "ibc", "discards", "catches",
              "fsqay", "ssbay", "fadvmax",
              "fmax", "f01", "fmed", "f35spr", "flim", "fpa", "fmsy",
              "blim", "bpa", "msybtrigger",
              "m1", "m5"),
            funs(as.numeric)) %>%
  mutate_at(c("tacyear", "assessmentyear", "stockkey", "firstyearofdata", "ncpueseries","nsurveyseries"),
            funs(as.integer)) %>% 
  mutate_at(c("purpose", "speciescommonname"), 
            funs(tolower))
  

save(iAdvice, file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))
# glimpse(iAdvice)



# ---------------------------------------------------------------------------------------
# Read iSpecies from excel (not from ICES SAG because more information added to excel version). 
# -----------------------------------------------------------------------------------------

iSpecies <-
  readxl::read_excel(
    path=paste(advicedir, "/excel/species_list.xlsx",sep=""), 
    col_names=TRUE, 
    col_types="text") %>%
  mutate_at(c("speciescommonname","trophicguild","fisheriesguild","sizeguild"), 
            funs(tolower)) %>%
  group_by(speciesfaocode, speciesscientificname, speciescommonname) %>%
  arrange(speciesfaocode) 

save(iSpecies, file=paste(advicedir, "/rdata/iSpecies.RData",sep=""))

# -----------------------------------------------------------------------------------------
# Generate iRename, iStockkey
# -----------------------------------------------------------------------------------------

# generate iRename (linking stockkeylabel to stockkey)
iRename <-
  iAdvice %>% 
  group_by(stockkey, stockkeylabel) %>% 
  filter(row_number() ==1 ) %>% 
  dplyr::select(stockkey, stockkeylabel, stocklongname) %>% 
  mutate(speciesfaocode = substr(stockkeylabel,1,3)) 

iStockkey <-
  iAdvice %>% 
  group_by(stockkey) %>% 
  filter(!is.na(stocklongname)) %>% 
  filter(assessmentyear == max(assessmentyear) ) %>% 
  ungroup() %>% 
  dplyr::select(stockkey, stockkeylabelnew, stockkeylabelold, stockarea) %>% 
  distinct()
  # dplyr::select(stockkey, stockkeylabelnew, stockkeylabelold, stocklongname, speciescommonname, stockarea)

save(iRename,   file=paste(advicedir, "/rdata/iRename.RData",sep=""))
save(iStockkey, file=paste(advicedir, "/rdata/iStockkey.RData",sep=""))

# write.csv(iRename, file=paste(advicedir, "/excel/iRename.csv",sep=""), row.names = FALSE)
# write.csv(iStockkey, file=paste(advicedir, "/excel/iStockkey.csv",sep=""), row.names = FALSE)

# -----------------------------------------------------------------------------------------
# load Excel and QSC data
# -----------------------------------------------------------------------------------------

qcsexcel <-
  readxl::read_excel(
    path= paste(advicedir, "/excel/QCS and EXCEL Assessment Database combined.xlsx", sep=""), 
    sheet     = "data",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() %>% 
  
  mutate_at(c("assessmentyear","year", "recruitment","stocksize","fishingpressure", "catches"),   
            funs(as.numeric)) %>% 
  
  mutate_at(c("stockkey"),    funs(as.integer)) %>% 
  
  mutate_at(c("unitofrecruitment", "recruitmentdescription"),    funs(tolower)) %>% 
  
  mutate(recruitment       = ifelse(unitofrecruitment == "thousands", recruitment/1000, recruitment),
         unitofrecruitment = ifelse(unitofrecruitment == "thousands", "millions", unitofrecruitment),
         recruitment       = ifelse(unitofrecruitment == "billions", recruitment*1000, recruitment),
         unitofrecruitment = ifelse(unitofrecruitment == "billions", "millions", unitofrecruitment) ) %>% 
  
  # remove nephrops for now
  filter(tolower(substr(stockkeylabel,1,3)) != "nep") %>% 
  
  # make numeric
  mutate_at(c("recruitment","lowrecruitment","highrecruitment",  
              "tbiomass","lowtbiomass","hightbiomass", 
              "stocksize", "lowstocksize","highstocksize", 
              "catches", "landings","discards","ibc","unallocatedremovals",
              "fishingpressure", "lowfishingpressure","highfishingpressure",
              "fdiscards","flandings","fibc","funallocated"), 
            funs(as.numeric)) %>% 
  
  # make integer
  mutate_at(c("year", "assessmentyear", "recruitmentage"), funs(as.integer)) %>% 
  
  # make logical
  mutate_at(c("published"),  funs(as.logical)) %>% 
  
  # make date
  mutate( assessmentdate = as.Date(as.numeric(assessmentdate), origin="1899-12-30")) %>% 
  
  # make lowercase
  mutate_at(c("purpose"), funs(tolower)) %>% 
  
  # distinct rows only
  distinct()
  


save(qcsexcel, file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
# glimpse(qcsexcel)
# unique(qcsexcel$assessmentdate)

# -----------------------------------------------------------------------------------------
# load Stock Database 
# -----------------------------------------------------------------------------------------

# sd <- 
#   get(load(file=paste(advicedir, "/rdata/icesSD 20181023.RData",sep=""))) 
  
# -----------------------------------------------------------------------------------------
# load SAG full data (see: DownloadDataFromSAG.r)
# -----------------------------------------------------------------------------------------

sag <- 
  get(load(file=paste(advicedir, "/rdata/iSAGdownload 20181113.RData",sep=""))) %>% 
  select(-stockkey) %>% 
  left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  left_join(iStockkey, by="stockkey") %>% 
  
  # make numeric
  mutate_at(c("recruitment","lowrecruitment","highrecruitment",  
              "tbiomass","lowtbiomass","hightbiomass", 
              "stocksize", "lowstocksize","highstocksize", 
              "catches", "landings","discards","ibc","unallocatedremovals",
              "fishingpressure", "lowfishingpressure","highfishingpressure",
              "fdiscards","flandings","fibc","funallocated",
              "fpa","bpa", "flim", "blim", "fmsy", "msybtrigger"), 
            funs(as.numeric)) %>% 
  
  # make integer
  mutate_at(c("year", "assessmentyear", "recruitmentage","stockdatabaseid"), funs(as.integer)) %>%
  
  # make logical
  mutate_at(c("published"),  funs(as.logical)) %>% 
  
  # make lowercase
  mutate_at(c("purpose"), funs(tolower)) %>% 
  
  # change -alt for stock assessments to purpose "alternative"
  mutate(purpose       = ifelse(grepl("\\-alt", stockkeylabel), "alternative", purpose), 
         stockkeylabel = ifelse(grepl("\\-alt", stockkeylabel), gsub("\\-alt","",stockkeylabel), stockkeylabel)) %>% 
  
  # remove all data prior to 2001
  # filter(assessmentyear >= 2001) %>% 
  
  mutate(purpose = ifelse(purpose %in% c("initadvice"), "initial advice", purpose)) %>% 
  
  # Deal with Norway pout stockkeylabels and initial advice
  mutate(assessmentdate    = as.Date(NA)) %>% 
  mutate(assessmentdatestr = ifelse(stockkeylabel=="nop-34-oct",paste(as.character(assessmentyear), "1030",sep=""), NA)) %>%
  mutate(assessmentdatestr = ifelse(stockkeylabel=="nop-34-june",paste(as.character(assessmentyear), "0630",sep=""), assessmentdatestr)) %>%
  
  mutate(purpose           = ifelse(grepl("nop-34-jun", stockkeylabel), "initial advice", purpose)) %>% 
  mutate(stockkeylabel     = ifelse(grepl("nop-34", stockkeylabel), "nop-34", stockkeylabel)) %>% 
  mutate(assessmentdate    = ymd(assessmentdatestr)) %>%
  
  # group_by(stockkey, stockkeylabel, assessmentyear, purpose, published, year) %>% 
  # filter(row_number() == 1) %>% 
  ungroup() %>% 
  
  distinct()

save(sag, file=paste(advicedir, "/rdata/iSAG.RData",sep=""))

# glimpse(sag)
# sag %>% distinct(purpose) %>% View()
# sag %>% distinct(published) %>% View()
# sag %>% distinct(purpose, published) %>% View()
# sort(unique(sag$stockkeylabel))

# get(load(file=paste(advicedir, "/rdata/iSAGdownload 20181113.RData",sep=""))) %>% 
#   filter(purpose=="Unofficial") %>% 
#   distinct(stockkey, stockkeylabel, assessmentyear, published) %>% 
#   arrange(assessmentyear, stockkey, stockkeylabel, published) %>% 
#   View()
# 
# get(load(file=paste(advicedir, "/rdata/iSAGdownload 20181112.RData",sep=""))) %>% 
#   filter(stockkey=="136708", assessmentyear == "2016") %>% 
#   arrange(stockkey, stockkeylabel, assessmentyear, published) %>% 
#   View()

# sag %>% filter(grepl("jun", stockkeylabel)) %>% View()
# sag %>% filter(purpose == "alternative") %>% View()

# 
# sag %>% 
#   filter(purpose == "initadvice")  %>%
#   distinct(stockkey, stockkeylabel, assessmentyear, assessmentdate) %>%
#   View()
# 
# sag %>% 
#   filter(stockkeylabel == "had.27.46a20", assessmentyear == 2018) %>% 
#   View()

# -----------------------------------------------------------------------------------------
# load SAG reference points (see: DownloadDataFromSAG.r)
# -----------------------------------------------------------------------------------------

sagrefpoints <- 
  get(load(file=paste(advicedir, "/rdata/iSAGrefpoints 20181113.RData",sep=""))) %>% 
  select(-stockkey, -stockdatabaseid) %>% 
  left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  left_join(iStockkey, by="stockkey") %>% 
  mutate_at(c("flim","fpa","fmsy", "fmanagement", 
              "blim","bpa","msybtrigger", "bmanagement"), 
            funs(as.numeric)) %>% 
  mutate_at(c("assessmentyear", "recruitmentage"), funs(as.integer)) 


save(sagrefpoints, file=paste(advicedir, "/rdata/iSAGrefpoints.RData",sep=""))

# sagrefpoints %>% filter(grepl("her"))

# -----------------------------------------------------------------------------------------
# load the allocation mechanism (needs to be integrated into Advice spreadsheet?)
# -----------------------------------------------------------------------------------------

# iAlloc <-
#   readxl::read_excel(
#     path= paste(advicedir, "/Excel/iAlloc.xlsx", sep=""), 
#     col_names = TRUE, 
#     col_types = "text", 
#     trim_ws   = FALSE) %>%
#   lowcase() %>% 
#   mutate_at(c("stockkey"), funs(as.integer))


# -----------------------------------------------------------------------------------------
# load the STECF Fisheries Management Zones
# -----------------------------------------------------------------------------------------

# load(file=paste(advicedir, "/rdata/STECF sframe.RData",sep="")) 


# -----------------------------------------------------------------------------------------
# Extract unique list of fishstock and assessment years from SAG database and QCSEXCEL database
# -----------------------------------------------------------------------------------------

sag_unique <-
  sag %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, purpose) %>% 
  ungroup()

qcsexcel_unique <-
  qcsexcel %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, purpose) %>% 
  ungroup()

# filter(sag_, grepl("ang-kask", stockkeylabel)) %>% View()
# filter(sag_unique, grepl("nop", stockkeylabel)) %>% View()
# filter(sag_unique, is.na(stockkey)) %>% View()
# filter(sag_unique, !published & purpose=="advice") %>% View()

# sag_unique %>% filter(grepl("whb", stockkeylabel)) %>% View()
# sag_unique %>% filter(stockkey == 169049) %>% View()
# qcsexcel_unique %>% filter(stockkey == 169049) %>% View()

# -----------------------------------------------------------------------------------------
# compare: what is in common; what is only in SAG and what is only in QCSEXCEL?
# -----------------------------------------------------------------------------------------

incommon <-
  inner_join(qcsexcel_unique, sag_unique)

only_in_qcsexcel <-
  anti_join(qcsexcel_unique, sag_unique)

only_in_sag <-
  anti_join(sag_unique, qcsexcel_unique)

# incommon %>% filter(stockkey == 169049) %>% View()
# only_in_qcsexcel %>% filter(stockkey == 169049) %>% View()
# only_in_sag %>% filter(stockkey == 169049) %>% View()

# -----------------------------------------------------------------------------------------
# merge filesets
# -----------------------------------------------------------------------------------------

# sag
t1 <-
  bind_rows(incommon, only_in_sag) %>% 
  left_join(sag, by=c("stockkey","stockkeylabel", "assessmentyear", "purpose")) %>% 
  mutate(source = "sag")

# qcs
t2 <-
  only_in_qcsexcel %>% 
  left_join(qcsexcel, by=c("stockkey","stockkeylabel", "assessmentyear","purpose")) %>% 
  dplyr::select(one_of(names(t1))) %>% 
  mutate(source = "excel")

# generate iAssess
iAssess <-
  bind_rows(t1, t2) 

save(iAssess, file=paste(advicedir, "/rdata/iAssess.RData",sep=""))

