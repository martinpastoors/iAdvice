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
# 20/11/2018 small updates; making sure the database is consistently filled
# 26/11/2018 added comparison to the data used by Esther Schuch
# -----------------------------------------------------------------------------------------------

# rm(list=ls())

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
    path= paste(advicedir, "/Excel/ICES Scientific Advice database.xlsm", sep=""), 
    sheet     = "DATA",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() %>% 
  dplyr::rename(purpose = assessmentpurpose) %>% 
  
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
  mutate_at(c("purpose", "speciescommonname", "stockkeylabel","stockkeylabelold","stockkeylabelnew",
              "stocksizeunits","fishingpressureunits"), 
            funs(tolower)) %>% 
  mutate_at(c("benchmark", "published"), funs(as.logical)) %>% 
  
  mutate(
    adviceonstock = ifelse(toupper(adviceonstock) == "Y", TRUE, adviceonstock),
    adviceonstock = ifelse(toupper(adviceonstock) == "N", FALSE, adviceonstock),
    adviceonstock = as.logical(adviceonstock)
  )
  
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

# glimpse(iSpecies)

# -----------------------------------------------------------------------------------------
# Generate iRename, iStockkey
# -----------------------------------------------------------------------------------------

# generate iRename (linking stockkeylabel to stockkey)
iRename <-
  iAdvice %>% 
  group_by(stockkey, stockkeylabel) %>% 
  filter(row_number() ==1 ) %>% 
  ungroup() %>% 
  dplyr::select(stockkeylabel, stockkey) 

# iRename %>% filter(grepl("had.27", stockkeylabel)) %>% View()
# iRename %>% filter(grepl("had.27.7b", stockkeylabel)) %>% View()
# iRename %>% filter(grepl("smn-grl", stockkeylabel)) %>% View()

iStockkey <-
  iAdvice %>% 
  group_by(stockkey) %>% 
  # filter(!is.na(stocklongname)) %>% 
  filter(assessmentyear == max(assessmentyear) ) %>% 
  ungroup() %>% 
  distinct(stockkey, stockkeylabelnew, stockkeylabelold, speciesfaocode) 

# iStockkey %>% filter(grepl("had.27.7b", stockkeylabelnew)) %>% View()

save(iRename,   file=paste(advicedir, "/rdata/iRename.RData",sep=""))
save(iStockkey, file=paste(advicedir, "/rdata/iStockkey.RData",sep=""))

# write.csv(iRename, file=paste(advicedir, "/excel/iRename.csv",sep=""), row.names = FALSE)
# write.csv(iStockkey, file=paste(advicedir, "/excel/iStockkey.csv",sep=""), row.names = FALSE)

# -----------------------------------------------------------------------------------------
# load Excel and QSC data
# -----------------------------------------------------------------------------------------

qcsexcel <-
  readxl::read_excel(
    path= paste(advicedir, "/excel/QCS and EXCEL Assessment Database combined.xlsm", sep=""), 
    sheet     = "data",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() %>% 
  
  # remove nephrops and rays for now
  filter(tolower(substr(stockkeylabel,1,3)) != "nep") %>% 
  filter(tolower(substr(stockkeylabel,1,3)) != "raj") %>% 
  filter(tolower(substr(stockkeylabel,1,2)) != "rj") %>% 
  
  # make numeric
  mutate_at(c("recruitment","lowrecruitment","highrecruitment",  
              "tbiomass","lowtbiomass","hightbiomass", 
              "stocksize", "lowstocksize","highstocksize", 
              "catches", "landings","discards","ibc","unallocatedremovals",
              "fishingpressure", "lowfishingpressure","highfishingpressure",
              "fdiscards","flandings","fibc","funallocated"), 
            funs(as.numeric)) %>% 
  
  # make integer
  mutate_at(c("stockkey", "assessmentyear", "year", "recruitmentage"),    funs(as.integer)) %>% 
  
  # make lowercase
  mutate_at(c("recruitmentdescription", "unitofrecruitment",  
              "stocksizedescription", "stocksizeunits", 
              "catcheslandingsunits",
              "fishingpressuredescription", "fishingpressureunits",
              "purpose"),    funs(tolower)) %>% 


  # distinct rows only
  distinct() %>% 
  
  # remove date
  select(-assessmentdate)  %>% 
  
  # make logical
  mutate_at(c("published"),  funs(as.logical)) 
  
save(qcsexcel, file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))


# -----------------------------------------------------------------------------------------
# load Stock Database 
# -----------------------------------------------------------------------------------------

sd <-
  get(load(file=paste(advicedir, "/rdata/icesSD 20181023.RData",sep=""))) %>% 
  lowcase() %>% 
  dplyr::select(-stockkey) %>% 
  # dplyr::select(-stockkey, -previousstockkey, -previousstockkeylabel, -stockdatabaseid) %>% 
  left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  left_join(iStockkey, by="stockkey") %>% 
  rename(assessmentyear = activeyear) 


# -----------------------------------------------------------------------------------------
# load SAG full data (see: DownloadDataFromSAG.r)
# -----------------------------------------------------------------------------------------

sag <- 
  get(load(file=paste(advicedir, "/rdata/iSAGdownload 20181113.RData",sep=""))) %>% 
  
  rename(catcheslandingsunits = catchesladingsunits) %>% 
  
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
  mutate_at(c("purpose", "stockkeylabel", "unitofrecruitment", "stocksizeunits", "stocksizedescription",
              "catcheslandingsunits", "fishingpressureunits"), 
            funs(tolower)) %>% 
  
  # change -alt for stock assessments to purpose "alternative"
  mutate(purpose       = ifelse(grepl("\\-alt", stockkeylabel), "alternative", purpose), 
         stockkeylabel = ifelse(grepl("\\-alt", stockkeylabel), gsub("\\-alt","",stockkeylabel), stockkeylabel)) %>% 
  
  # remove nephrops and rays for now
  filter(tolower(substr(stockkeylabel,1,3)) != "nep") %>% 
  filter(tolower(substr(stockkeylabel,1,3)) != "raj") %>% 
  filter(tolower(substr(stockkeylabel,1,2)) != "rj") %>% 
  
  # remove all data prior to 2013
  filter(assessmentyear >= 2013) %>% 
  
  # remove specific assessments
  # filter(!(stockkeylabel == "whb.27.1-91214" & assessmentyear == 2010)) %>%   # This one is double with the whb-comb assessment 

  mutate(purpose = ifelse(purpose %in% c("initadvice"), "initial advice", purpose)) %>% 
  
  # Deal with Norway pout stockkeylabels and initial advice
  mutate(purpose           = ifelse(grepl("nop-34-jun", stockkeylabel), "initial advice", purpose)) %>% 
  mutate(stockkeylabel     = ifelse(grepl("nop-34-jun", stockkeylabel), "nop-34", stockkeylabel)) %>% 
  
  mutate(purpose           = ifelse(grepl("nop-34-oct", stockkeylabel), "advice", purpose)) %>% 
  mutate(stockkeylabel     = ifelse(grepl("nop-34-oct", stockkeylabel), "nop-34", stockkeylabel)) %>% 

  # group_by(stockkey, stockkeylabel, assessmentyear, purpose, published, year) %>% 
  # filter(row_number() == 1) %>% 
  ungroup() %>% 
  
  # Deal with plaice in the North Sea (assessments prior to 2015 had different area allocation)
  mutate(
    stockkey         = ifelse(stockkeylabel == "ple-nsea" & assessmentyear %in% 2013:2014, 100103, stockkey),
    stockkeylabel    = ifelse(stockkeylabel == "ple-nsea" & assessmentyear %in% 2013:2014, "ple-nsea2", stockkeylabel)
  ) %>% 
  
  # Only keep distinct rows
  distinct() %>% 
  
  # now do the stockkey transformations
  dplyr::select(-stockkey, -icesareas) %>% 
  left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  left_join(iStockkey, by="stockkey") %>% 
  
  # make logical
  mutate_at(c("published"),  funs(as.logical)) %>% 
  
  # remove duplicate assessments (Careful!)
  group_by(stockkey, stockkeylabel, assessmentyear, purpose, year) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  
  # remove all the custom fiels (for now)
  select(-starts_with('custom'))

save(sag, file=paste(advicedir, "/rdata/iSAG.RData",sep=""))


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
# Extract unique list of fishstock and assessment years from SAG database and QCSEXCEL database
# -----------------------------------------------------------------------------------------

sag_unique <-
  sag %>% 
  distinct(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, purpose) %>% 
  ungroup()

qcsexcel_unique <-
  qcsexcel %>% 
  distinct(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, purpose) %>% 
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
sag_to_merge <-
  bind_rows(incommon, only_in_sag) %>% 
  left_join(sag, by=c("stockkey","stockkeylabel", "stockkeylabelold", "stockkeylabelnew", "assessmentyear", "purpose")) %>% 
  mutate(source = "sag")

# qcs
qcsexcel_to_merge <-
  only_in_qcsexcel %>% 
  left_join(qcsexcel, by=c("stockkey","stockkeylabel", "stockkeylabelold", "stockkeylabelnew", "assessmentyear","purpose")) %>% 
  dplyr::select(one_of(names(sag_to_merge))) %>% 
  mutate(source = tolower(source))

# iAdvice (only model specifications)
iadvice_to_merge <-
  iAdvice %>% 
  dplyr::select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, purpose, 
                stockarea, assessmentmodel, benchmark, assessmentscale, nsurveyseries, ncpueseries, 
                adviceonstock, published) %>% 
  mutate_at(c("assessmentmodel"), funs(tolower))


# generate iAssess
iAssess <-
  
  bind_rows(sag_to_merge, qcsexcel_to_merge) %>% 
  
  # add information from iAdvice and add source if not already existing
  full_join(iadvice_to_merge, by=c("stockkey", "stockkeylabel", "stockkeylabelold", 
                     "stockkeylabelnew", "assessmentyear","purpose")) %>% 
  mutate(source = ifelse(is.na(source), "iadvice", source)) %>% 
  
  # descriptions and units to lowercase
  mutate_at(c("unitofrecruitment", "recruitmentdescription",
              "stocksizeunits", "stocksizedescription",
              "fishingpressureunits", "fishingpressuredescription",
              "catcheslandingsunits"), funs(tolower(str_trim(., side="both")))) %>% 
  mutate_at(c("unitofrecruitment", "recruitmentdescription",
              "stocksizeunits", "stocksizedescription",
              "fishingpressureunits", "fishingpressuredescription",
              "catcheslandingsunits"), funs(str_trim(.))) %>% 
  
  # do unit conversions
  mutate(
    
    # recruitment
    recruitment       = ifelse(unitofrecruitment == "millions", 1000*recruitment, recruitment),
    lowrecruitment    = ifelse(unitofrecruitment == "millions", 1000*lowrecruitment, lowrecruitment),
    highrecruitment   = ifelse(unitofrecruitment == "millions", 1000*highrecruitment, highrecruitment),
    unitofrecruitment = ifelse(unitofrecruitment == "millions", "thousands", unitofrecruitment),
    
    unitofrecruitment = ifelse(stockkeylabelold == "had-iris" & assessmentyear == 2016, "thousands", unitofrecruitment),
    unitofrecruitment = gsub("no/","n/", unitofrecruitment),
    unitofrecruitment = ifelse(unitofrecruitment == "select units", NA, unitofrecruitment),
    
    recruitmentdescription = ifelse(recruitmentdescription == "select recruitment type", NA, recruitmentdescription),
    
    # stock size
    stocksize       = ifelse(stocksizeunits == "thousand tonnes", 1000*stocksize, stocksize),
    lowstocksize    = ifelse(stocksizeunits == "thousand tonnes", 1000*lowstocksize, lowstocksize),
    highstocksize   = ifelse(stocksizeunits == "thousand tonnes", 1000*highstocksize, highstocksize),
    tbiomass        = ifelse(stocksizeunits == "thousand tonnes", 1000*tbiomass, tbiomass),
    lowtbiomass     = ifelse(stocksizeunits == "thousand tonnes", 1000*lowtbiomass, lowtbiomass),
    hightbiomass    = ifelse(stocksizeunits == "thousand tonnes", 1000*hightbiomass, hightbiomass),
    stocksizeunits  = ifelse(stocksizeunits == "thousand tonnes", "tonnes", stocksizeunits),
    
    # stocksizeunits
    stocksizeunits  = ifelse(stocksizeunits == "cpue (kg/1000 hooks)", "kg/1000 hooks", stocksizeunits),
    stocksizeunits  = ifelse(stocksizeunits == "n/hr", "n/hour", stocksizeunits),
    stocksizeunits  = ifelse(stocksizeunits == "kg/h", "kg/hour", stocksizeunits),
    stocksizeunits  = ifelse(stocksizeunits == "select units", NA, stocksizeunits),
    
    # stocksizedescription
    stocksizedescription = gsub("stock size: |stock size index: ","",stocksizedescription),
    stocksizedescription = ifelse(stocksizedescription == "select stock size description", NA, stocksizedescription),
    
    # catcheslandingsunits
    catcheslandingsunits = ifelse(catcheslandingsunits == "t", "tonnes", catcheslandingsunits), 
    
    # fishing pressure descriptions
    fishingpressuredescription = gsub("fishing pressure: ","",fishingpressuredescription),
    fishingpressuredescription = gsub(" in winter rings","", fishingpressuredescription),
    fishingpressuredescription = ifelse(fishingpressuredescription == "select fishing pressure description", NA, fishingpressuredescription),
    fage                       = ifelse(grepl("[0-9]{1,2}-[0-9]{1,2}", fishingpressuredescription), 
             str_extract(fishingpressuredescription, "[0-9]{1,2}-[0-9]{1,2}"), fage),
    fishingpressuredescription = ifelse(grepl("[0-9]{1,2}-[0-9]{1,2}", fishingpressuredescription), 
             str_replace(fishingpressuredescription, "[0-9]{1,2}-[0-9]{1,2}", ""), fishingpressuredescription), 
    fishingpressuredescription = gsub("\\(ages \\)|bar\\(\\)|mean |weighted ","", fishingpressuredescription),
    fishingpressuredescription = gsub("harvest rate", "hr", fishingpressuredescription),
    
    # specific cases for cod 5a in 2016 and had 5a in 2018 (description is F; harvest rate in custom 2)
    fishingpressuredescription = ifelse(stockkeylabel == "cod-iceg"   & assessmentyear == 2016, "f", fishingpressuredescription),
    fishingpressuredescription = ifelse(stockkeylabel == "had.27.5a"  & assessmentyear == 2018, "f", fishingpressuredescription),
    fishingpressuredescription = ifelse(is.na(fishingpressure) & !is.na(fishingpressuredescription), NA, fishingpressuredescription),
    fishingpressuredescription = str_trim(fishingpressuredescription),
      
    # fishingpressureunits
    fishingpressureunits       = ifelse(is.na(fishingpressure) & !is.na(fishingpressureunits), NA, fishingpressureunits),
    fishingpressureunits       = ifelse(is.na(fishingpressureunits) & fishingpressuredescription == "f", "year-1",fishingpressureunits),
    fishingpressureunits       = ifelse(fishingpressureunits == "per year", "year-1", fishingpressureunits)
  ) %>% 
  
  # Handle published and assessmentscale
  mutate(published = ifelse(is.na(published.x), published.y, published.x)) %>% 
  select(-published.x, -published.y) %>% 
  
  # sorting
  arrange(speciesfaocode, stockkey, assessmentyear, purpose)

save(iAssess, file=paste(advicedir, "/rdata/iAssess.RData",sep=""))

