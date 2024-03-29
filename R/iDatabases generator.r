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
# 20/03/2019 adapted with new data during HAWG
# 23/03/2019 adapated after change to download data procedure
# 06/05/2019 dplyr update. Changed "funs" to "list" in mutate_at functions 
# 15/05/2019 removed double assessments from SAG
# 15/08/2019 removed doubles arising from iAdvice and SAG coupling on purpose
# 04/09/2019 added manual allocation of hom-nsea 2019 assessment
# 04/09/2019 fixed strange error: mutate with multiple ifelse gave logical; now each line a separate mutate with ifelse works well
# 09/06/2020 updated version of iadvice tables (more details on stf)
# -----------------------------------------------------------------------------------------------

# rm(list=ls())

library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)      # reshaping data; e.g. dcast
library(pander)        # for print tables
library(readxl)        # read excel files
library(lubridate)     # dates
library(cowplot)       # multiplots
library(directlabels)  # for printing labels at end of geom lines

library(icesSD)        # devtools::install_github("ices-tools-prod/icesSD")

# Load utils code
source("../prf/r/my utils.r")

# Set working directory to dropbox folder
dropboxdir  <- paste(get_dropbox(), "/iAdvice", sep="")

# -----------------------------------------------------------------------------------------
# load the Advice database
# -----------------------------------------------------------------------------------------

iAdvice <-
  readxl::read_excel(
    path= paste(dropboxdir, "/Excel/ICES Scientific Advice database.xlsm", sep=""), 
    sheet     = "DATA",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() %>% 
  dplyr::rename(purpose = assessmentpurpose) %>% 
  
  mutate_at(c("advisedlandingsmin", "advisedcatchmin", 
              "advisedlandingsmax", "advisedcatchmax",
              "tal", "tac", "unilateralquota",
              "officiallandings", "landings", "ibc", "discards", "catches", 
              "fsqimy", "ssbimy", "fadvmax",
              "catchrealized","catch1fcy","catch2fcy","f1fcy","f2fcy","ssb1fcy","ssb2fcy","fset","ssbset",
              "fmax", "f01", "fmed", "f35spr", "flim", "fpa", "fmsy",
              "blim", "bpa", "msybtrigger",
              "m1", "m5"),
            list(as.numeric)) %>%
  mutate_at(c("tacyear", "assessmentyear", "stockkey", "firstyearofdata", "ncpueseries","nsurveyseries"),
            list(as.integer)) %>% 
  mutate_at(c("purpose", "speciescommonname", "stockkeylabel","stockkeylabelold","stockkeylabelnew",
              "stocksizeunits","fishingpressureunits"), 
            list(tolower)) %>% 
  mutate_at(c("benchmark", "published"), list(as.logical)) %>% 
  
  mutate(
    adviceonstock = ifelse(toupper(adviceonstock) == "Y", TRUE, adviceonstock),
    adviceonstock = ifelse(toupper(adviceonstock) == "N", FALSE, adviceonstock),
    adviceonstock = as.logical(adviceonstock)
  )
  
save(iAdvice, file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))

# skimr::skim(iAdvice)
# count_not_finite(iAdvice)
# count_zeroes(iAdvice)
# inspectdf::inspect_num(iAdvice) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(iAdvice) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(iAdvice) %>% inspectdf::show_plot()

# iAdvice %>% group_by(tacyear, adviceonstock) %>% summarise(n=n()) %>% 
#   ggplot(aes(x=tacyear, y=n, group=tacyear)) + geom_bar(aes(fill=adviceonstock), stat="identity")

# iAdvice %>% group_by(tacyear) %>% 
#   summarise(
#     nlandingsadvice=sum(!is.na(advisedlandingsmax)),
#     ncatchadvice=sum(!is.na(advisedcatchmax)),
#   ) %>% 
#   pivot_longer(names_to = "variable", values_to = "n", nlandingsadvice:ncatchadvice) %>% 
#   ggplot(aes(x=tacyear, y=n, group=tacyear)) + geom_bar(aes(fill=variable), stat="identity") + facet_wrap(~variable)

# glimpse(iAdvice)
# iAdvice %>% filter(speciescommonname=="salmon") %>% group_by(stockkeylabel, stockkey, assessmentyear) %>% summarise(n=n()) %>% 
#   pivot_wider(names_from = assessmentyear, values_from = n) %>% View()

# ---------------------------------------------------------------------------------------
# Read iSpecies from excel (not from ICES SAG because more information added to excel version). 
# -----------------------------------------------------------------------------------------

iSpecies <-
  readxl::read_excel(
    path=paste(dropboxdir, "/excel/species_list.xlsx",sep=""), 
    col_names=TRUE, 
    col_types="text") %>%
  mutate_at(c("speciescommonname","trophicguild","fisheriesguild","sizeguild"), 
            list(tolower)) %>%
  group_by(speciesfaocode, speciesscientificname, speciescommonname) %>%
  arrange(speciesfaocode) 

save(iSpecies, file=paste(dropboxdir, "/rdata/iSpecies.RData",sep=""))

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

# iRename %>% filter(grepl("sal", stockkeylabel)) %>% View()
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

# save(iRename,   file=paste(dropboxdir, "/rdata/iRename.RData",sep=""))
# save(iStockkey, file=paste(dropboxdir, "/rdata/iStockkey.RData",sep=""))

# write.csv(iRename, file=paste(dropboxdir, "/excel/iRename.csv",sep=""), row.names = FALSE)
# write.csv(iStockkey, file=paste(dropboxdir, "/excel/iStockkey.csv",sep=""), row.names = FALSE)

# load(file=paste(dropboxdir, "/rdata/iRename.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/iStockkey.RData",sep=""))

# -----------------------------------------------------------------------------------------
# load Excel and QSC data
# -----------------------------------------------------------------------------------------

qcsexcel <-
  readxl::read_excel(
    path= paste(dropboxdir, "/excel/QCS and EXCEL Assessment Database combined.xlsm", sep=""), 
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
            list(as.numeric)) %>% 
  
  # make integer
  mutate_at(c("stockkey", "assessmentyear", "year", "recruitmentage"),    list(as.integer)) %>% 
  
  # make lowercase
  mutate_at(c("recruitmentdescription", "unitofrecruitment",  
              "stocksizedescription", "stocksizeunits", 
              "catcheslandingsunits",
              "fishingpressuredescription", "fishingpressureunits",
              "purpose"),    list(tolower)) %>% 


  # distinct rows only
  distinct() %>% 
  
  # remove date
  select(-assessmentdate)  %>% 
  
  # make logical
  mutate_at(c("published"),  list(as.logical)) 
  
save(qcsexcel, file=paste(dropboxdir, "/rdata/qcsexcel.RData",sep=""))


# -----------------------------------------------------------------------------------------
# load Stock Database 
# -----------------------------------------------------------------------------------------

sd <-
  sd <- icesSD::getSD() %>%
  # loadRData(file=paste(dropboxdir, "/rdata/icesSD 20181023.RData",sep="")) %>%
  lowcase() %>%
  drop_na(stockkey) %>%
  dplyr::select(-stockkey) %>%
  distinct() %>%
  # dplyr::select(-stockkey, -previousstockkey, -previousstockkeylabel, -stockdatabaseid) %>%
  left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  left_join(iStockkey, by="stockkey") %>%
  rename(assessmentyear = activeyear)


# -----------------------------------------------------------------------------------------
# load SAG full data (see: DownloadDataFromSAG.r)
# -----------------------------------------------------------------------------------------
# sag %>% filter(assessmentyear == 2019, stockkeylabelold=="hom-nsea") %>% View()

sag <- 
  get(load(file=paste(dropboxdir, "/rdata/iSAGstock.RData",sep=""))) %>% 
  
  # dplyr::select(-stockkey) %>% 
  # left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  # left_join(iStockkey, by="stockkey") %>% 
  
  # sag %>% filter(stockkeylabelold == "hom-nsea" & assessmentyear == 2019) %>% View()
  
  # !!!! manual allocation of hom-nsea assessment (from custom to stocksize)
  # mutate() %>% 
  #   stocksize = ifelse(stockkeylabelold == "hom-nsea" & assessmentyear == 2019, )
  
# remove custom fields for now
  dplyr::select(-starts_with("custom")) %>% 
  
  # remove assessmentkey (prevents appropriate merging)
  dplyr::select(-assessmentkey) %>% 
  
  # remove all data prior to 2013
  filter(assessmentyear >= 2013) 

# save(sag, file=paste(dropboxdir, "/rdata/iSAG.RData",sep=""))


# -----------------------------------------------------------------------------------------
# load SAG reference points (see: DownloadDataFromSAG.r)
# -----------------------------------------------------------------------------------------

# DOES THIS INCLUDE REF POINTS FOR NON-ADVICE PURPOSES?

sagrefpoints <- 
  get(load(file=paste(dropboxdir, "/rdata/iSAGrefpoints.RData",sep=""))) %>% 
  select(-stockkey, -assessmentkey) %>% 
  # left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  # left_join(iStockkey, by="stockkey") %>% 
  mutate_at(c("flim","fpa","fmsy", "fmanagement", 
              "blim","bpa","msybtrigger", "bmanagement"), 
            list(as.numeric)) %>% 
  mutate_at(c("assessmentyear", "recruitmentage"), list(as.integer)) 


# save(sagrefpoints, file=paste(dropboxdir, "/rdata/iSAGrefpoints.RData",sep=""))

# sagrefpoints %>% filter(grepl("her"))
# sagrefpoints %>% group_by(stockkeylabel, stockkey)


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
# incommon %>% filter(stockkeylabelold == "hom-nsea) %>% View()
# only_in_qcsexcel %>% filter(stockkey == 169049) %>% View()
# only_in_sag %>% filter(stockkey == 169049) %>% View()
# only_in_sag %>% filter(stockkeylabelold == "hom-nsea") %>% View()

# -----------------------------------------------------------------------------------------
# merge filesets
# -----------------------------------------------------------------------------------------

# sag
sag_to_merge <-
  bind_rows(incommon, only_in_sag) %>% 
  left_join(sag, by=c("stockkey","stockkeylabel", "stockkeylabelold", "stockkeylabelnew", "assessmentyear", "purpose")) %>% 
  mutate(source = "sag")

# unique(sag_to_merge$source)

# qcs
qcsexcel_to_merge <-
  only_in_qcsexcel %>% 
  left_join(qcsexcel, by=c("stockkey","stockkeylabel", "stockkeylabelold", "stockkeylabelnew", "assessmentyear","purpose")) %>% 
  dplyr::select(one_of(names(sag_to_merge))) %>% 
  mutate(source = tolower(source))

# unique(qcsexcel_to_merge$source)


# iAdvice (only model specifications)
iadvice_to_merge <-
  iAdvice %>% 
  dplyr::select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, assessmentdate,
                purpose,  stockarea, assessmentmodel, benchmark, assessmentscale, nsurveyseries, ncpueseries, 
                adviceonstock, published) %>% 
  mutate_at(c("assessmentmodel"), list(tolower)) %>% 
  ungroup()


# iAssess %>% filter(stockkeylabelold == "mac-nea", assessmentyear == 2016, purpose=="replaced") %>% View()
# iadvice_to_merge %>% filter(stockkeylabelold == "mac-nea", assessmentyear == 2016, purpose=="replaced") %>% View()
# t1 <- iadvice_to_merge %>% filter(stockkeylabelold == "mac-nea", assessmentyear == 2016, purpose=="replaced") %>% 
#   dplyr::select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, purpose, assessmentmodel)
# t2 <- bind_rows(sag_to_merge, qcsexcel_to_merge) %>% 
#   filter(stockkeylabelold == "mac-nea", assessmentyear == 2016, purpose=="replaced", year == 2015) %>% 
#   dplyr::select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, purpose, source)
# full_join(t1, t2, by=c("stockkey", "stockkeylabel", "stockkeylabelold","stockkeylabelnew", "assessmentyear","purpose")) %>%
#   View()


# generate iAssess
iAssess <-
# t <-
  bind_rows(sag_to_merge, qcsexcel_to_merge) %>% 
  ungroup() %>% 
  
  # add information from iAdvice and add source if not already existing
  # left_join(iadvice_to_merge, by=c("stockkey", "stockkeylabel", "stockkeylabelold","stockkeylabelnew", "assessmentyear","purpose")) %>% 
  full_join(iadvice_to_merge, by=c("stockkey", "stockkeylabel", "stockkeylabelold","stockkeylabelnew", "assessmentyear","purpose")) %>%
  mutate(source = ifelse(is.na(source), "iadvice", source)) %>%
  
  # descriptions and units to lowercase
  mutate_at(c("unitofrecruitment", "recruitmentdescription",
              "stocksizeunits", "stocksizedescription",
              "fishingpressureunits", "fishingpressuredescription",
              "catcheslandingsunits"), list(stringr::str_trim)) %>% 
  mutate_at(c("unitofrecruitment", "recruitmentdescription",
              "stocksizeunits", "stocksizedescription",
              "fishingpressureunits", "fishingpressuredescription",
              "catcheslandingsunits"), list(tolower)) %>% 
  
  # filter(stockkeylabelold=="hom-nsea" & assessmentyear == 2019) %>% 
  
  #stocksize
  mutate(stocksizeunits  = ifelse(is.na(stocksizeunits) ,"unknown", stocksizeunits)) %>% 
  mutate(stocksize       = ifelse(stocksizeunits == "thousand tonnes", 1000*stocksize, 1*stocksize) ) %>% 
  mutate(lowstocksize    = ifelse(stocksizeunits == "thousand tonnes", 1000*lowstocksize, lowstocksize)) %>% 
  mutate(highstocksize   = ifelse(stocksizeunits == "thousand tonnes", 1000*highstocksize, highstocksize)) %>% 
  mutate(tbiomass        = ifelse(stocksizeunits == "thousand tonnes", 1000*tbiomass, tbiomass)) %>% 
  mutate(lowtbiomass     = ifelse(stocksizeunits == "thousand tonnes", 1000*lowtbiomass, lowtbiomass)) %>% 
  mutate(hightbiomass    = ifelse(stocksizeunits == "thousand tonnes", 1000*hightbiomass, hightbiomass) ) %>% 
  mutate(stocksizeunits  = ifelse(stocksizeunits == "thousand tonnes", "tonnes", stocksizeunits)) %>% 
  
  mutate(stocksizeunits  = ifelse(stocksizeunits == "cpue (kg/1000 hooks)", "kg/1000 hooks", stocksizeunits)) %>% 
  mutate(stocksizeunits  = ifelse(stocksizeunits == "n/hr", "n/hour", stocksizeunits)) %>% 
  mutate(stocksizeunits  = ifelse(stocksizeunits == "kg/h", "kg/hour", stocksizeunits)) %>% 
  mutate(stocksizeunits  = ifelse(stocksizeunits == "select units", NA, stocksizeunits)) %>% 

  mutate(stocksizedescription = gsub("stock size: |stock size index: ","",stocksizedescription)) %>% 
  mutate(stocksizedescription = ifelse(stocksizedescription == "select stock size description", NA, stocksizedescription) ) %>% 
  
  # recruitment
  mutate(unitofrecruitment  = ifelse(is.na(unitofrecruitment) ,"unknown", unitofrecruitment)) %>% 
  mutate(recruitment       = ifelse(unitofrecruitment == "millions", 1000*recruitment, recruitment)) %>% 
  mutate(lowrecruitment    = ifelse(unitofrecruitment == "millions", 1000*lowrecruitment, lowrecruitment)) %>% 
  mutate(highrecruitment   = ifelse(unitofrecruitment == "millions", 1000*highrecruitment, highrecruitment)) %>% 
  mutate(unitofrecruitment = ifelse(unitofrecruitment == "millions", "thousands", unitofrecruitment)) %>% 
  mutate(recruitment       = ifelse(unitofrecruitment == "millions", 1000*recruitment, recruitment)) %>% 
        
  mutate(unitofrecruitment = ifelse(stockkeylabelold == "had-iris" & assessmentyear == 2016, "thousands", unitofrecruitment)) %>% 
  mutate(unitofrecruitment = gsub("no/","n/", unitofrecruitment)) %>% 
  mutate(unitofrecruitment = ifelse(unitofrecruitment == "select units", NA, unitofrecruitment)) %>% 
  
  mutate(recruitmentdescription = ifelse(recruitmentdescription == "select recruitment type", NA, recruitmentdescription) ) %>% 
  
  # catcheslandingsunits
  mutate(catcheslandingsunits = ifelse(is.na(catcheslandingsunits) ,"unknown", catcheslandingsunits)) %>% 
  mutate(catcheslandingsunits = ifelse(catcheslandingsunits == "t", "tonnes", catcheslandingsunits)) %>% 

  # fishing pressure 
  mutate(fishingpressureunits  = ifelse(is.na(fishingpressureunits) ,"unknown", fishingpressureunits)) %>% 
  
  mutate(fishingpressuredescription = gsub("fishing pressure: ","",fishingpressuredescription)) %>% 
  mutate(fishingpressuredescription = gsub(" in winter rings","", fishingpressuredescription)) %>% 
  mutate(fishingpressuredescription = ifelse(fishingpressuredescription == "select fishing pressure description", NA, fishingpressuredescription)) %>% 
  mutate(fage                       = ifelse(grepl("[0-9]{1,2}-[0-9]{1,2}", fishingpressuredescription),
                                             str_extract(fishingpressuredescription, "[0-9]{1,2}-[0-9]{1,2}"),
                                             fage) ) %>% 
  mutate(fishingpressuredescription = ifelse(grepl("[0-9]{1,2}-[0-9]{1,2}", fishingpressuredescription),
                                             str_replace(fishingpressuredescription, "[0-9]{1,2}-[0-9]{1,2}", ""),
                                             fishingpressuredescription)) %>% 
  mutate(fishingpressuredescription = gsub("\\(ages \\)|bar\\(\\)|mean |weighted ","", fishingpressuredescription)) %>% 
  mutate(fishingpressuredescription = gsub("harvest rate", "hr", fishingpressuredescription)) %>% 

                        
  # specific cases for cod 5a in 2016 and had 5a in 2018 (description is F; harvest rate in custom 2)
  mutate(fishingpressuredescription = 
           ifelse(stockkeylabel == "cod-iceg"   & assessmentyear == 2016, "f", fishingpressuredescription)) %>% 
  mutate(fishingpressuredescription = 
           ifelse(stockkeylabel == "had.27.5a"  & assessmentyear == 2018, "f", fishingpressuredescription)) %>% 
  mutate(fishingpressuredescription = 
           ifelse(is.na(fishingpressure) & !is.na(fishingpressuredescription), NA, fishingpressuredescription)) %>% 
  mutate(fishingpressuredescription = str_trim(fishingpressuredescription)) %>% 
                            
  # fishingpressureunits
  mutate(fishingpressureunits       = ifelse(is.na(fishingpressure) & !is.na(fishingpressureunits), NA, fishingpressureunits)) %>% 
  mutate(fishingpressureunits       = ifelse(is.na(fishingpressureunits) & fishingpressuredescription == "f", "year-1",fishingpressureunits)) %>% 
  mutate(fishingpressureunits       = ifelse(fishingpressureunits == "per year", "year-1", fishingpressureunits)) %>% 
                            
  # purpose
  mutate(purpose                    = ifelse(purpose == "initadvice", "initial advice", purpose)) %>% 
  mutate(purpose                    = ifelse(purpose == "trends"    , "trends only", purpose)) %>% 
  mutate(purpose                    = ifelse(purpose == "forecast"  , "advice", purpose) ) %>% 
  
  # remove historical (only used once)
  filter(purpose != "historical") %>% 
  filter(purpose != "no assessment") %>% 
  
  # Handle published and assessmentscale
  mutate(published = ifelse(is.na(published.x), published.y, published.x)) %>% 
  select(-published.x, -published.y) %>% 
  
  # sorting
  arrange(speciesfaocode, stockkey, assessmentyear, purpose) %>% 
  
  ungroup() %>% 
  distinct()

save(iAssess, file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))

# skimr::skim(iAssess)
# count_not_finite(iAssess)
# count_zeroes(iAssess)
# inspectdf::inspect_num(iAssess) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(iAssess) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(iAssess) %>% inspectdf::show_plot()

# iAssess %>% filter(stockkeylabelold=="hom-nsea" & assessmentyear == 2019) %>% View()
# glimpse(iAssess)

# unique(iAssess$source)

# unique(iAssess$purpose)
# filter(iAssess, purpose=="historical") %>% View()

# create overviews
# t1 <-
#   sag_unique %>% 
#   ungroup() %>% 
#   mutate(purpose = toupper(substr(purpose,1,1))) %>% 
#   group_by(stockkey, stockkeylabel, stockkeylabelnew, stockkeylabelold, assessmentyear) %>% 
#   arrange(stockkey, stockkeylabel, stockkeylabelnew, stockkeylabelold, assessmentyear, purpose) %>% 
#   summarize(purpose= paste(purpose, collapse="")) 

# t2 <-
#   qcsexcel_unique %>% 
#   ungroup() %>% 
#   mutate(purpose = ifelse(grepl("bench",purpose), "bench", purpose)) %>% 
#   mutate(purpose = ifelse(grepl("exploratory|trendsonly",purpose), "unofficial", purpose)) %>% 
#   mutate(purpose = tolower(substr(purpose,1,1))) %>% 
#   group_by(stockkey, stockkeylabel, stockkeylabelnew, stockkeylabelold, assessmentyear) %>% 
#   arrange(stockkey, stockkeylabel, stockkeylabelnew, stockkeylabelold, assessmentyear, purpose) %>% 
#   summarize(purpose= paste(purpose, collapse="")) 

# bind_rows(t1,t2) %>% 
#   filter(!is.na(stockkey)) %>% 
#   group_by(stockkeylabelold, stockkeylabelnew, stockkey, assessmentyear) %>% 
#   arrange(stockkeylabelold, stockkeylabelnew, stockkey, assessmentyear, purpose) %>% 
#   summarize(purpose= paste(purpose, collapse="")) %>% 
#   spread(key=assessmentyear, value=purpose) %>% 
#   write.csv(., file=paste0("iDatabases overview ",today, ".csv"), row.names=FALSE)

# iAssess %>% 
#   filter(assessmentyear==2020) %>% 
#   ungroup() %>% 
#   distinct(stockkeylabel) %>% 
#   View()

# iAssess %>% filter(assessmentyear==2010, stockkeylabelold=="hke-nrtn") %>% View()
# qcsexcel_unique %>% filter(stockkeylabelold=="hke-nrtn") %>% View()

  