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

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files

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
  mutate_at(c("assessmenttype", "speciescommonname"), 
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
  dplyr::select(stockkey, stockkeylabelnew, stockkeylabelold, stockarea)
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
  
  mutate_at(c("recruitment","lowrecruitment","highrecruitment",  
              "tbiomass","lowtbiomass","hightbiomass", 
              "stocksize", "lowstocksize","highstocksize", 
              "catches", "landings","discards","ibc","unallocatedremovals",
              "fishingpressure", "lowfishingpressure","highfishingpressure",
              "fdiscards","flandings","fibc","funallocated"), 
            funs(as.numeric)) %>% 
  mutate_at(c("year", "assessmentyear",
              "recruitmentage"), 
            funs(as.integer)) 


save(qcsexcel, file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))

# -----------------------------------------------------------------------------------------
# load SAG full data (see: DownloadDataFromSAG.r)
# -----------------------------------------------------------------------------------------

sag <- 
  get(load(file=paste(advicedir, "/rdata/iSAGdownload 20181112.RData",sep=""))) %>% 
  select(-stockkey) %>% 
  left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  left_join(iStockkey, by="stockkey") %>% 
  mutate_at(c("recruitment","lowrecruitment","highrecruitment",  
              "tbiomass","lowtbiomass","hightbiomass", 
              "stocksize", "lowstocksize","highstocksize", 
              "catches", "landings","discards","ibc","unallocatedremovals",
              "fishingpressure", "lowfishingpressure","highfishingpressure",
              "fdiscards","flandings","fibc","funallocated",
              "fpa","bpa", "flim", "blim", "fmsy", "msybtrigger"), 
            funs(as.numeric)) %>% 
  mutate_at(c("year", "assessmentyear", "recruitmentage","stockdatabaseid"), funs(as.integer)) %>%
  mutate_at(c("published"),  funs(as.logical)) %>% 
  mutate_at(c("purpose"), funs(tolower)) %>% 
  
  # change -alt for stock assessments to purpose "alternative"
  mutate(purpose       = ifelse(grepl("\\-alt", stockkeylabel), "alternative", purpose), 
         stockkeylabel = ifelse(grepl("\\-alt", stockkeylabel), gsub("\\-alt","",stockkeylabel), stockkeylabel)) %>% 
  
  # remove erroneous assessments for blue whiting (trials?)
  filter(!(stockkeylabel == "whb-comb" & assessmentyear == 1996)) %>% 
  mutate(purpose = ifelse(purpose %in% c("initial advice", "initadvice", "replaced"), "withdrawn", purpose)) %>% 
  
  group_by(stockkey, stockkeylabel, assessmentyear, purpose, published, year) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

save(sag, file=paste(advicedir, "/rdata/iSAG.RData",sep=""))
glimpse(sag)

# sag %>% distinct(purpose) %>% View()
# sag %>% distinct(published) %>% View()
# sag %>% distinct(purpose, published) %>% View()

sag %>% filter(grepl("jun", stockkeylabel)) %>% View()
sag %>% filter(purpose == "alternative") %>% View()

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

# Plot of number of reference points
iAdvice %>% 
  group_by(assessmentyear) %>% 
  summarize(flim = sum(!is.na(flim)),
            fpa  = sum(!is.na(fpa)),
            fmsy = sum(!is.na(fmsy)),
            blim = sum(!is.na(blim)),
            bpa  = sum(!is.na(bpa)),
            msybtrigger = sum(!is.na(msybtrigger)) ) %>% 
  gather(key=variable, value=value, flim:msybtrigger) %>% 
  mutate(type = ifelse(substr(variable,1,1) == "f", "fishing mortality", "biomass")) %>% 

  
  ggplot(aes(x=assessmentyear, y=value)) +
  theme_publication() +
  geom_line(aes(colour=factor(variable))) +
  facet_wrap(~type)
  

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
# iAssess_part1: standard graph data SAG and prepare for combining
# -----------------------------------------------------------------------------------------

# Extract list of fishstock and assessment years from SAG database
sag_unique <-
  sag %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, purpose, published) %>% 
  ungroup()

# filter(sag_, grepl("ang-kask", stockkeylabel)) %>% View()
# filter(sag_unique, grepl("nop", stockkeylabel)) %>% View()
# filter(sag_unique, is.na(stockkey)) %>% View()
# filter(sag_unique, !published & purpose=="advice") %>% View()
# glimpse(iAssess_part1)

# -----------------------------------------------------------------------------------------
# iAssess_part2: Old excel assessment database and prepare for combining
# -----------------------------------------------------------------------------------------

qcsexcel_ <-
  qcsexcel %>% 
  mutate(purpose = tolower(purpose))

# Extract list of fishstock and assessment years from old database
qcsexcel_unique <-
  qcsexcel_ %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, purpose) %>% 
  ungroup()

# -----------------------------------------------------------------------------------------
# compare: what is in common; what is only in SAG and what is only in QCSEXCEL?
# -----------------------------------------------------------------------------------------

incommon <-
  intersect(qcsexcel_unique, sag_unique)

only_in_qcsexcel <-
  setdiff(qcsexcel_unique, sag_unique)

only_in_sag <-
  setdiff(sag_unique, qcsexcel_unique)

# -----------------------------------------------------------------------------------------
# compare filesets in common
# -----------------------------------------------------------------------------------------

t1 <-
  bind_rows(incommon, only_in_qcsexcel) %>% 
  left_join(qcsexcel_, by=c("stockkey","stockkeylabel", "assessmentyear", "purpose")) %>% 
  mutate(source = "excel")

t2 <-
  only_in_sag %>% 
  left_join(sag_, by=c("stockkey","stockkeylabel", "assessmentyear", "purpose")) %>% 
  dplyr::select(one_of(names(t1))) %>% 
  mutate(source = "sag")

t3 <-
  iAdvice %>% 
  ungroup() %>% 
  select(stockkey, stockkeylabel, assessmentyear, assessmentdate, assessmenttype, 
         flim, fpa, fmsy,
         blim, bpa, msybtrigger,
         assessmentmodel, assessmentscale,
         firstyearofdata, agerange, ncpueseries, nsurveyseries) %>%
  rename(purpose=assessmenttype) %>% 
  mutate(purpose = tolower(purpose),
         purpose = ifelse(is.na(purpose), "advice", purpose)) 

iAssess <-
  bind_rows(t1, t2) %>% 
  # filter(purpose=="advice") %>% 
  left_join(t3, by=c("stockkey","stockkeylabel","assessmentyear", "assessmentdate", "purpose"))

# test unique fields
iAdvice %>% 
  filter(grepl("whb", stockkeylabel)) %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, assessmentdate, assessmenttype) %>% 
  View()

bind_rows(t1, t2) %>% 
  filter(grepl("whb", stockkeylabel)) %>% 
  distinct(stockkey, assessmentyear, assessmentdate, purpose, source) %>%
  arrange(stockkey, assessmentyear) %>% 
  View()


  # ggplot(aes(x=year, y=stocksize, group=assessmentyear)) +
  # theme_publication() +
  # geom_line(aes(colour=factor(source))) +
  # 
  # geom_path(data=t3, aes(x=year,y=stocksize), colour="black", inherit.aes = FALSE) +
  # geom_point(data=t3, aes(x=year,y=stocksize), colour="black", inherit.aes = FALSE) +
  # 
  # expand_limits(y=0)

# number of benchmarks
iAdvice %>% 
  filter(grepl("benchmark", assessmenttype) ) %>% 
  group_by(assessmentyear) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=assessmentyear, y=n)) +
  theme_publication() +
  geom_line(colour="red")


# check blue whiting
bind_rows(t1, t2) %>% 
  filter(grepl("whb", stockkeylabelold)) %>%
  filter(assessmentyear < 1997) %>% 
  View()

  
# number of changes in reference points 












# -----------------------------------------------------------------------------------------
# Below is old code that needs revising. 
# -----------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------
# iAssess_part3: dataset from old Quality Control Sheets
# -----------------------------------------------------------------------------------------

iAssess_part3 <-
  qcsdata

# Extract list of fishstock and assessment years from old QCS database
iAssess_part3_unique <-
  iAssess_part3 %>% 
  group_by(stockkey, assessmentyear, stockkeylabel, assessmenttype) %>% 
  filter(row_number()==1) %>% 
  select(stockkey, assessmentyear, stockkeylabel, assessmenttype) %>% 
  ungroup()

# Select from iAssess_part3 the assessments to be added to iAssess_part1
iAssess_part3_toadd <-
  setdiff(select(iAssess_part3_unique, stockkey, assessmentyear, assessmenttype),
          select(iAssess_part1_unique, stockkey, assessmentyear, assessmenttype)) %>% 
  left_join(iAssess_part3, by=c("stockkey","assessmentyear", "assessmenttype")) %>% 
  data.frame()

# setdiff(names(iAssess_part1), names(iAssess_part2))
# setdiff(names(iAssess_part2), names(iAssess_part3))

# setdiff(names(iAssess_part2), names(iAssess_part1))
# setdiff(names(iAssess_part3), names(iAssess_part2))
# glimpse(iAssess_part3)
# filter(iStock, grepl("nop-34", stockkeylabel)) %>% View()

# -----------------------------------------------------------------------------------------
# Generate iAssess and do cleaning up
# -----------------------------------------------------------------------------------------

iAssess <- 
  bind_rows(iAssess_part1, iAssess_part2_toadd) %>%  
  bind_rows(., iAssess_part3_toadd) %>% 

  # corrections to stock size descriptions
  mutate(
    stocksizedescription = gsub("stock size: "                 ,""                   , stocksizedescription),
    stocksizedescription = gsub("indices"                      ,"index"              , stocksizedescription),
    stocksizedescription = gsub("indicator"                    ,"index"              , stocksizedescription),
    stocksizedescription = gsub("^biomass$"                    ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^biomass index$"              ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^evhoe biomass index$"        ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^stock size index: abundance$","abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^abundance$"                  ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^density$"                    ,"density index"      , stocksizedescription),
    stocksizedescription = gsub("^tsb$"                        ,"total biomass"      , stocksizedescription),
    stocksizedescription = gsub("^total abundance index$"      ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^index$"                      ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^stock abundance$"            ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^density index$"              ,"abundance index"    , stocksizedescription),
    
    stocksizedescription = gsub("stock size index: biomass \\(ages 1-8\\)"   ,"total biomass index"      , stocksizedescription),
    stocksizedescription = gsub("stock size index: german survey"            ,"total biomass index"      , stocksizedescription),
    stocksizedescription = gsub("stock size index: smoothed greenland index" ,"total biomass index"      , stocksizedescription),
    
    stocksizedescription = ifelse(grepl("tv",stocksizedescription), "abundance index", stocksizedescription),
    stocksizedescription = ifelse(grepl("ssb & b",stocksizedescription) , "ssb", stocksizedescription),
    stocksizedescription = ifelse(grepl("total biomass/bmsy",stocksizedescription) , "b/bmsy", stocksizedescription),
    stocksizedescription = ifelse(stocksizedescription == "stock size" & stocksizeunits == "tonnes", "ssb", stocksizedescription),
    stocksizedescription = ifelse(stocksizedescription == "stock size" & grepl("kg/",stocksizeunits), "total biomass index", stocksizedescription),
    stocksizedescription = ifelse(grepl("relative", stocksizeunits, fixed=TRUE) & is.na(stocksizedescription), "total biomass index", stocksizedescription)
  ) %>% 
  
  # corrections to stock units
  mutate(
    stocksizeunits = gsub("stock size: "                 ,""                   , stocksizeunits),
    stocksizeunits = gsub(" ", "", stocksizeunits),
    stocksizeunits = gsub("density(burrows/m2)", "burrows/m2", stocksizeunits, fixed=TRUE),
    stocksizeunits = gsub("cpue(kg/1000hooks)", "kg/1000hooks", stocksizeunits, fixed=TRUE),
    stocksizeunits = gsub("^abundance$", "millions", stocksizeunits),
    stocksizeunits = gsub("na(ratio)", "relative", stocksizeunits, fixed=TRUE),
    stocksizeunits = ifelse(grepl("kg/h", stocksizeunits, fixed=TRUE), "kg/hour", stocksizeunits),
    stocksizeunits = ifelse(grepl("n/h", stocksizeunits, fixed=TRUE), "n/hour", stocksizeunits)
  ) %>% 

  # corrections to fishing pressure descriptions
  mutate(
    fishingpressuredescription = gsub("fishing pressure: ",""  , fishingpressuredescription),
    fishingpressuredescription = gsub(" ",""  , fishingpressuredescription),
    fishingpressuredescription = gsub("f&hr","f"  , fishingpressuredescription, fixed=TRUE),
    fishingpressuredescription = gsub("fishingpressure","f"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("finwinterrings","f"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("weightedf","f"  , fishingpressuredescription), 
    
    fishingpressuredescription = gsub("harvestrate","hr"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("relativehr","hr/index"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("hrindex","hr/index"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("relativeexploitationrate","hr/index"  , fishingpressuredescription), 
    
    fishingpressuredescription = ifelse(grepl("ages",fishingpressuredescription), "f", fishingpressuredescription),
    fishingpressuredescription = ifelse(grepl("null",fishingpressuredescription), NA, fishingpressuredescription),
    
    fishingpressureunits       = ifelse(grepl("relative",fishingpressuredescription) & is.na(fishingpressureunits), "relative", fishingpressureunits ),
    fishingpressuredescription = ifelse(grepl("relative",fishingpressuredescription) , "fproxy", fishingpressuredescription )
  ) %>% 
  

  # corrections to fishing pressure units
  mutate(
    fishingpressureunits = gsub(" ",""  , fishingpressureunits),
    fishingpressureunits = gsub("peryear","year-1"  , fishingpressureunits),
    fishingpressureunits = gsub("%","percent"  , fishingpressureunits),
    fishingpressureunits = gsub("^f$","year-1"  , fishingpressureunits),
    fishingpressureunits = gsub("^catch/biomass$","relative"  , fishingpressureunits),

    fishingpressureunits = ifelse(grepl("cm",fishingpressureunits), "year-1",fishingpressureunits) ,
    fishingpressureunits = ifelse(grepl("null",fishingpressureunits), NA,fishingpressureunits) ,
    fishingpressureunits = ifelse(grepl("ratio",fishingpressureunits), "relative",fishingpressureunits) 
  ) %>% 
  
  # correction due to missing units
  mutate(
    stocksizeunits       = ifelse(stocksizedescription=="b/bmsy" & is.na(stocksizeunits),"relative",stocksizeunits),  
    fishingpressureunits = ifelse(fishingpressuredescription=="f/fmsy" & is.na(fishingpressureunits),"relative",fishingpressureunits)
  ) %>% 
  
  # corrections to the assignments of specific stocks and years
  mutate(
    stocksizedescription       = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"b/bmsy"  ,stocksizedescription),
    stocksizeunits             = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"relative",stocksizeunits),
    fishingpressuredescription = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"f/fmsy"  ,fishingpressuredescription),
    fishingpressureunits       = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"relative",fishingpressureunits)
  ) %>% 
  
  # remove double series (e.g. mac-nea 2013 is twice in the sag download)
  group_by(stockkeylabel, assessmentyear, year, assessmenttype) %>% 
  filter(row_number() == 1) %>% 
  
  # add old and new names
  left_join(iStockkey, by="stockkey") %>% 
  
  # add species info
  left_join(iSpecies, by="speciesfaocode") %>% 
  
  # add stock info
  # left_join(iStock, by=c("stockkey","stockkeylabel","assessmentyear","assessmenttype","datepublished")) %>% 
  
  # convert to lowercase
  ungroup() %>% 
  as.data.frame()

save(iAssess, file=paste(advicedir, "/rdata/iAssess.RData",sep=""))
# load(file="rdata/iAssess.RData")

# filter(iAssess, assessmentyear == 2017 & grepl("mac", stockkeylabel)) %>% View()
# filter(iAssess, assessmentyear == 2016 & grepl("mac", stockkeylabel)) %>% View()
# filter(iAssess_part2_toadd, assessmentyear == 2016 & grepl("mac", stockkeylabel)) %>% View()

# glimpse(iAssess)
# sortunique(iAssess$stockkeylabel)
# filter(iAssess, grepl("june", stockkeylabel)) %>% View()

# --------------------------------------------------------------------------------------------
# Check names 
# --------------------------------------------------------------------------------------------

# setdiff(names(iForecast), names(iRename))
# setdiff(names(iAdvice), names(iRename))
# setdiff(names(iManage), names(iRename))
# setdiff(names(iAssess), names(iRename))
# setdiff(names(iAssess), names(iStock))
# 
# setdiff(names(iAssess_part1), names(iAssess_part2_toadd))
# setdiff(names(iAssess_part1), names(iAssess_part3_toadd))
# setdiff(names(iAssess), names(iStock))
# 
# intersect(names(iAssess),names(iStock))

# filter(iAssess, stockkeylabelold == "whb-comb", assessmentyear == 2017)

# count_not_na(iStock)
# count_not_na(iAssess)
# 
# t <-
#   iAssess %>% 
#   left_join(as.data.frame(iStock), by=c("stockkey","stockkeylabel","assessmentyear"))
# 
# count_not_na(t)
# glimpse(iStock)
# glimpse(iAssess)
# 
# class(as.data.frame(iStock))
# class(iAssess)
# unique(iAssess$datepublished)
# unique(iStock$datepublished)
# 
# filter(iAssess, stockkeylabelold =="cod-iceg") %>% View()

