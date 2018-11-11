# -----------------------------------------------------------------------------------------------
# IDatabases generator.r
#
# iSpecies  : properties of species (names etc.)
# iStock    : properties of stocks and assessment
# iAssess   : stock assessment database (by stock, assessmentyear and year and date)
# iAdvice   : ICES advice and advice basis
# iForecast : short term forecasts
# iManage   : management actions and implementation
# iKeys     : transferring names of stocks
# 
# stocklist : stock list with stock description and advice status by assessment year - integrate into iStock
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
# 03/11/2017 now merges from separate datasets: qcsdata, exceldata, sagdata
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
# getSD         <- icesSD::getSD()
# save(getSD        , file=paste(dropboxdir, "/rdata/getSD.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/getSD.RData",sep=""))

# List stocks
# getListStocks <- icesSAG::getListStocks(year=0)
# save(getListStocks, file=paste(dropboxdir, "/rdata/getListStocks.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/getListStocks.RData",sep=""))

# SAG
getSAG        <- icesSAG::getSAG(stock=NULL, year=0, data="summary", combine=TRUE)
save(getSAG       , file=paste(dropboxdir, "/rdata/getSAG.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/getSAG.RData",sep=""))


# refpoints        <- icesSAG::getSAG(stock=NULL, year=0, data="refpoints", combine=TRUE)
# refpoints <- icesSAG::getFishStockReferencePoints(icesSAG::findAssessmentKey(year=0))



# -------------------------------------------------------------------------------------------------
# iStock, iStockkey, iRename
# Generate iStock (by stock, assessment year and date) - information on stock assessment properties
# Note: iStock read from Excel file because substantial amount of information added, e.g. areas
# -------------------------------------------------------------------------------------------------

# load iStock etc datafiles
load(file=paste(dropboxdir, "/rdata/iStock.RData", sep=""))
load(file=paste(dropboxdir, "/rdata/iStockkey.RData", sep=""))
load(file=paste(dropboxdir, "/rdata/iRename.RData", sep=""))

# -----------------------------------------------------------------------------------------
# Convert ICES stock database to iStockkey and iRename object for renaming between new and 
# old stockkeylabels. 
# iStockkey has all the stockkeys with both the new and the old stockkeylabels. 
# iRename has the stockkey, the stockkeylabel, the long names and the FAO species code
# -----------------------------------------------------------------------------------------

# t <-
#   getSD %>%
#   lowcase %>% 
#   group_by(stockkeylabel) %>% 
#   filter(row_number() == 1) %>% 
#   select(stockkey, stockkeylabel, stockkeydescription, previousstockkey, previousstockkeylabel) 

# write.csv(t, file="downloads/icessd_raw.csv")
# filter(t, stockkeylabel == "ang-78ab")
# filter(t, grepl("^an", stockkeylabel)) %>% View()
# range(t$stockkey, na.rm=TRUE)

# old assessment codes
# told <-
#   t %>% 
#   filter(is.na(previousstockkey)) %>% 
#   ungroup() %>% 
#   select(stockkey, stockkeylabel) %>% 
#   arrange(stockkey)

# filter(x, stockkeylabel == "ang-78ab")

# old stock description names
# oldnames <- 
#   readxl::read_excel(path=paste(dropboxdir,"/downloads/ICES old names.xlsx",sep=""), col_names=TRUE, col_types="text") 

# new assessment codes
# tnew <-
#   t %>% 
#   filter(!is.na(previousstockkey)) %>% 
#   arrange(stockkey)

# create iStockkey dataset
# iStockkey <-
#   told %>% 
#   left_join(tnew, by=c("stockkeylabel" = "previousstockkeylabel")) %>% 
#   ungroup() %>% 
#   mutate(stockkey         = ifelse(!is.na(stockkey.y), stockkey.y, stockkey.x),
#          stockkeylabelold = stockkeylabel,
#          stockkeylabelnew = stockkeylabel.y) %>% 
#   select(stockkey, stockkeylabelnew, stockkeylabelold)

# filter(iStockkey, grepl("agn", stockkeylabelold)) %>% View()

  
# create iRename dataset
# iRename <-
#   told %>% 
#   left_join(tnew, by=c("stockkeylabel" = "previousstockkeylabel")) %>% 
#   ungroup() %>% 
#   mutate(stockkey = ifelse(!is.na(stockkey.y), stockkey.y, stockkey.x) ) %>% 
#   select(stockkey, stockkeylabel, stockkeydescription ) %>% 
#   data.frame() %>% 
#   
#   # add the new assessment codes
#   rbind(data.frame(select(tnew, stockkey, stockkeylabel, stockkeydescription))) %>% 
#   
#   # add historic names
#   left_join(oldnames, by=c("stockkeylabel")) %>% 
#   mutate(stockkeydescription = ifelse(!is.na(stockkeydescription.y), stockkeydescription.y, stockkeydescription.x)) %>% 
#   select(stockkey, stockkeylabel, stockkeydescription ) %>% 
#   
#   # fix problem with missing stockkey
#   mutate(stockkey = ifelse(stockkeylabel == "sal-wgc", 999999, stockkey)) %>% 
#   
#   # add fao code
#   mutate(speciesfaocode = substr(stockkeylabel,1,3)) %>% 
#   
#   arrange(stockkeylabel, stockkey)

# filter(x, grepl("ang|ank|anf|anb", stockkeylabel)) %>% View()
# filter(told, grepl("ang|ank|anf|anb", stockkeylabel)) %>% View()
# filter(told, grepl("whb", stockkeylabel)) %>% View()
# filter(tnew, grepl("whb", stockkeylabel)) %>% View()
# filter(iStockkey, grepl("whb", stockkeylabelnew)) %>% View()
# filter(iRename, grepl("agn", stockkeylabel)) %>% View()

# save(iStockkey, file=paste(dropboxdir, "/rdata/iStockkey.RData",sep=""))
# save(iRename, file=paste(dropboxdir, "/rdata/iRename.RData",sep=""))
# load(file="rdata/iRename.RData")

# glimpse(iStockkey)
# glimpse(iRename)

# rm(t, tnew, told, oldnames)

# -----------------------------------------------------------------------------------------
# Read the species database (from excel, not from ICES SAG because more information 
# added to excel version). 
# iSpecies has information on species properties, like different names etc. 
# -----------------------------------------------------------------------------------------

iSpecies <-
  readxl::read_excel(
    path=paste(dropboxdir, "/downloads/species_list.xlsx",sep=""), col_names=TRUE, col_types="text") %>%
  mutate_at(c("speciescommonname","trophicguild","fisheriesguild","sizeguild"), 
            funs(tolower)) %>%
  group_by(speciesfaocode, speciesscientificname, speciescommonname) %>%
  arrange(speciesfaocode) 

save(iSpecies, file=paste(dropboxdir, "/rdata/iSpecies.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/iSpecies.RData",sep=""))
# filter(iSpecies, grepl("agn", speciesfaocode)) %>% View()

# glimpse(iSpecies)

# -----------------------------------------------------------------------------------------
# iAdvice, iForecast, iManage
# Read ICES advice database and split up in different parts (iAdvice, iForecast, iManage, ...)
# Eventually this will need to be done as integrated part of the assessment information basis. 
# -----------------------------------------------------------------------------------------

t <- 
  readxl::read_excel(path= paste(get_dropbox(), 
                                 "/ICES advice database/ICES scientific advice database.xlsx", sep=""), 
                     col_names = TRUE, 
                     col_types = "text", 
                     trim_ws   = TRUE) %>%
  lowcase %>% 
  rename(stockkeylabel = stockices, 
         managementyear = year,
         assessmentyear = assessyear) %>% 
  mutate_at(c("advisedlandingsmax","advisedcatchmax","tal","tac","officiallandings","iceslandings","icesindustrialbycatch",
              "icesdiscards","icescatch","fsqymin1","ssbymin1","fadvmax","fmax","f01","fmed","f35spr","flim","fpa","fmsy",
              "blim","bpa","msybtrig"), 
            funs(as.numeric)) %>%
  mutate_at(c("managementyear", "assessmentyear","firstyearofdata","ncpueseries","nsurveyseries"), 
            funs(as.integer)) %>% 
  left_join(iRename, by="stockkeylabel")
  
# filter(t, grepl("whb", stockkeylabel)) %>% View()

# Generate iAdvice
iAdvice <-
  t %>% 
  filter(!is.na(advicebasis)) %>% 
  filter(!is.na(stockkey)) %>% 
  select(stockkey, stockkeylabel, stockkeydescription, speciesfaocode, managementyear, advicebasis, advisedlandings, advisedcatch,
         advisedlandingsmax, advisedcatchmax) %>% 
  data.frame()

# Generate iManage
iManage <-
  t %>% 
  select(stockkey, stockkeylabel, stockkeydescription, stockarea, stocksubarea, 
         managementyear, managementauthority, taccode, tacarea, multispeciesmanagement, 
         stockmanagement, aggregateat, aggregated, 
         tal, tac, officiallandings, iceslandings, icesindustrialbycatch, icesdiscards, icescatch) %>% 
  data.frame()

# Generate iForecast
iForecast <-
  t %>% 
  filter(!is.na(assessmentyear)) %>% 
  filter(!is.na(stockkey)) %>% 
  select(stockkey, stockkeylabel, managementyear, fsqpreviousyear=fsqymin1, ssbpreviousyear = ssbymin1, fadvmax) %>% 
  data.frame()

# rm(t)

save(iAdvice     , file=paste(dropboxdir, "/rdata/iAdvice.RData", sep=""))
save(iForecast   , file=paste(dropboxdir, "/rdata/iForecast.RData", sep=""))
save(iManage     , file=paste(dropboxdir, "/rdata/iManage.RData", sep=""))

# glimpse(iAdvice)
# glimpse(iForecast)
# glimpse(iManage)


# -----------------------------------------------------------------------------------------
# iAssess_part1: download standard graph data SAG and prepare for combining
# -----------------------------------------------------------------------------------------

iAssess_part1 <- 
  # read.csv(file=paste(dropboxdir,"/downloads/sagdownload.csv", sep=""), stringsAsFactors =FALSE) %>%
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

# Extract list of fishstock and assessment years from SAG database
iAssess_part1_unique <-
  iAssess_part1 %>% 
  group_by(stockkey, assessmentyear, stockkeylabel, assessmenttype2, datepublished) %>% 
  filter(row_number()==1) %>% 
  select(stockkey, assessmentyear, stockkeylabel, assessmenttype2, datepublished) %>% 
  ungroup()


# need to add the assessment date to the database !!!!!

# filter(iAssess_part1, grepl("whb", stockkeylabel)) %>% View()
# filter(iAssess_part1_unique, grepl("nop", stockkeylabel)) %>% View()
# glimpse(iAssess_part1)

# -----------------------------------------------------------------------------------------
# iAssess_part2: Old excel assessment database and prepare for combining
# -----------------------------------------------------------------------------------------

iAssess_part2 <-
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

# filter(iAssess_part2, grepl("whb", stockkeylabel)) %>% View()

# Extract list of fishstock and assessment years from old database
iAssess_part2_unique <-
  iAssess_part2 %>% 
  group_by(stockkey, assessmentyear, stockkeylabel, assessmenttype2, datepublished) %>% 
  filter(row_number()==1) %>% 
  select(stockkey, assessmentyear, stockkeylabel, assessmenttype2, datepublished) %>% 
  ungroup()

# Select from iAssess_part2 the assessments to be added to iAssess_part1

iAssess_part2_toadd <-
  setdiff(select(iAssess_part2_unique, stockkey, assessmentyear, assessmenttype2, datepublished),
          select(iAssess_part1_unique, stockkey, assessmentyear, assessmenttype2, datepublished)) %>% 
  left_join(iAssess_part2, by=c("stockkey","assessmentyear", "assessmenttype2", "datepublished")) %>% 
  data.frame()

# setdiff(names(iAssess_part1), names(iAssess_part2))
# setdiff(names(iAssess_part2), names(iAssess_part1))
# filter(iAssess_part2_toadd, grepl("whb", stockkeylabel)) %>% View()
# glimpse(iAssess_part2)
# sortunique(iAssess_part2$assessmenttype2)

# -----------------------------------------------------------------------------------------
# iAssess_part3: dataset from old Quality Control Sheets
# -----------------------------------------------------------------------------------------

iAssess_part3 <-
  get(load(file="rdata/qcsdata.RData") ) %>% 
  rename(recruitment=r) %>% 
  mutate(assessmenttype2 = "assess",
         source = "qcs",
         datepublished=as.Date(NA)) %>% 
  select(stockkey, stockkeylabel, stockkeydescription, speciesfaocode, 
         assessmentyear, assessmenttype2, datepublished, year, 
         recruitment, recruitmentage, recruitmentdescription, recruitmentunits, 
         ssb, stocksizedescription, stocksizeunits,
         f, fishingpressuredescription, fishingpressureunits,
         source) %>% 
  
  # need to fix the missing stockkeys. For now just remove
  filter(!is.na(stockkey))


# unique(filter(iRename, grepl("cod",stockkeylabel))$stockkeylabel)
# filter(iAssess_part3, is.na(stockkey)) %>% distinct(stockkeylabel)
# count_not_na(iAssess_part3)
# filter(iAssess_part3, is.na(stockkey)) %>% View()
# filter(iRename, grepl("nop", stockkeylabel)) %>% distinct(stockkeylabel)


# Extract list of fishstock and assessment years from old QCS database
iAssess_part3_unique <-
  iAssess_part3 %>% 
  group_by(stockkey, assessmentyear, stockkeylabel, assessmenttype2, datepublished) %>% 
  filter(row_number()==1) %>% 
  select(stockkey, assessmentyear, stockkeylabel, assessmenttype2, datepublished) %>% 
  ungroup()

# Select from iAssess_part3 the assessments to be added to iAssess_part1
iAssess_part3_toadd <-
  setdiff(select(iAssess_part3_unique, stockkey, assessmentyear, assessmenttype2, datepublished),
          select(iAssess_part1_unique, stockkey, assessmentyear, assessmenttype2, datepublished)) %>% 
  left_join(iAssess_part3, by=c("stockkey","assessmentyear", "assessmenttype2","datepublished")) %>% 
  
  # add species information
  left_join(iSpecies, by="speciesfaocode") %>% 
  
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
  group_by(stockkeylabel, assessmentyear, year, assessmenttype2) %>% 
  filter(row_number() == 1) %>% 
  
  # add old and new names
  left_join(iStockkey, by="stockkey") %>% 
  
  # add species info
  left_join(iSpecies, by="speciesfaocode") %>% 
  
  # add stock info
  # left_join(iStock, by=c("stockkey","stockkeylabel","assessmentyear","assessmenttype2","datepublished")) %>% 
  
  # convert to lowercase
  ungroup() %>% 
  as.data.frame()

save(iAssess, file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))
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

