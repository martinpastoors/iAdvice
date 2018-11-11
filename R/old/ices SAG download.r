# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph download
#
# stocknames: lookup list to transfer old and new stock names
# stocklist : stock list with stock description and advice status by assessment year
# speciesdb : properties of species (names etc.)
# stockdb   : properties of stocks
# sagdb     : stock assessment database (by stock, assessmentyear and year)
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
# -----------------------------------------------------------------------------------------------

# ICES Stock database
# library(devtools)
# devtools::install_github("ices-tools-prod/icesSD")
# devtools::install_github("ices-tools-prod/icesSAG")
# library(icesSD)  # ICES Stock database
# library(icesSAG)  # ICES Stock Assessment Graphs

# which functions in a package?
# lsf.str("package:icesSAG")
# lsf.str("package:icesSD")

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- get_dropbox()
setwd(paste(dropboxdir, "/ICES Assessment database", sep=""))


# -----------------------------------------------------------------------------------------
# set the year
# -----------------------------------------------------------------------------------------

myyear <- 0   # use 0 for all years

# -----------------------------------------------------------------------------------------
# read stock names (for lookup old names and new names)
# -----------------------------------------------------------------------------------------

stocknames <-
  read.csv("D:/Dropbox/ICES Assessment database/ICES StocksOldCodesNewCodesEGs.csv", stringsAsFactors = FALSE) %>% 
  select(1,2) %>% 
  setNames(., c("fishstockold","fishstock")) 

# -----------------------------------------------------------------------------------------
# get ICES stock database and create species database (by species) and stock database (by stock)
# -----------------------------------------------------------------------------------------

# s <-
#   icesSD::getSD() %>%
#   lowcase %>%
#   rename(fishstock      = stockkeylabel,
#          assessmentyear = activeyear,
#          scientificname = speciesscientificname,
#          commonname     = speciescommonname) %>%
#   mutate   (fishstock   = gsub(" ", "", fishstock))


# -----------------------------------------------------------------------------------------
# read the species database (from excel, not from ICES SAG because more information added to excel version)
# -----------------------------------------------------------------------------------------

# speciesdb <-
#   readxl::read_excel(path="downloads/species_list.xlsx", col_names=TRUE, col_types="text") %>%
#   mutate_at(c("commonname","trophicguild","fisheriesguild","sizeguild"), funs(tolower)) %>%
#   group_by(faocode, scientificname, commonname) %>%
#   filter(row_number() == 1) %>%
#   select(faocode, scientificname, commonname, trophicguild, fisheriesguild, sizeguild) %>%
#   arrange(faocode) %>%
#   filter(!is.na(faocode) & !is.na(scientificname) & !is.na(commonname))
    
# save(speciesdb, file="rdata/speciesdb.RData")
# write.csv(data.frame(names(speciesdb)), file="downloads/speciesdb_fields.csv", row.names=FALSE)
load(file="rdata/speciesdb.RData")


# glimpse(speciesdb)
# Observations: 97, Variables: 6
# $ faocode        <chr> "agn", "alf", "anb", "ane", "anf", "ang", "ank", "anp", "arg", "aru", "bli", "bll", "boc", "bsf", "bsh", "bs...
# $ scientificname <chr> "Squatina squatina", "Beryx", "Lophius budegassa", "Engraulis encrasicolus", "Lophius budegassa, Lophius pis...
# $ commonname     <chr> "angel shark", "alfonsinos", "black-bellied anglerfish", "anchovy", "anglerfish", "anglerfish", "black-belli...
# $ trophicguild   <chr> "demersal piscivore", "pelagic piscivore", "demersal piscivore", "pelagic planktivore", "demersal piscivore"...
# $ fisheriesguild <chr> "elasmobranch", "demersal", "demersal", "pelagic", "benthic", "demersal", "benthic", "benthic", "pelagic", "...
# $ sizeguild      <chr> "large sharks", "medium benthopelagic", "large demersal", "small pelagic", "large demersal", "large demersal...

# -----------------------------------------------------------------------------------------
# create stock database (by stock and assessment year)
# -----------------------------------------------------------------------------------------

# stockdb <-
#   s %>%
#   mutate(faocode = substr(fishstock,1,3)) %>%
#   select(fishstock,
#          assessmentyear,
#          ecoregion,
#          expertgroup, advicedraftinggroup,
#          datacategory, assessmentfrequency, assessmenttype,
#          advicecategory, advicetype, useofdiscardsinadvice,
#          pabufferapplied) %>%
# 
#   # lowercase and remove na text strings
#   mutate_at(vars("assessmenttype","advicecategory","advicetype","useofdiscardsinadvice", "pabufferapplied"), funs(tolower)) %>%
#   mutate(assessmenttype  = ifelse(assessmenttype == "na",NA, assessmenttype),
#          pabufferapplied = ifelse(pabufferapplied == "na", NA, pabufferapplied)) %>% 
#   rename(assessmentmodel = assessmenttype)

# save(stockdb, file="rdata/stockdb.RData")
# write.csv(data.frame(names(stockdb)), file="downloads/stockdb_fields.csv", row.names=FALSE)
load(file="rdata/stockdb.RData")

# glimpse(stockdb)
# Observations: 1,297, Variables: 15
# $ fishstock             <chr> "bsf-89", "bsf-nrtn", "bsf-oth", "rng-comb", "cod-offgr", "cod-offgr", "cod-ewgr", "had-scow", "had-3...
# $ assessmentyear        <int> 2013, 2013, 2013, 2013, 2014, 2013, 2013, 2013, 2013, 2014, 2013, 2013, 2013, 2013, 2014, 2013, 2013,...
# $ ecoregion             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ expertgroup           <chr> "WGDEEP", "WGDEEP", "WGDEEP", "WGDEEP", "NWWG", "NWWG", "NWWG", "WGCSE", "WGNSSK", NA, "WGEF", "WGNSS...
# $ advicedraftinggroup   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ datacategory          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "3.2", NA, NA, NA, NA, NA, "3.2", NA, NA, NA,...
# $ assessmentfrequency   <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ assessmenttype        <chr> NA, NA, NA, NA, "survey trends", NA, NA, NA, NA, NA, NA, NA, NA, NA, "survey trends from local compon...
# $ advicecategory        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ advicetype            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ useofdiscardsinadvice <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ pabufferapplied       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "no", NA, NA, NA, NA, NA, "no", NA, NA, NA, N...

# -----------------------------------------------------------------------------------------
# get stock list with stock description and advice status (is this in overlap with stockdb ??)
# -----------------------------------------------------------------------------------------
# p <-
#  icesSAG::getListStocks(year=myyear)
#  # write.csv(p, file="downloads/stocklist.csv", row.names=FALSE)
#  # write.csv(data.frame(names(p)), file="downloads/stocklist_fields.csv", row.names=FALSE)
# 
# stocklist <-
#   p %>%
#   lowcase %>%
#   rename(fishstock = stockkeylabel) %>%
#   select(fishstock, assessmentyear, stockdescription, status)

# save(stocklist, file="rdata/stocklist.RData")
load(file="rdata/stocklist.RData")

# glimpse(stocklist)
# Observations: 1,449, Variables: 4
# $ fishstock        <chr> "agn-nea", "agn-nea", "alf-comb", "alf-comb", "alf-comb", "anb-78ab", "anb-78ab", "anb-78ab", "anb-78ab", ...
# $ assessmentyear   <int> 1999, 2015, 2014, 2016, 1999, 2016, 2015, 2001, 2002, 2003, 2004, 2005, 2001, 2002, 2003, 2016, 2014, 2010...
# $ stockdescription <chr> "Angel shark (Squatina squatina) in the Northeast Atlantic", "Angel shark (Squatina squatina) in the North...
# $ status           <chr> "Not Published", "Published", "Not Published", "Published", "Not Published", "Published", "Not Published",...


# -----------------------------------------------------------------------------------------
# get reference points
# -----------------------------------------------------------------------------------------

# t <-
#   getSAG(stock=NULL, year=0, data="refpts", combine=TRUE)
#   # write.csv(t, file="downloads/refpoints.csv", row.names=FALSE)
#   # write.csv(data.frame(names(t)), file="downloads/refpoints_fields.csv", row.names=FALSE)
#   # t <- read.csv(file="downloads/refpoints.csv", stringsAsFactors = FALSE)
# 
# refpoints <-   
#   t %>% 
#   lowcase %>% 
#   rename(fishstock = stockkeylabel) %>% 
#   dplyr::select(fishstock, assessmentyear,
#                 flim, blim, fmsy, msybtrigger, fpa, bpa, 
#                 fmgt=fmanagement, bmgt=bmanagement,
#                 recruitmentage) %>% 
#   mutate_at(vars(flim:bmgt), funs(as.numeric)) 

# save(refpoints, file="rdata/refpoints.RData")
load(file="rdata/refpoints.RData")

# glimpse(refpoints)
# Observations: 620, Variables: 12
# $ fishstock         <chr> "agn-nea", "alf-comb", "anb-78ab", "anb-8c9a", "anb-8c9a", "anb-8c9a", "anb-8c9a", "anb-8c9a", "anb-8c9a"...
# $ assessmentyear    <int> 2015, 2016, 2016, 2016, 2014, 2012, 2011, 2015, 2013, 2017, 2016, 2015, 2014, 2013, 2016, 2015, 2016, 201...
# $ flim              <dbl> NA, NA, NA, 1.70, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1.70, NA, 0.60, NA, NA, NA, NA, NA,...
# $ blim              <dbl> NA, NA, NA, 0.3, NA, NA, NA, NA, NA, NA, 21000.0, 21000.0, 21000.0, 21000.0, NA, NA, NA, 0.3, NA, 1900.0,...
# $ fmsy              <dbl> NA, NA, NA, 1.000, 1.000, 1.000, 0.430, 1.000, 1.000, NA, NA, NA, NA, NA, NA, NA, NA, 1.000, NA, 0.310, 0...
# $ msybtrigger       <dbl> NA, NA, NA, 0.5, 0.5, 0.0, 0.0, 0.5, 0.5, NA, NA, NA, NA, 33000.0, NA, NA, NA, 0.5, NA, 5400.0, NA, NA, N...
# $ fpa               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.43, NA, NA, NA, NA, NA, NA,...
# $ bpa               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 33000, NA, NA, NA, NA, NA, 2600, NA, NA, NA, NA, NA, ...
# $ fmgt              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ bmgt              <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
# $ recruitmentage    <int> NA, NA, NA, NA, NA, 0, 0, 0, 0, NA, NA, 1, 1, 1, NA, NA, NA, NA, NA, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, ...

# -----------------------------------------------------------------------------------------
# read ICES advice database
# -----------------------------------------------------------------------------------------

load(file="../ICES advice database/rdata/iad.RData")

iad <-
  iad %>% 
  select(fishstock, assessmentyear, assessmentmodel = assessmodel,
         flim, fpa, fmsy, blim, bpa, msybtrig,
         firstyearofdata, ncpueseries, nsurveyseries, 
         assessmenttype=assesstype, assessmentcomment=assesscomment) %>% # Note: could add area info if needed
  filter(!is.na(assessmentmodel), assessmentmodel != "")

# -----------------------------------------------------------------------------------------
# Download standard graph data and combine with previous datasets
# -----------------------------------------------------------------------------------------

# sagdownload <-
#   icesSAG::getSAG(stock=NULL, year=0, data="summary", combine=TRUE) %>%
#   lowcase()
# write.csv(sagdownload, file="downloads/sagdownload.csv", row.names=FALSE)

sagdownload <- 
  read.csv(file="downloads/sagdownload.csv", stringsAsFactors =FALSE) %>% 
  lowcase %>% 
  dplyr:: select(fishstock, assessmentyear, year,
                 recruitment, highrecruitment, lowrecruitment,
                 ssb, highssb, lowssb,
                 f, highf, lowf, 
                 catches, landings, discards,
                 fage, 
                 units, 
                 stocksizedescription, stocksizeunits,
                 fishingpressuredescription, fishingpressureunits,
                 stockpublishnote) %>% 
  
  # add fao code if missing
  mutate(faocode = substr(fishstock,1,3)) %>% 
  
  # dealing with old and new stocknames
  left_join(stocknames, by = c("fishstock" = "fishstockold")) %>% 
  rename(fishstocknew = fishstock.y) %>% 
  left_join(stocknames, by = c("fishstock")) %>% 
  mutate(fishstocknew = ifelse(is.na(fishstocknew) & !is.na(fishstockold), fishstock, fishstocknew),
         fishstockold = ifelse(is.na(fishstockold) & !is.na(fishstocknew), fishstock, fishstockold),
         fishstockold = ifelse(is.na(fishstockold) & is.na(fishstocknew) , fishstock, fishstockold)) %>% 
  mutate_at(vars("stocksizedescription","stocksizeunits","fishingpressuredescription","fishingpressureunits"), funs(tolower)) %>% 
  
  
  # join relevant datasets
  left_join(refpoints, by=c("fishstock","assessmentyear")) %>% 
  left_join(stocklist, by=c("fishstock","assessmentyear")) %>% 
  left_join(stockdb  , by=c("fishstock","assessmentyear")) %>% 
  left_join(speciesdb, by=c("faocode")) %>% 
  left_join(iad,       by=c("fishstock","assessmentyear")) %>% 
  
  mutate(source   = "download", 
         flim     = ifelse(!is.na(flim.x), flim.x, flim.y),
         fpa      = ifelse(!is.na(fpa.x),  fpa.x, fpa.y),
         fmsy     = ifelse(!is.na(fmsy.x), fmsy.x, fmsy.y),
         blim     = ifelse(is.na(blim.x),  blim.x, blim.y),
         bpa      = ifelse(is.na(bpa.x),   bpa.x, bpa.y),
         msybtrig = ifelse(is.na(msybtrigger),  msybtrigger, msybtrig),
         assessmentmodel = ifelse(!is.na(assessmentmodel.y), assessmentmodel.y, assessmentmodel.x) ) %>% 
  
  select(-flim.x, -flim.y, -fpa.x, -fpa.y, -fmsy.x, -fmsy.y, -blim.x, -blim.y, -bpa.x, -bpa.y, -msybtrigger, 
         -assessmentmodel.x, -assessmentmodel.y) %>% 
  
  data.frame() 

# Extract list of fishstock and assessment years from SAG database
sagdownload_unique <-
  sagdownload %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  select(fishstock, assessmentyear)

# -----------------------------------------------------------------------------------------
# read old excel SAG database
# -----------------------------------------------------------------------------------------

sagexcel <-
  readxl::read_excel("ICES Assessment Summary database.xlsx",
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
  
  # dealing with old and new stocknames
  left_join(stocknames, by = c("fishstock" = "fishstockold")) %>% 
  rename(fishstocknew = fishstock.y) %>% 
  left_join(stocknames, by = c("fishstock")) %>% 
  mutate(fishstocknew = ifelse(is.na(fishstocknew) & !is.na(fishstockold), fishstock, fishstocknew),
         fishstockold = ifelse(is.na(fishstockold) & !is.na(fishstocknew), fishstock, fishstockold),
         fishstockold = ifelse(is.na(fishstockold) & is.na(fishstocknew) , fishstock, fishstockold)) %>% 
  
  
  mutate_at(vars("stocksizedescription","stocksizeunits","fishingpressuredescription","fishingpressureunits",
                 "commonname", "scientificname"), funs(tolower)) %>% 
  select(-assessmentkey, -icesareas, -report, -(unitofrecruitment:tbiomassunit), 
         -catcheslandingsunits, -officiallandings, -(ibc:yieldssb), -(flandings:funallocated), 
         -(flength:area), -scientificname, -commonname) %>% 
  filter(year <= assessmentyear) %>% 
  
  # add fao code if missing
  mutate(faocode = substr(fishstock,1,3)) %>% 
  
  # add species information
  left_join(speciesdb, by=c("faocode")) %>% 
  
  # add ICES Advice Database information
  select(-blim, -bpa, -msybtrigger,-flim, -fpa, -fmsy) %>% 
  left_join(iad, by=c("fishstock","assessmentyear")) %>% 
  
  mutate(source = "excel") 
  

# Extract list of fishstock and assessment years from old database
sagexcel_unique <-
  sagexcel %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  select(fishstock, assessmentyear)

# sagexcel_overview <-
#   sagexcel %>% 
#   group_by(fishstock, assessmentyear) %>% 
#   summarise(nssb = sum(!is.na(ssb)),
#          nf   = sum(!is.na(f)),
#          nrec = sum(!is.na(recruitment)),
#          nlan = sum(!is.na(landings))) %>% 
#   select(fishstock, assessmentyear, nlan, nrec, nssb, nf)
# 
# write.csv(sagexcel_overview, file="excel_overview.csv")



# -----------------------------------------------------------------------------------------
# Construct the excel sag dataset to add to the downloaded SAG dataset, 
# i.e. unique combinations of fishstock and assessmentyear
# -----------------------------------------------------------------------------------------

sagexcel_toadd <-
  setdiff(sagexcel_unique,sagdownload_unique) %>% 
  left_join(sagexcel, by=c("fishstock","assessmentyear")) %>% 
  data.frame()

# setdiff(names(sagexcel),names(sagdownload)) 
# setdiff(names(sagdownload),names(sagexcel)) 

# -----------------------------------------------------------------------------------------
# Add the unique data in the excel sag dataset to the downloaded sag dataset and do cleaning up
# -----------------------------------------------------------------------------------------

sagdb <- 
  rbind.all.columns(sagdownload, sagexcel_toadd) %>%  

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
  
  # Add assessment type2 category (assess, bench, old, alt, explore)
  # CHECK: Should I remove the labels from the fishstock variable  ??
  mutate(
    assessmenttype2 = ifelse(grepl("-bench$" , fishstock), "bench", "assess"),
    assessmenttype2 = ifelse(grepl("-old$"   , fishstock), "old"  , assessmenttype2),
    assessmenttype2 = ifelse(grepl("-alt$"   , fishstock), "alt"  , assessmenttype2)
  ) %>% 
  
  # correction due to missing units
  mutate(
    stocksizeunits       = ifelse(stocksizedescription=="b/bmsy" & is.na(stocksizeunits),"relative",stocksizeunits),  
    fishingpressureunits = ifelse(fishingpressuredescription=="f/fmsy" & is.na(fishingpressureunits),"relative",fishingpressureunits)
  ) %>% 
  
  # corrections to the assignments of specific stocks and years
  mutate(
    stocksizedescription       = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"b/bmsy"  ,stocksizedescription),
    stocksizeunits             = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"relative",stocksizeunits),
    fishingpressuredescription = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"f/fmsy"  ,fishingpressuredescription),
    fishingpressureunits       = ifelse(fishstock=="anb-8c9a" & assessmentyear==2013,"relative",fishingpressureunits)
  ) %>% 
  
  # remove double series (e.g. mac-nea 2013 is twice in the sag download)
  group_by(fishstock, assessmentyear, year) %>% 
  filter(row_number() == 1) %>% 
  
  # convert to lowercase
  mutate_at(vars(assessmenttype), funs(tolower)) %>% 
  
  # remove empty variables
  select(-recruitmentlength)


save(sagdb, file="rdata/sagdb.RData")
# load(file="rdata/sagdb.RData")



