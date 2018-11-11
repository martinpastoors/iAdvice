# -----------------------------------------------------------------------------------------------
# iDatabases checks
#
# 10/08/2017 created code from ICES SAG download.r
# 25/09/2017 cleaned up; only checking code left
# 23/10/2018 comparing iAdvice and iStock
# 25/10/2018 revising the 20181001 iAdvice database in Excel
# -----------------------------------------------------------------------------------------------

setwd("D:/Dropbox/ICES Assessment database")

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
assessdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")
advicedir  <- paste(get_dropbox(), "/ICES Advice database", sep="")

load(file=paste(assessdir, "/rdata/iRename.RData",sep=""))
load(file=paste(assessdir, "/rdata/iStockkey.RData",sep=""))

# Read and convert iAdvice spreadsheet
iAdvice <-
  
  # read from excel
  readxl::read_excel(
    path= paste(advicedir, "/Excel/ICES Scientific Advice database 20181001.xlsx", sep=""), 
    sheet     = "DATA",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  
  # make variables lowercase
  lowcase() %>% 
  
  # rename variables
  rename(tacyear = year, 
         landings=iceslandings, ibc=icesindustrialbycatch, discards=icesdiscards,
         catches = icescatch, msybtrigger=msybtrig, assessmentyear=assessyear,
         assessmenttype = assesstype, assessmentcomment=assesscomment, assessmentmodel = assessmodel, 
         catchlandingsunits = catchunits, stockkeylabel = stockices) %>% 
  
  # convert to numeric
  mutate_at(c("advisedlandingsmax", "advisedcatchmax", "tal", "tac", 
              "officiallandings", "landings", "ibc", "discards", "catches",
              "fsqymin1", "ssbymin1", "fadvmax", 
              "fmax", "f01", "fmed", "f35spr", "flim", "fpa", "fmsy",
              "blim", "bpa", "msybtrigger", 
              "m1", "m5"), 
            funs(as.numeric)) %>% 
  
  # remove certain columns
  # dplyr::select(-stockarea, -x1) %>% 
  
  # convert to integer
  mutate_at(c("tacyear", "assessmentyear", "firstyearofdata", "ncpueseries","nsurveyseries"), 
            funs(as.integer)) %>% 
  
  # merge with iRename
  left_join(dplyr::select(iRename,
                          stockkeylabel, stockkey, speciesfaocode), 
            by="stockkeylabel") %>% 
  
  # merge with iStockkey
  left_join(iStockkey, by="stockkey") %>% 
  
  # mutate units of advice, tac and catch
  mutate(
    advisedlandingsmax = ifelse(catchlandingsunits == "kT" & advisedlandings >=0, 
                                1000*advisedlandingsmax, advisedlandingsmax),
    advisedcatchmax    = ifelse(catchlandingsunits == "kT" & advisedcatch >=0, 
                                1000*advisedcatchmax, advisedcatchmax),
    tal                = ifelse(catchlandingsunits == "kT" & tal >= 0, 
                                1000 * tal, tal),
    tac                = ifelse(catchlandingsunits == "kT" & tac >= 0, 
                                1000 * tac, tac),
    officiallandings   = ifelse(catchlandingsunits == "kT" & officiallandings >= 0, 
                                1000 * officiallandings, officiallandings),
    landings           = ifelse(catchlandingsunits == "kT" & landings >= 0        , 
                                1000 * landings        , landings),
    ibc                = ifelse(catchlandingsunits == "kT" & ibc >= 0             , 
                                1000 * ibc             , ibc),
    discards           = ifelse(catchlandingsunits == "kT" & discards >= 0        , 
                                1000 * discards        , discards),
    catches            = ifelse(catchlandingsunits == "kT" & catches >= 0         , 
                                1000 * catches         , catches),
    catchlandingsunits = ifelse(catchlandingsunits == "kT" , 
                                "tonnes", catchlandingsunits),
    
    ssbymin1           = ifelse(ssbunits == "kT" & ssbymin1 >= 0,
                                1000 * ssbymin1, ssbymin1),
    bpa                = ifelse(ssbunits == "kT" & bpa >= 0,
                                1000 * bpa, bpa),
    blim               = ifelse(ssbunits == "kT" & blim >= 0,
                                1000 * blim, blim),
    msybtrigger        = ifelse(ssbunits == "kT" & msybtrigger >= 0,
                                1000 * msybtrigger, msybtrigger),
    ssbunits           = ifelse(ssbunits == "kT",
                                "tonnes", ssbunits)
  )

  # merge with iStock where needed 

write.csv(iAdvice, file="iadvice.csv", row.names=FALSE)

iAdvice %>% 
  filter(tolower(catchlandingsunits) != "kt" ) %>% 
  View()

  




