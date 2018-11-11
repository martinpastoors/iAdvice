# -----------------------------------------------------------------------------------------------
# iStock.r
#
# Generate iStock, iRename and iStockkey
#
# 23/10/2017 copied code out of iDatabases generator.r
# -----------------------------------------------------------------------------------------------

# ICES Stock database libraries

# library(devtools)
# devtools::install_github("ices-tools-prod/icesSD")
# devtools::install_github("ices-tools-prod/icesSAG")
# library(icesSD)  # ICES Stock database
# library(icesSAG)  # ICES Stock Assessment Graphs

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

# getSD         <- icesSD::getSD()
# getListStocks <- icesSAG::getListStocks(year=0)

# save(getSD        , file=paste(dropboxdir, "/rdata/getSD.RData",sep=""))
# save(getListStocks, file=paste(dropboxdir, "/rdata/getListStocks.RData",sep=""))

# load(file=paste(dropboxdir, "/rdata/getSD.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/getListStocks.RData",sep=""))

# -------------------------------------------------------------------------------------------------
# iStock, iStockkey, iRename
# Generate iStock (by stock, assessment year and date) - information on stock assessment properties
# Note: iStock read from Excel file because substantial amount of information added, e.g. areas
# -------------------------------------------------------------------------------------------------

# read iStock object from excel file
t <- 
  readxl::read_excel(path= paste(get_dropbox(), 
                                 "/ICES assessment database/iStock.xlsx", sep=""), 
                     col_names = TRUE, 
                     col_types = "text", 
                     trim_ws   = TRUE) %>%
  lowcase() %>% 
  mutate_at(c("assessmentmodel","assessmenttype","advicetype", "expertgroup","useofdiscardsinadvice","pabufferapplied", "status"),
            funs(tolower)) %>% 
  mutate_at(c("fmax","f01","fmed","f35spr","flim","fpa","fmsy","blim","bpa","msybtrigger"), 
            funs(as.numeric)) %>% 
  mutate_at(c("stockkey", "assessmentyear", "firstyearofdata", "ncpueseries","nsurveyseries"),
            funs(as.integer)) %>% 
  
  mutate(assessmenttype   = ifelse(grepl("explo",assessmentmodel), "exploratory",assessmenttype),
         assessmenttype   = ifelse(grepl("trends",assessmentmodel), "trends",assessmenttype),
         
         assessmentmodel  = ifelse(grepl("(xsa"   , assessmentmodel, fixed=TRUE), "xsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("sxsa"   , assessmentmodel, fixed=TRUE), "sxsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flxsa" , assessmentmodel, fixed=TRUE), "xsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(ica"   , assessmentmodel, fixed=TRUE), "ica"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flica" , assessmentmodel, fixed=TRUE), "ica"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(sam"   , assessmentmodel, fixed=TRUE), "sam"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flsam" , assessmentmodel, fixed=TRUE), "sam"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(tsa"   , assessmentmodel, fixed=TRUE), "tsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(adapt" , assessmentmodel, fixed=TRUE), "adapt" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(ss3"   , assessmentmodel, fixed=TRUE), "ss3"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(stock synthesis 3", assessmentmodel, fixed=TRUE), "ss3"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(gadget" , assessmentmodel, fixed=TRUE), "gadget", assessmentmodel),
         assessmentmodel  = ifelse(grepl("(asap"  , assessmentmodel, fixed=TRUE), "asap"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(amish"  , assessmentmodel, fixed=TRUE), "amish" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(aspic"  , assessmentmodel, fixed=TRUE), "aspic" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(mycc"   , assessmentmodel, fixed=TRUE), "mycc"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("multi-year catch curve"   , assessmentmodel, fixed=TRUE), "mycc"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(aspic"  , assessmentmodel, fixed=TRUE), "aspic" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("aarts"  , assessmentmodel, fixed=TRUE), "aarts_poos" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(cbbm"  , assessmentmodel, fixed=TRUE), "cbbm" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(scaa"  , assessmentmodel, fixed=TRUE), "scaa" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(sms"  , assessmentmodel, fixed=TRUE), "sms" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(tasacs"  , assessmentmodel, fixed=TRUE), "tasacs" , assessmentmodel),
         # NEED TO FINALIZE THIS LIST !!
         
         datepublished    = as.Date(as.numeric(datepublished), origin="1899-12-30"),
         advicereleasedate= as.Date(as.numeric(advicereleasedate), origin="1899-12-30"),
         modifieddate     = as.Date(as.numeric(modifieddate), origin="1899-12-30"),
         
         assessmenttype2 = ifelse(assessmenttype %in% c("exploratory","trends","trends only"), "trends",assessmenttype)
  ) %>% 
  arrange(stockkey, assessmentyear, stockkeylabel) 

iStock <-
  t %>% 
  select(stockkey, stockkeylabel, 
         assessmentyear, assessmentmodel, stockarea, datepublished,
         expertgroup, advicedraftinggroup, 
         firstyearofdata, ncpueseries, nsurveyseries, 
         assessmenttype2, assessmentcomment, datacategory,
         yearoflastassessment, assessmentfrequency, yearofnextassessment, advicereleasedate,
         advicetype, useofdiscardsinadvice, pabufferapplied, published, status, sectionnumber, 
         assessmentkey, modifieddate,
         fmax, f01, fmed, f35spr, flim, fpa, fmsy, blim, bpa, msybtrigger) 

# filter(iStock, grepl("agn", stockkeylabel)) %>% View()

# iStockkey
iStockkey <-
  t %>% 
  group_by(stockkey) %>% 
  filter(row_number() == 1) %>% 
  select(stockkey, stockkeylabelnew, stockkeylabelold)


# iRename
iRename <-
  t %>% 
  group_by(stockkey, stockkeylabel, stockkeydescription, speciesfaocode=fao) %>% 
  filter(row_number() == 1) %>% 
  select(stockkey, stockkeylabel, stockkeydescription, speciesfaocode) 


save(iStock     , file=paste(dropboxdir, "/rdata/iStock.RData", sep=""))
save(iStockkey  , file=paste(dropboxdir, "/rdata/iStockkey.RData", sep=""))
save(iRename    , file=paste(dropboxdir, "/rdata/iRename.RData", sep=""))

