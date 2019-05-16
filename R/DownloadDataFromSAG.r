# ============================================================================
# Download Assessment Data from SAG
#
# 03/07/2018 First coding of downloading data from SAG; adapted from addHistoricaldata...
# 12/08/2018 Included raw downloads (all variables)
# 17/08/2018 Update icesSAG@development because of errors in previous download
# 23/10/2018 Now only download from SAG; do manipulations in iDatabases generator
# ============================================================================

# rm(list=ls())

# library(devtools)
# devtools::install_github("ices-tools-prod/icesSAG")
# devtools::install_github("ices-tools-prod/icesSD")
# devtools::install_github("ices-tools-prod/icesSAG@development")

library(icesSAG)
library(tidyverse)
library(stringr)       # for string manipulation

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load old SAG download data
iSAGstock_astext_complete     <- get(load(file=paste(dropboxdir,"/rdata/iSAGstock_astext_complete.RData", sep=""))) 
# save(iSAGstock_astext_complete, file=paste0(dropboxdir, "/rdata/iSAGstock_astext_complete", ".RData"))

# iSAGrefpoints_astext_complete <- get(load(file=paste(dropboxdir,"/rdata/iSAGrefpoints_astext_complete.RData", sep="")))
iRename                       <- get(load(file=paste(dropboxdir, "/rdata/iRename.RData",sep="")))
iStockkey                     <- get(load(file=paste(dropboxdir, "/rdata/iStockkey.RData",sep="")))

# set date
today <- format(Sys.time(), '%Y%m%d')

# =====================================================================================
# which year (set year = 0 for all years)
# =====================================================================================

myyear <- 2019

# =====================================================================================
# generate list of assessmentkeys
# =====================================================================================

assessmentkeys <- sort(findAssessmentKey(year=myyear))

# assessmentkeys <- findAssessmentKey(year=1983, full=TRUE)
# assessmentkeys <- sort(findAssessmentKey())
# assessmentkeys[assessmentkeys == 10362]
# assessmentkeys <- findAssessmentKey(year=2019, full=TRUE)
# getStockDownloadData(7306)

# =====================================================================================
# Download assessments from SAG
# =====================================================================================

# Download the stock data; use lapply on the assessmentkeys with method getStockDownloadData and then bind the rows. Does not work because of type mismatches for variables
# t1 <-
#   do.call(bind.rows, lapply(assessmentkeys, getStockDownloadData)) %>%
#   lowcase() %>%
#   dplyr::select(-starts_with('custom')) %>%
#   distinct()


# download all data from assessment keys in a list object
t1  <- getStockDownloadData(assessmentkeys)
# sort(names(t1[[12]]))

# loop over all assessments to bind them together
t2 <- filter(iSAGstock_astext_complete, stockkey == 0) # start with empty data frame with all columns
for (i in 1:length(t1)) {
  
  if (class(t1[[i]]) == "data.frame") {
    
    print(paste(i,t1[[i]]$AssessmentKey[1],t1[[i]]$StockKeyLabel[1], t1[[i]]$AssessmentYear[1], sep=", "))
    
    tmp <-
      t1[[i]] %>% 
      mutate_all(funs("as.character")) %>% 
      lowcase() 
    
    t2  <- bind_rows(t2, tmp)
  }
}

# Create iSAGdownload as text with distinct rows
iSAGstock_astext <- 
  t2 %>%
  ungroup() %>% 
  distinct()

# Save dataset to file
save(iSAGstock_astext, file=paste0(dropboxdir, "/rdata/iSAGstock_astext ", myyear," ", today, ".RData"))
# save(iSAGstock_astext, file=paste0(dropboxdir, "/rdata/iSAGstock_astext_complete", ".RData"))


# Remove the downloaded stocks from the original dataset and add the new downloads
iSAGstock_astext_complete <-
  iSAGstock_astext_complete %>%
  filter(assessmentyear != as.character(myyear)) %>%
  bind_rows(iSAGstock_astext)

save(iSAGstock_astext_complete, file=paste0(dropboxdir, "/rdata/iSAGstock_astext_complete", ".RData"))



# Convert to dataset with appropriate field types
iSAGstock <-
  iSAGstock_astext_complete %>% 
  
  # rename(catcheslandingsunits = catchesladingsunits) %>% 
  
  # make numeric
  mutate_at(c("recruitment","lowrecruitment","highrecruitment",  
              "tbiomass","lowtbiomass","hightbiomass", 
              "stocksize", "lowstocksize","highstocksize", 
              "catches", "landings","discards","ibc","unallocatedremovals",
              "fishingpressure", "lowfishingpressure","highfishingpressure",
              "fdiscards","flandings","fibc","funallocated",
              "fpa","bpa", "flim", "blim", "fmsy", "msybtrigger",
              "custom1", "custom2", "custom3", "custom4", "custom5"), 
            funs(as.numeric)) %>% 
  
  # make integer
  mutate_at(c("year", "assessmentyear", "recruitmentage","stockdatabaseid"), funs(as.integer)) %>%
  
  # make logical
  mutate_at(c("published"),  funs(as.logical)) %>% 
  
  # make lowercase
  mutate_at(c("purpose", "stockkeylabel", "unitofrecruitment", "stocksizeunits", "stocksizedescription",
              "catcheslandingsunits", "fishingpressureunits",
              "customname1", "customname2", "customname3","customname4","customname5"), 
            funs(tolower)) %>% 
  
  # change -alt for stock assessments to purpose "alternative"
  mutate(purpose       = ifelse(grepl("\\-alt", stockkeylabel), "alternative", purpose), 
         stockkeylabel = ifelse(grepl("\\-alt", stockkeylabel), gsub("\\-alt","",stockkeylabel), stockkeylabel)) %>% 
  
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
  
  # keep only the most recent assessment (in case of more than one assessment)
  group_by(stockkey, stockkeylabel, assessmentyear, purpose) %>% 
  mutate(assessmentkey = as.numeric(assessmentkey)) %>% 
  filter(assessmentkey == max(assessmentkey, na.rm=TRUE) ) %>% 
  
  # now do the stockkey transformations
  dplyr::select(-stockkey, -icesareas) %>% 
  left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  left_join(iStockkey, by="stockkey") %>% 
  
  # remove duplicate assessments (Careful!)
  # group_by(stockkey, stockkeylabel, assessmentyear, purpose, year) %>% 
  # filter(row_number() == 1) %>% 
  ungroup() 
  

# Save dataset to file
save(iSAGstock, file=paste0(dropboxdir, "/rdata/iSAGstock", ".RData"))



# =====================================================================================
# Download all the reference point data
# =====================================================================================

# t4 <- 
#   do.call(bind_rows, lapply(assessmentkeys, getFishStockReferencePoints)) %>% 
#   lowcase() %>% 
#   distinct()
# 
# iSAGrefpoints <- t4
# 
# 
# save(iSAGrefpoints, file=paste0(dropboxdir, "/rdata/iSAGrefpoints ", today, ".RData"))