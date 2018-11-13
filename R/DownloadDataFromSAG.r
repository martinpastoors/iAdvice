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
# install_github("ices-tools-prod/icesSAG@development")

library(icesSAG)
library(tidyverse)
library(stringr)       # for string manipulation

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
assessdir <- paste(get_dropbox(), "/iAdvice", sep="")

# =====================================================================================
# generate list of assessmentkeys
# =====================================================================================

assessmentkeys <- sort(findAssessmentKey())
# getStockDownloadData(7306)

# =====================================================================================
# Download all assessments in SAG in full
# =====================================================================================

# Download all the stock data - takes a long time
t1  <- getStockDownloadData(assessmentkeys)

t2 <- data.frame()

# loop over all assessments to bind them together
for (i in 1:length(t1)) {
  
  print(i)
  if (!is.null(t1[[i]])) {
    tmp <-
      t1[[i]] %>% 
      mutate_all(funs("as.character"))
    
    t2  <- bind_rows(t2, tmp)
  }
}

# Create iSAGdownload with distinct rows
iSAGdownload <- 
  t2 %>% 
  lowcase() %>% 
  
  # keep only distinct rows
  distinct()


# Save file
today <- format(Sys.time(), '%Y%m%d')
save(iSAGdownload, file=paste0(assessdir, "/rdata/iSAGdownload ", today, ".RData"))


# =====================================================================================
# Download all the reference point data
# =====================================================================================

t4 <- getFishStockReferencePoints(assessmentkeys)

iSAGrefpoints <- data.frame()

# loop over all assessments to bind them together
for (i in 1:length(t4)) {
  
  print(i)
  if (!is.null(t4[[i]])) {
    tmp <-
      t4[[i]] %>% 
      mutate_all(funs("as.character"))
    
    iSAGrefpoints  <- bind_rows(iSAGrefpoints, tmp)
  }
}

iSAGrefpoints %>% 
  lowcase() %>% 
  distinct() %>% 
  save(file=paste0(assessdir, "/rdata/iSAGrefpoints ", today, ".RData"))