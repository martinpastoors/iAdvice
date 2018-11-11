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
assessdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# =====================================================================================
# Download all assessments in SAG in full
# =====================================================================================

assessmentkeys <- sort(findAssessmentKey())
# getStockDownloadData(7306)

# Download all the stock data - takes a long time
t1  <- getStockDownloadData(assessmentkeys)

t2 <- data.frame()
for (i in 1:length(t1)) {
  
  print(i)
  if (!is.null(t1[[i]])) {
    tmp <-
      t1[[i]] %>% 
      mutate_all(funs("as.character"))
    
    t2  <- bind_rows(t2, tmp)
  }
}

iSAGdownload <- 
  t2 %>% 
  lowcase()

today <- format(Sys.time(), '%Y%m%d')
save(iSAGdownload, file=paste0(assessdir, "/rdata/iSAGdownload ", today, ".RData"))

