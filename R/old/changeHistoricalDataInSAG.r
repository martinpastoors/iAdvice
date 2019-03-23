# ============================================================================
# Change Historical Assessment Data in SAG
#
# 21/03/2018 First coding of adding historical data to standardgraphs
# 27/04/2018 Change to Historical in the Purpose Field
# 15/11/2018 Change just the comments of an assessment (trial)
# ============================================================================

rm(list=ls())

# library(devtools)
# devtools::install_github("ices-tools-prod/icesSAG")

library(icesSAG)
library(tidyverse)

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
advicedir <- paste(get_dropbox(), "/iAdvice", sep="")

# Download and mutate the data for stock and year
rby <- getStockDownloadData(9331)[[1]]
rp  <- getFishStockReferencePoints(9331)[[1]]

# Create the input data for uploading  
info     <- stockInfo(StockCode      = unique(rby$StockKeyLabel), 
                      AssessmentYear = unique(rby$AssessmentYear), 
                      ContactPerson  = "martin.pastoors@gmail.com")
# UNFINISHED !!!
t <-
  rby %>% 
  dplyr::select(one_of(names(info))) %>% 
  distinct() %>% 
  bind_cols (rp %>% dplyr::select(one_of(names(info))))

xy.list <- as.list(as.data.frame(t))
xy.list <- split(t, seq(ncol(t)))

                 
                 
info$StockCategory             <- NA
info$MSYBtrigger               <- rp$MSYBtrigger
info$Blim                      <- rp$Blim
info$Bpa                       <- rp$Bpa
info$Flim                      <- rp$FLim
info$Fpa                       <- rp$Fpa
info$FMSY                      <- rp$FMSY
if("Fage"           %in% colnames(rby)) {info$Fage <- unique(rby$Fage)} else {info$Fage <- NA }
if("RecruitmentAge" %in% colnames(rby)) {info$RecruitmentAge <- unique(rby$RecruitmentAge)} else {info$RecruitmentAge <- NA }
info$RecruitmentAge            <- 0
info$CatchesCatchesUnits       <- unique(rby$units[!is.na(rby$units)])
info$RecruitmentDescription    <- "wr"
info$RecruitmentUnits          <- "thousands"
info$FishingPressureDescription<- "F"
info$FishingPressureUnits      <- "Year-1"
info$StockSizeDescription      <- "SSB"
info$StockSizeUnits            <- unique(rby$stockSizeUnits[!is.na(rby$stockSizeUnits)])
info$Purpose                   <- "Advice"
info$ModelName                 <- "SAM"
# info$CustomLimitName1         <- "MBAL"
# info$CustomLimitValue1        <- 800000

fishdata <- stockFishdata(min(rby$Year):max(rby$Year))
fishdata <- rby %>% dplyr::select(one_of(names(fishdata))) 

fishdata$Landings              <- rby$landings
fishdata$Catches               <- rby$catches
# fishdata$Unallocated_Removals  <- rby$unallocatedremovals
fishdata$Discards              <- rby$discards
fishdata$Recruitment           <- rby$recruitment
fishdata$StockSize             <- rby$SSB
fishdata$FishingPressure       <- rby$F

# summary(fishdata)
# glimpse(fishdata)

# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

# now add a comment to the upload of an assessment
setSAGSettingForAStock(AssessmentKey=9354, 
                       chartKey=0,
                       settingKey=21,
                       settingValue="Martin Pastoors Historical data",
                       copyNextYear=FALSE) 


