# ============================================================================
# Change Historical Assessment Data in SAG
#
# 21/03/2018 First coding of adding historical data to standardgraphs
# 27/04/2018 Change to Historical in the Purpose Field
# ============================================================================

rm(list=ls())

# library(devtools)
# devtools::install_github("ices-tools-prod/icesSAG")

library(icesSAG)
library(icesSD)
library(tidyverse)
library(directlabels)  # for printing labels at end of geom lines

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# Download and mutate the data for stock and year
rby <- 
  getSAG("her-47d3",year = 2002) %>% 
  mutate(landings = ifelse(landings == 0, NA, landings))
  

# Create the input data for uploading  
info     <- stockInfo(StockCode      = unique(rby$fishstock), 
                      AssessmentYear = unique(rby$AssessmentYear), 
                      ContactPerson  = "martin.pastoors@gmail.com")

info$StockCategory             <- NA
info$MSYBtrigger               <- NA
info$Blim                      <- 800000
info$Bpa                       <- 1300000
info$Flim                      <- NA
info$Fpa                       <- 0.25 
info$FMSY                      <- NA
info$Fage                      <- "2-6"
info$RecruitmentAge            <- 0
info$CatchesCatchesUnits       <- unique(rby$units[!is.na(rby$units)])
info$RecruitmentDescription    <- "wr"
info$RecruitmentUnits          <- "thousands"
info$FishingPressureDescription<- "F"
info$FishingPressureUnits      <- "Year-1"
info$StockSizeDescription      <- "SSB"
info$StockSizeUnits            <- unique(rby$stockSizeUnits[!is.na(rby$stockSizeUnits)])
info$Purpose                   <- "Advice"
# info$CustomLimitName1         <- "MBAL"
# info$CustomLimitValue1        <- 800000

fishdata <- stockFishdata(min(rby$Year):max(rby$Year))

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


