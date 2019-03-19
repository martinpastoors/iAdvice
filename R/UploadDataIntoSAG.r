# ============================================================================
# UPload Assessment Data in SAG
#
# 21/03/2018 First coding of adding historical data to standardgraphs
# 27/04/2018 Change to Historical in the Purpose Field
# 15/11/2018 Change just the comments of an assessment (trial)
# 19/03/2019 Updated for HAWG 2019
# ============================================================================

rm(list=ls())

library(icesSAG)  # devtools::install_github("ices-tools-prod/icesSAG")
library(tidyverse)

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
advicedir <- paste(get_dropbox(), "/iAdvice", sep="")

# Get the assessment data and convert to dataframe
load("//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/HAWG/2019 Meeting Docs/06. Data/NSAS/NSH_HAWG2019_sf.Rdata")

# rename the FLStock and FLSAM objects
STK     <- NSH
STK.sam <- NSH.sam

# Set years and ranges
FiY   <- as.numeric(min(dimnames(STK)$year))
DtY   <- as.numeric(max(dimnames(STK)$year))
LaY   <- as.numeric(max(dimnames(STK.sam)$year))
nyrs  <- ((DtY)-(FiY))+1
nyrs2 <- ((LaY)-(FiY))+1

# Meta information
stockkeylabel  <- "her.27.3a47d"
assessmentyear <- 2019
contactperson  <- "benoit.berges@wur.nl"

# Create the input data for uploading  
info     <- stockInfo(
  StockCode      = stockkeylabel, 
  AssessmentYear = assessmentyear, 
  ContactPerson  = contactperson)

info$StockCategory             <- "1"
info$MSYBtrigger               <- 1400000
info$Blim                      <- 800000
info$Bpa                       <- 900000
info$Flim                      <- 0.34
info$Fpa                       <- 0.30
info$FMSY                      <- 0.26
info$Fage                      <- "2-6" 
info$RecruitmentAge            <- 0
info$CatchesCatchesUnits       <- "t"
info$RecruitmentDescription    <- "wr"
info$RecruitmentUnits          <- "NE3" 
info$FishingPressureDescription<- "F"
info$FishingPressureUnits      <- NA 
info$StockSizeDescription      <- "SSB"
info$StockSizeUnits            <- "t"
info$Purpose                   <- "Advice"
info$CustomSeriesName1         <- "model catch"
info$CustomSeriesName2         <- "model catch low"
info$CustomSeriesName3         <- "model catch high"
info$CustomSeriesName4         <- "F0-1"
info$CustomSeriesName5         <- "F7-8"
info$CustomSeriesUnits1        <- "t"
info$CustomSeriesUnits2        <- "t"
info$CustomSeriesUnits3        <- "t"
info$CustomSeriesUnits4        <- NA
info$CustomSeriesUnits5        <- NA

# Create the fish data
fishdata                          <- stockFishdata(FiY:LaY)

fishdata$Catches[1:nyrs]          <- an(STK@landings)[1:nyrs]

fishdata$Low_Recruitment[1:nyrs]  <- rec(STK.sam)$lbnd[1:nyrs]
fishdata$Recruitment              <- rec(STK.sam)$value
fishdata$High_Recruitment[1:nyrs] <- rec(STK.sam)$ubnd[1:nyrs] 

fishdata$Low_StockSize[1:nyrs]    <- ssb(STK.sam)$lbnd[1:nyrs]
fishdata$StockSize                <- ssb(STK.sam)$value
fishdata$High_StockSize[1:nyrs]   <- ssb(STK.sam)$ubnd[1:nyrs]

fishdata$Low_TBiomass[1:nyrs]     <- tsb(STK.sam)$lbnd[1:nyrs]
fishdata$TBiomass                 <- tsb(STK.sam)$value
fishdata$High_TBiomass[1:nyrs]    <- tsb(STK.sam)$ubnd[1:nyrs]

fishdata$Low_FishingPressure[1:nyrs] <- fbar(STK.sam)$lbnd[1:nyrs]
fishdata$FishingPressure             <- fbar(STK.sam)$value
fishdata$FishingPressure[1:nyrs]     <- fbar(STK.sam)$ubnd[1:nyrs]

fishdata$CustomSeries1[1:nyrs]    <- catch(STK.sam)$value[1:nyrs]
fishdata$CustomSeries2[1:nyrs]    <- catch(STK.sam)$lbnd[1:nyrs]
fishdata$CustomSeries3[1:nyrs]    <- catch(STK.sam)$ubnd[1:nyrs]

fishdata$CustomSeries4[1:nyrs]    <- c(quantMeans(harvest(NSH.sam)[ac(0:1),]))[1:nyrs]
fishdata$CustomSeries5[1:nyrs]    <- c(quantMeans(harvest(NSH.sam)[ac(7:8),]))[1:nyrs]

# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

