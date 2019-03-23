# ============================================================================
# UPload Historical Data into SAG
#
# 21/03/2018 First coding of adding historical data to standardgraphs
# 27/04/2018 Change to Historical in the Purpose Field
# 15/11/2018 Change just the comments of an assessment (trial)
# 19/03/2019 Updated for HAWG 2019
# 22/03/2019 Updated after HAWG 2019
# ============================================================================

rm(list=ls())

library(icesSAG)  # devtools::install_github("ices-tools-prod/icesSAG")
library(tidyverse)

# use token
options(icesSAG.use_token = TRUE)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# load the data
load(file=paste(dropboxdir,"/rdata/iAssess.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iAdvice.RData", sep=""))

# Select a stock and year
STK   <- filter(iAssess, stockkeylabelold == "her-47d", assessmentyear == 1983)
STK2  <-
  distinct(STK, stockkeylabelold, assessmentyear, purpose) %>% 
  left_join(iAdvice, by=c("stockkeylabelold", "assessmentyear", "purpose"))

# Set years and ranges
FiY   <- min(STK$year)
LaY   <- max(STK$year)
nyrs  <- ((LaY)-(FiY))+1

# Meta information
stockkeylabel  <- unique(STK$stockkeylabelold)
assessmentyear <- unique(STK$assessmentyear)
contactperson  <- "martin.pastoors@gmail.com"

# ------------------------------------------------------------------------------------------
# Create the input data for uploading  
# ------------------------------------------------------------------------------------------

info     <- stockInfo(
  StockCode      = stockkeylabel, 
  AssessmentYear = assessmentyear, 
  ContactPerson  = contactperson)

info$StockCategory             <- ""
info$MSYBtrigger               <- STK2$msybtrigger
info$Blim                      <- STK2$blim
info$Bpa                       <- STK2$bpa
info$Flim                      <- STK2$flim
info$Fpa                       <- STK2$fpa
info$FMSY                      <- STK2$fmsy
info$Fage                      <- STK2$fage 
info$CustomLimitName1          <- "MBAL"
info$CustomLimitValue1         <- STK2$mbal
info$RecruitmentAge            <- unique(STK$recruitmentage)  
info$CatchesLandingsUnits      <- ifelse(unique(na.omit(STK$catcheslandingsunits))=="tonnes", "t", "")
info$RecruitmentDescription    <- "wr"
info$RecruitmentUnits          <- ifelse(unique(na.omit(STK$unitofrecruitment))=="thousands", "NE3", "") 
info$FishingPressureDescription<- ifelse(tolower(unique(na.omit(STK$fishingpressuredescription)))=="f", "F", "")
info$FishingPressureUnits      <- NA 
info$StockSizeDescription      <- na.omit(unique(STK$stocksizedescription))
info$StockSizeUnits            <- ifelse(unique(na.omit(STK$stocksizeunits)) =="tonnes", "t", "")
info$Purpose                   <- "Historical"
# info$ModelName                 <- toupper(na.omit(STK2$assessmentmodel))
info$ModelType                 <- "A"    # age based

# ------------------------------------------------------------------------------------------
# Create the fish data
# ------------------------------------------------------------------------------------------

fishdata                          <- stockFishdata(FiY:LaY)

fishdata$Catches                  <- STK$catches
fishdata$Landings                 <- STK$landings
fishdata$Low_Recruitment          <- STK$lowrecruitment
fishdata$Recruitment              <- STK$recruitment
fishdata$High_Recruitment         <- STK$highrecruitment 

fishdata$Low_StockSize            <- STK$lowstocksize
fishdata$StockSize                <- STK$stocksize
fishdata$High_StockSize           <- STK$highstocksize

fishdata$Low_FishingPressure      <- STK$lowfishingpressure
fishdata$FishingPressure          <- STK$fishingpressure
fishdata$High_FishingPressure     <- STK$highfishingpressure

# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

# save(info, fishdata, file="testupload.RData")

# Get SAG settings
# getSAGSettingsForAStock(assessmentKey=key) %>% View()
getSAGSettingsForAStock(assessmentKey=10364) %>% View()

# Add comment to SAG settings
setSAGSettingForAStock(assessmentKey=10364, 
                       chartKey=0,
                       settingKey=21,
                       settingValue="Martin Pastoors Historical data",
                       copyNextYear=FALSE) 


