### ============================================================================
### Add Historical Assessment Data to SAG
###
### 21/03/2018 First coding of adding historical data to standardgraphs
### ============================================================================

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

# =====================================================================================
# Download ICES Stock database
# =====================================================================================

icesSD         <- icesSD::getSD()
save(icesSD, file=paste(dropboxdir, "/rdata/icesSD.RData",sep=""))

# =====================================================================================
# Download all the assessments in SAG; also unpublished ones
# =====================================================================================


icesSAG  <-
  getSAG(stock=NULL, year = 0, data="summary", combine=TRUE) %>% 
  lowcase()
  # filter(Purpose == "Advice")
save(icesSAG, file=file.path(dropboxdir, "rdata/icesSAG 20180427.RData"))

sort(unique(icesSAG$Purpose))

# create overview of assessments
icesSAG %>% 
  group_by(fishstock, assessmentyear, purpose) %>% 
  filter(row_number() == 1) %>% 
  group_by(assessmentyear) %>% 
  summarize(n = n()) %>% 
  View()

# check the historical data
icesSAG %>% 
  filter(assessmentyear == 1989) %>% 
  View()

# plot specific assessment
icesSAG %>% 
  filter(year >= 1980) %>%
  filter(fishstock %in% c("cod-347d")) %>%
  mutate(decade            = 10*floor(assessmentyear/10),
         tyear             = substr(as.character(assessmentyear),3,4)) %>% 
  
  ggplot(aes(x=year, y=ssb, group=assessmentyear)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(colour=factor(tyear))) +
  expand_limits(y=0) +
  geom_dl(aes(label  = tyear, colour = tyear),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  facet_grid(fishstock~decade, scales="free_y") +
  labs(title="SSB") 

# plot one assessment
icesSAG %>% 
  filter(fishstock %in% c("hom.27.2a4a5b6a7a-ce-k8")) %>% 
  filter(AssessmentYear == 2017) %>% 
  # filter(!Year %in% c(1982, 2001)) %>% 
  ungroup() %>% 
  summarize(meanr = exp(mean(log(recruitment), na.rm=TRUE)))


# =====================================================================================
# open the QCS and Excel database
# =====================================================================================

d <-
  readxl::read_excel(path= paste(dropboxdir, "/data/QCS and EXCEL Assessment Database combined.xlsx", sep=""), 
                     sheet = "data",
                     col_names = TRUE, 
                     col_types = "text", 
                     trim_ws   = FALSE) %>%
  lowcase() %>% 
  mutate_at(c("assessmentyear","year", "recruitment","ssb","f", "catches"),   funs(as.numeric)) %>% 
  mutate_at(c("stockkey"),    funs(as.integer)) %>% 
  mutate_at(c("unitofrecruitment", "recruitmentdescription"),    funs(tolower)) %>% 
  mutate(recruitment       = ifelse(unitofrecruitment == "thousands", recruitment/1000, recruitment),
         unitofrecruitment = ifelse(unitofrecruitment == "thousands", "millions", unitofrecruitment),
         recruitment       = ifelse(unitofrecruitment == "billions", recruitment*1000, recruitment),
         unitofrecruitment = ifelse(unitofrecruitment == "billions", "millions", unitofrecruitment) ) 


d %>% 
  filter(year >= 1980) %>% 
  filter(assessmentyear >= 1990) %>% 
  rename(fishstock=stockkeylabelold) %>% 
  filter(fishstock %in% c("her-47d3","her-47d")) %>% 
  #  filter(AssessmentYear < 2000) %>% 
  mutate(decade            = 10*floor(assessmentyear/10),
         tyear             = substr(as.character(assessmentyear),3,4)) %>% 
  dplyr::select(fishstock, assessmentyear, year, tyear, decade, recruitment, ssb, f, landings, catches) %>% 
  gather(key=variable, value=value, recruitment:landings) %>% 
  mutate(variable = factor(variable, levels = c("landings","recruitment","ssb","f")),
         value=as.numeric(value)) %>% 

  filter(variable == "ssb") %>% 
  # glimpse()
  # View()

  ggplot(aes(x=year, y=value)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(colour=tyear)) +
  geom_dl(aes(label  = tyear, colour = tyear),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y=0) +
  labs(title="NSH overview") +
  facet_wrap(~decade) 
# facet_grid(variable~decade, scales="free_y") 
# facet_grid(variable~., scales="free_y") 

# NSHdata %>% distinct(AssessmentYear) %>% arrange(AssessmentYear) %>% View()

# plot recent two assessments for north sea herring ; TEMPORARY FIXES; need to include reference points
d %>% 
  filter(year >= 2000) %>% 
  filter(assessmentyear %in% 2017:2018) %>%
  filter(assessmenttype == "assess") %>% 
  rename(fishstock=stockkeylabelold) %>% 
  filter(fishstock %in% c("her-47d3","her-47d")) %>% 
  
  mutate(fmsy = ifelse(assessmentyear == 2017, 0.32, as.numeric(NA)),
         fmsy = ifelse(assessmentyear == 2018, 0.26, fmsy)) %>% 
  
  mutate(decade            = 10*floor(assessmentyear/10),
         tyear             = substr(as.character(assessmentyear),3,4)) %>% 
  
  dplyr::select(fishstock, assessmentyear, year, tyear, decade, recruitment, ssb, f, landings, catches, fmsy) %>% 
  gather(key=variable, value=value, recruitment:landings) %>% 
  mutate(variable = factor(variable, levels = c("landings","recruitment","ssb","f")),
         value=as.numeric(value)) %>% 
  
  filter(variable == "f") %>% 
  # glimpse()
  # View()
  
  ggplot(aes(x=year, y=value)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(colour=tyear)) +
  geom_hline(aes(yintercept=fmsy, colour=tyear)) +
  geom_dl(aes(label  = tyear, colour = tyear),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  geom_text   (aes(x=2000, y=fmsy+0.01, label="fmsy", colour=tyear),
               size=3, hjust=0.0, vjust=0.0) +
  
  expand_limits(y=0) +
  labs(title="") +
  facet_wrap(~assessmentyear) 


# Upload stock assessment data to SAG database


info     <- 
  stockInfo(StockCode="cod-347d", AssessmentYear = 1996, ContactPerson = "martin.pastoors@gmail.com") 


x <-
  d %>% 
  filter(
    stockkeylabelold == info$StockCode,
    assessmentyear   == info$AssessmentYear)



info$StockCategory             <- NA
info$MSYBtrigger               <- NA
info$Blim                      <- NA
info$Bpa                       <- NA
info$Flim                      <- NA
info$Fpa                       <- NA 
info$FMSY                      <- NA
info$Fage                      <- "2-8"
info$RecruitmentAge            <- 1
info$CatchesCatchesUnits       <- unique(x$catcheslandingsunits[!is.na(x$catcheslandingsunits)])
info$RecruitmentDescription    <- "Recruitment"
info$RecruitmentUnits          <- unique(x$unitofrecruitment[!is.na(x$unitofrecruitment)])
info$FishingPressureDescription<- "F"
info$FishingPressureUnits      <- "Year-1"
info$StockSizeDescription      <- "SSB"
info$StockSizeUnits            <- unique(x$stocksizeunits[!is.na(x$stocksizeunits)])
info$Purpose                   <- "Historical"
info$CustomLimitName1         <- "MBAL"
info$CustomLimitValue1        <- 150000

# fishdata

FiY       <- min(x$year) 
LaY       <- max(x$year)

fishdata  <- stockFishdata(FiY:LaY)

fishdata$Landings              <- x$landings
fishdata$Catches               <- x$catches
fishdata$Unallocated_Removals  <- x$unallocatedremovals
fishdata$Discards              <- x$discards
fishdata$Recruitment           <- x$recruitment
fishdata$StockSize             <- x$ssb
fishdata$FishingPressure       <- x$f

# summary(fishdata)
# glimpse(fishdata)

# upload to SAG
key <- icesSAG::uploadStock(info, fishdata)

# now add a comment to the upload of an assessment
setSAGSettingForAStock(AssessmentKey=9564, 
                       chartKey=0,
                       settingKey=21,
                       settingValue="Martin Pastoors Historical data",
                       copyNextYear=FALSE) 



# test
getSAG(stock=NULL, year = 1989)  
  

