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

# icesSD         <- icesSD::getSD()
# save(icesSD, file=paste(dropboxdir, "/rdata/icesSD.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/icesSD.RData",sep=""))

# get all the sandeel stocks
filter(icesSD, substr(StockKeyLabel,1,3) == "san") %>% 
  distinct(StockKeyLabel)

# =====================================================================================
# Download all the assessments in SAG; also unpublished ones
# =====================================================================================

# set all the hawgstocks
hawgstocks <- c("her.27.3a47d", "her.27.nirs","her.27.irls","her.27.20-24", "her.27.6a7bc",
                "spr.27.4","spr.27.3a","spr.27.7de",
                "san.sa.1r","san.sa.2r","san.sa.3r","san.sa.4", "san.sa.5r", "san.sa.6", "san.sa.7r")

# download all the stocks in HAWG 2018
hawgdata <- 
  getSAG(hawgstocks, year = 2018) %>% 
  filter(Purpose == "Advice")

save(hawgdata, file="hawgdata.RData")

# Catch or landings
hawgdata %>% 
  mutate(catches = ifelse(is.na(catches), landings, catches)) %>% 
  
  ggplot(aes(x=Year, y=catches)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: Landings or catches")

# SSB
hawgdata %>% 
  filter(Year >= 1980) %>% 
  
  ggplot(aes(x=Year, y=SSB)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_ribbon(aes(ymin=low_SSB, ymax=high_SSB, fill=substr(fishstock,1,3)), alpha=0.3) +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: SSB")

# F
hawgdata %>% 
  filter(Year >= 1980) %>% 
  
  ggplot(aes(x=Year, y=F)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_ribbon(aes(ymin=low_F, ymax=high_F, fill=substr(fishstock,1,3)), alpha=0.3) +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: Fishing mortality")

# recruitment
hawgdata %>% 
  filter(Year >= 1980) %>% 
  
  ggplot(aes(x=Year, y=recruitment)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_ribbon(aes(ymin=low_recruitment, ymax=high_recruitment, fill=substr(fishstock,1,3)), alpha=0.3) +
  geom_line(aes(colour=substr(fishstock,1,3))) +
  facet_wrap(~fishstock, scales="free_y") +
  expand_limits(y=0) +
  labs(title="HAWG 2018: recruitment")



