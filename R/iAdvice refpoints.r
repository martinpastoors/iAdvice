# ===========================================================================
# iAdvice refpoints.r
#
# 10/10/2018 initial coding
# ===========================================================================

# rm(list=ls())

library(readxl)
library(tidyverse)
library(stringr)
library(pander)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Source the utils file
source("../mptools/R/my_utils.r")

# set dropboxdirs
assessdir <- paste(get_dropbox(), "/ICES assessment database", sep="")
advicedir <- paste(get_dropbox(), "/ICES advice database", sep="")


load(file=paste(assessdir, "/rdata/iStockkey.RData", sep=""))
load(file=paste(assessdir, "/rdata/iRename.RData", sep=""))

iAdvice <- 
  read_excel(paste0(advicedir, "/ICES Scientific Advice database 20181001.xlsx",sep=""), 
             sheet     = "DATA",
             col_names = TRUE,
             col_types = "text",
             skip      = 0)  %>% 
  lowcase() %>% 
  mutate_at( c("year", "assessyear", "ncpueseries","nsurveyseries"), 
             funs(as.integer)) %>% 
  mutate_at( c("advisedlandingsmax","advisedcatchmax","tal","tac",
               "officiallandings","iceslandings","icesindustrialbycatch","icesdiscards","icescatch",
               "fsqymin1","ssbymin1","fadvmax","fmax","f01","fmed","f35spr","flim","fpa","fmsy",
               "blim","bpa","msybtrig"), funs(as.numeric)) %>% 
  mutate( blim     = 1000 * blim, 
          msybtrig = 1000 * msybtrig) %>% 
  
  mutate_at( c("stockices","speciestype", "assessmodel", 
               "multispeciesmanagement", "stockmanagement", "adviceonstock", 
               "aggregated"), funs(tolower)) %>% 
  rename(stockkeylabelold = stockices, 
         assessmentyear = assessyear) %>% 
  arrange(stockkeylabelold, tacarea, year) %>%  
  mutate(species    = substr(stockkeylabelold,1,3) ) %>% 
  left_join(iStockkey, by="stockkeylabelold") 





# ---------------------------------------------------------------------------
# overview of %change in reference points
# ---------------------------------------------------------------------------

ref <-
  iAdvice %>% 
  dplyr::select(year, stockkeylabelold, flim, fpa, fmsy, blim, bpa, msybtrig) %>% 
  gather(key=variable, value=value, flim:msybtrig) %>% 
  filter(!is.na(value)) %>% 
  group_by(stockkeylabelold, variable) %>% 
  mutate(lagged = lag(value),
         change = (value/lagged - 1),
         abschange = abs(change)) %>%
#   filter(!is.na(change)) %>% 
  
  group_by(variable, year) %>% 
  summarize(
    n =n(),
    nchange= sum(abschange != 0), 
    meanchange=mean(abschange, na.rm=TRUE)) 
# %>%  View()

filter(iAdvice, !is.na(fpa)) %>% group_by(stockkeylabelold) %>% filter(row_number() == 1) %>% select(stockkeylabelold) %>% View()
filter(ref, variable == "fpa") %>% View()

# Number of reference points
ref %>% 
  ggplot(aes(x=year, y=n)) +
  theme_publication() +
  geom_bar(stat="identity") +
  facet_wrap(~variable)



