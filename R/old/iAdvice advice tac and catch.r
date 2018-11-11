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

compare <-
  iAdvice %>% 
  dplyr::select(year, stockkeylabelold, advisedlandingsmax, tal, iceslandings, icescatch) %>% 
  rename(advice=advisedlandingsmax, tac = tal, landings=iceslandings, catch=icescatch) %>% 
  filter(stockkeylabelold %in% c("her-47d3", "mac-nea","whb-comb", "her-noss")) %>% 
  filter(year >= 2000) %>% 
  gather(key=variable, value=value, advice:catch)

# plot of comparison of advice, tac, landings and catch
compare %>% 
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  geom_line(aes(colour=factor(variable), size=variable)) +
  scale_size_manual    (values=c(advice=1.5 ,tac = 1.5, landings = 1.0, catch = 1.0)) +
  expand_limits(y = 0) +
  facet_wrap(~stockkeylabelold, scales="free_y") 

# plot of difference between tac and advice
compare %>% 
  filter(variable %in% c("advice","tac")) %>% 
  spread(key=variable, value=value) %>% 
  mutate(deviance = as.integer(100*(tac/advice-1)) ) %>% 
  filter(!is.na(deviance)) %>% 

  ggplot(aes(x=year, y=deviance)) +
  theme_publication() +
  geom_hline(aes(yintercept=0), colour="black") +
  geom_line(colour="blue", size=1.5) +
  facet_wrap(~stockkeylabelold) 
