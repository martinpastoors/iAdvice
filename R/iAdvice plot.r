# ===========================================================================
# iAdvice plot.r

# 06/09/2016 adapted after WGWIDE
# 18/09/2016 prepared for ICES ASC talk with Inigo and David
# 23/09/2016 After adapting the horse mackerel data; new way of aggregateAt
# 05/01/2017 After revision of database structure and logic
# 27/06/2017 Read excel and save rdata file
# 01/08/2017 add all variables to the rdata file; moved to read_excel
# 11/08/2017 adapted for R 3.4.1 and tidyverse
# 04/10/2017 ported to github site
# ===========================================================================

# rm(list=ls())

library(readxl)
library(tidyverse)
library(stringr)
library(pander)
library(directlabels)  # for printing labels at end of geom lines


# Source the utils file
source("../mptools/R/my_utils.r")

# load gis data
load("../prf/rdata/world.europe.df.RData")
load("../prf/rdata/fao.df.RData")
load("../prf/rdata/fao27.df.RData")
load("../prf/rdata/eez.europe.df.RData")

# Set working directory
dropboxdir <- paste(gsub("\\","/",get_dropbox(), fixed=TRUE), "/ICES Advice database", sep="")



# ===========================================================================
# Read the data
# ===========================================================================

iAdvice <- 
  read_excel(paste0(dropboxdir, "/ICES Scientific Advice database.xlsx",sep=""), 
             sheet     = "DATA",
             col_names = TRUE,
             col_types = "text",
             skip      = 0)  %>% 
  lowcase() %>% 
  mutate_at( c("year", "assessyear", "ncpueseries","nsurveyseries"), 
             funs(as.integer)) %>% 
  mutate_at( c("advisedlandings", "advisedcatch","advisedlandingsmax","advisedcatchmax","tal","tac",
               "officiallandings","iceslandings","icesindustrialbycatch","icesdiscards","icescatch",
               "fsqymin1","ssbymin1","fadvmax","fmax","f01","fmed","f35spr","flim","fpa","fmsy",
               "blim","bpa","msybtrig"), 
            funs(as.numeric)) %>% 
  mutate_at( c("stockices","speciestype", "assessmodel"), 
             funs(tolower)) %>% 
  rename(fishstock = stockices, 
         assessmentyear = assessyear) %>% 
  mutate(faocode    = substr(fishstock,1,3),
         stockarea_ = strsplit(stockarea, "/"),
         tacarea_   = strsplit(tacarea,   "/"),
         intersect  = stockarea_,
         stockonly  = stockarea_,
         taconly    = stockarea_ ) %>% 
  arrange(fishstock, tacarea, year) 

save(iAdvice, file="rdata/iAdvice.RData")
# load(file="rdata/iAdvice.RData")

# ---------------------------------------------------------------------------
# overview of advice this year compared to previous year
# ---------------------------------------------------------------------------

iAdvice %>% 
  filter(fishstock ==   "mac-nea", 
         year  %in% c(2005:2018)) %>% 
  select(year, fishstock, advisedlandingsmax,  
         fsqymin1, ssbymin1, fadvmax) %>% 
  gather(key=variable, value=value, advisedlandingsmax:fadvmax) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(panel.border = element_rect(colour="gray" , size=0.2)) +
  geom_bar(aes(fill=year), stat = "identity") +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL) +
  facet_wrap( ~ variable, scales="free_y")


# ---------------------------------------------------------------------------
# overview of advice in %change between years
# ---------------------------------------------------------------------------


  iAdvice %>% 
  filter(fishstock ==   "mac-nea", 
         year  %in% c(2005:2018)) %>% 
  select(year, fishstock, advisedlandingsmax) %>% 
  mutate(lagged = lag(advisedlandingsmax),
         change = as.integer(100*(advisedlandingsmax/lagged - 1)), 
         change2 = paste0(as.integer(100*(advisedlandingsmax/lagged - 1)), "%")) %>% 
  
  ggplot(aes(x=year, y=change)) +
  theme_publication() +
  geom_bar(stat="identity") +
  geom_dl(aes(label  = change2), 
          method = list(list("top.bumptwice", dl.trans(y = y + sign(y) * 0.2))))




