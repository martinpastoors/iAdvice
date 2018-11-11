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
# 10/05/2018 checked the excel spreadsheet; split up database in management and advice parts
# 02/07/2018 updated database + plot of Total Allowable Landings (for Eric R. )
# ===========================================================================

# rm(list=ls())

library(readxl)
library(tidyverse)
library(stringr)
library(pander)
library(directlabels)  # for printing labels at end of geom lines

# Source the utils file
source("../prf/R/my utils.r")

# set dropbox directory
dropboxdir <- paste(get_dropbox(), "/ICES Advice database", sep="")

# ===========================================================================
# Read the data
# ===========================================================================

iAdvice_all <- 
  read_excel(paste0(dropboxdir, "/ICES Scientific Advice database 20180702.xlsx",sep=""), 
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
               "blim","bpa","msybtrig"), funs(as.numeric)) %>% 
  mutate_at( c("stockices","speciestype", "assessmodel", 
               "multispeciesmanagement", "stockmanagement", "adviceonstock", "aggregated"), funs(tolower)) %>% 
  rename(fishstock = stockices, 
         assessmentyear = assessyear) %>% 
  arrange(fishstock, tacarea, year) %>%  
  mutate(faocode    = substr(fishstock,1,3)
         # stockarea_ = strsplit(stockarea, "/"),
         # tacarea_   = strsplit(tacarea,   "/"),
         # intersect  = stockarea_,
         # stockonly  = stockarea_,
         # taconly    = stockarea_ 
         ) 

save(iAdvice_all, file="rdata/iAdvice_all.RData")
# load(file="rdata/iAdvice.RData")
# names(iAdvice_all)

# split up
iAdvice <-
  iAdvice_all %>% 
  select(1,2, 4,6,7,8,9,10,14,15,16, 19:23, 67)

iManage <-
  iAdvice_all %>% 
  select(1,2,3, 4,6,7,8,9,10,11,12,13,14,15,16,17,23:29, 67)

iForecast <-
  iAdvice_all %>% 
  select(1,2, 4,6,7,8,9,10,14,15,17,33:35,66)

iRefpoint <-
  iAdvice_all %>% 
  select(1,2, 4,6,7,8,9,10,14,15,17,37:46,66) %>% 
  mutate_at(c("fmsy", "fpa","flim","bpa","blim","msybtrig"), funs(as.numeric))

# now start checking and filtering
iAdvice %>% 
  # filter(fishstock %in% c("had-3a","had-34","had-346a","had-scow")) %>% 
  filter(faocode %in% c("ple")) %>% 
  filter(adviceonstock == "y") %>% 
  
  ggplot(aes(x=year, y=advisedlandingsmax, group=fishstock)) +
  theme_publication() +
  geom_bar(aes(fill=fishstock), stat="identity")

# and with iManage
iManage %>% 
  group_by(year, fishstock) %>% 
  mutate(nobs = n()) %>% 
  filter(faocode %in% c("ple")) %>% 
  filter(grepl("ple-nsea", fishstock)) %>% 
  filter(stockmanagement == "y") %>% 
  
  ggplot(aes(x=year, y=tal, group=fishstock)) +
  theme_publication() +
  geom_bar(aes(fill=fishstock), stat="identity")

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


# ---------------------------------------------------------------------------
# number of reference points
# ---------------------------------------------------------------------------

iAdvice %>% 
  select(year, fishstock, fpa, flim, bpa, blim) %>% 
  group_by(year) %>% 
  summarize(fpa = sum(!is.na(fpa)),
            flim = sum(!is.na(flim)),
            bpa  = sum(!is.na(bpa)),
            blim = sum(!is.na(blim))) %>% 
  
  ggplot(aes(x=year)) +
  theme_publication() +
  geom_line(aes(y=fpa), colour="blue") +
  geom_line(aes(y=bpa), colour="red") +
  geom_line(aes(y=blim),colour="pink")


# ---------------------------------------------------------------------------
# trend in reference points
# ---------------------------------------------------------------------------

iRefpoint %>% 
  filter(fishstock %in% c("ple-2123", "ple-kask")) %>% 
  select(year, fishstock, fpa, flim, bpa, blim) %>% 
  gather(key=refpoint, value=value, fpa:blim) %>% 
  mutate(reftype = ifelse(refpoint %in% c("fpa","flim"), "f","ssb")) %>% 

  # View()

  ggplot(aes(x=year, y=value)) +
    theme_publication() +
    geom_point(aes(colour=factor(refpoint))) +
    expand_limits(y=0) +
    facet_wrap(~reftype, scales="free_y")

# ---------------------------------------------------------------------------
# trend in TACs
# ---------------------------------------------------------------------------

unique(iManage$fishstock)

iManage %>% 
  filter(fishstock %in% c("hom-west", "hom-nsea", "her-47d3", "mac-nea", "whb-comb")) %>% 
  group_by(fishstock, year, taccode, tacarea) %>% 
  summarize(tal = sum(tal, na.rm=TRUE)) %>% 
  ungroup() %>% 
  data.frame() %>% 
  filter(tal != 0) %>% 
  # View()
  
  ggplot(aes(x=year, y=tal)) +
  theme_publication() +
  theme(
    legend.position = "none"
  ) + 
  geom_point(aes(colour=factor(taccode))) +
  geom_path(aes(colour=factor(taccode))) +
  expand_limits(y=0) +
  facet_wrap(~taccode, scales="free_y")