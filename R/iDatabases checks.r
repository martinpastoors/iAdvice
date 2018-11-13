# -----------------------------------------------------------------------------------------------
# iDatabases checks
#
# 10/08/2017 created code from ICES SAG download.r
# 25/09/2017 cleaned up; only checking code left
# 23/10/2018 comparing iAdvice and iStock
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(directlabels)  # for printing labels at end of geom lines

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
assessdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")
advicedir  <- paste(get_dropbox(), "/ICES Advice database", sep="")

# load(file=paste(assessdir, "/rdata/iStock.RData",sep=""))
# load(file=paste(assessdir, "/rdata/qcsexcel.RData",sep=""))

# load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))

# -----------------------------------------------------------------------------------------
# Compare iStock and iAdvice 
# -----------------------------------------------------------------------------------------

t <- 
  iAdvice %>% 
  dplyr::select(assessmentyear, tacyear, stockkey, stockarea, stocksubarea) %>% 
  left_join(dplyr::select(iStock, 
                          assessmentyear, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, stockarea), 
            by = c("assessmentyear","stockkey"))

# -----------------------------------------------------------------------------------------
# Trial for anglerfish  
# -----------------------------------------------------------------------------------------

# t <- 
#   iAlloc %>% 
#   filter(!is.na(stockkey)) %>% 
#   group_by(stockkeyscombined) %>% 
#   filter(row_number() ==1) %>% 
#   dplyr::select(stockkeyscombined) %>% 
#   ungroup() %>% 
#   mutate(stockkeyscombined = str_split(stockkeyscombined, pattern="~")) %>% 
#   unnest(stockkeyscombined) %>%
#   rename(stockkeylabelold = stockkeyscombined) %>% 
#   left_join(iAdvice, by=c("stockkeylabelold")) 

filter(t, grepl("her-4", stockkeylabelold), tacyear == 1988) %>% View()
filter(t, tacyear == 1992) %>% View()

iAdvice %>% 
  filter(grepl("her-4", stockkeylabelold)) %>% 
  # filter(stockkeylabelold=="mac-west") %>% 
  # filter(!is.na(tacarea)) %>% 
  filter(adviceonstock == "Y") %>% 
  # filter(speciesfaocode == "mac") %>% 

  dplyr::select(year=tacyear, tacarea, advicemax=advisedlandingsmax, tac=tal, catches) %>% 
  gather(key=var, value=tonnes, advicemax:catches) %>% 
  # dplyr::select(year=tacyear, tacarea, advicemin = advisedlandingsmin, advicemax=advisedlandingsmax, tac=tal, landings) %>% 
  # gather(key=var, value=tonnes, advicemin:landings) %>% 
  
  filter(!is.na(tonnes)) %>% 
  
  # filter(year == 1981) %>%
  # View()

  group_by(var, year) %>% 
  summarize(tonnes = sum(tonnes)) %>%
  
  ungroup() %>% 

  ggplot(aes(x=year, y=tonnes, group=var)) +
  theme_publication() +
  geom_path(aes(colour=factor(var)), size=1) +
  geom_point(aes(colour=factor(var)), size=1.5) +
  expand_limits(y=0)
  # +
  # facet_wrap(~tacarea)
  


# -----------------------------------------------------------------------------------------
# Testing consistency with units of advice, TAC and catch
# -----------------------------------------------------------------------------------------

iAdvice %>% 
  mutate(
    catch        = max(officiallandings, landings, catches, na.rm=TRUE),
    tac_advice   = ifelse(advisedlandingsmax>0, tal/advisedlandingsmax, NA),
    catch_tac    = ifelse(tal>0, catch/tal, NA)
  ) %>% 
  # group_by(stockkeylabelold, tacyear) %>% 
  filter(!stockkeylabelold %in% c("rng-kask")) %>% 
  
  ggplot(aes(tacyear)) +
  theme_publication() +
  geom_point(aes(y=tac_advice), colour="red") +
  geom_point(aes(y=catch_tac), colour="blue") +
  facet_wrap(~stockkeylabelold)

iAdvice %>% 
  ungroup() %>% 
  mutate(
    catch        = pmax(officiallandings, iceslandings, icescatch, na.rm=TRUE), 
    tac_advice   = ifelse(advisedlandingsmax>0, tal/advisedlandingsmax, NA),
    catch_tac    = ifelse(tal>0, catch/tal, NA)
  ) %>% 
  # group_by(stockkeylabelold, tacyear) %>% 
  filter(!stockices %in% c("rng-kask", "bsf-oth", "nep-8c", "rng-oth")) %>%
  filter(!grepl("lin", stockices)) %>% 
  
  filter(grepl("ple", stockices)) %>% 

  ggplot(aes(year)) +
  theme_publication() +
  geom_point(aes(y=tac_advice), colour="red") +
  geom_point(aes(y=catch_tac), colour="blue") +
  facet_wrap(~stockices)


# -----------------------------------------------------------------------------------------
# Testing the handling of advice ranges 
# -----------------------------------------------------------------------------------------

iAdvice %>% 
  # filter(grepl("\\[",advisedcatch)) %>%
  mutate(advisedcatch = gsub("\\[|\\]","",advisedcatch)) %>% 
  select(-advisedcatchmax) %>% 
  separate(advisedcatch, into=c("advisedcatchmin","advisedcatchmax"), sep="-") %>% 
  mutate(advisedcatchmax = ifelse(is.na(advisedcatchmax), advisedcatchmin, advisedcatchmax)) %>% 
  mutate(advisedcatchmin = ifelse(advisedcatchmin == advisedcatchmax, NA, advisedcatchmin)) %>% 
  filter(!is.na(advisedcatchmax)) %>% 
  View()



# -----------------------------------------------------------------------------------------
# Overview of assessment data
# -----------------------------------------------------------------------------------------

qcsexcel %>% 
  filter(grepl("mac|cod|her|whb", substr(stockkeylabelold,1,3))) %>% 
  filter(tolower(purpose) == "advice") %>% 
  group_by(stockkeylabelold, assessmentyear) %>% 
  summarize(nyears = n()) %>% 
  spread(key=assessmentyear, value=nyears) %>% 
  View()

qcsexcel %>% 
  # filter(grepl("her-47d", stockkeylabelold)) %>%
  filter(grepl("mac", substr(stockkeylabelold,1,3))) %>% 
  group_by(purpose, assessmentyear) %>% 
  summarize(nyears = n()) %>% 
  View()

# qcsexcel %>% 
# bind_rows(t1, t2)  %>% 
  iAssess %>% 
  # filter(grepl("her-47d", stockkeylabelold)) %>%
  # filter(grepl("mac", substr(stockkeylabelold,1,3))) %>% 
  filter(grepl("whb", substr(stockkeylabelold,1,3))) %>%
  
  filter(tolower(purpose) == "advice") %>% 
  filter(year >= assessmentyear - 10 ) %>% 
  
  # filter(assessmentyear < 1990) %>% 
  View()
  
  mutate(
    decade = as.character(10*floor(assessmentyear/10)),
    tyear  = substr(as.character(assessmentyear),3,4)
  ) %>% 

  ggplot(aes(x=year, y=stocksize, group=assessmentyear)) +
  theme_publication() +
  geom_line(aes(colour=factor(assessmentmodel))) +
  geom_dl(aes(label  = tyear, colour = decade), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y=0) +
  # facet_wrap(~decade, scales = "free_x") 
  facet_wrap(~assessmentyear, scales = "free_x") 


# Check redfish

sag %>% 
  filter(grepl("reg", stockkeylabel)) %>% 
  distinct(stockkeylabel, assessmentyear)

# -----------------------------------------------------------------------------------------
# Below is old code that needs revising. 
# -----------------------------------------------------------------------------------------

# filter on methods
sagdb %>% 
  filter(assessmentmodel %in% c("xsa","ica","vpa","ls","sam","xsm", "tsa", "trends only", "surba","sms")) %>% 
  View()

# check assessmentypes used
sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number() == 1) %>% 
  filter(!is.na(assesstype)) %>% 
  select(fishstock, assessmentyear, stockpublishnote, status, assesstype) %>% 
  View()

# check stocksizedescriptions used
sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number() == 1) %>% 
  group_by(stocksizedescription) %>% 
  summarise(n = n()) %>% 
  arrange(stocksizedescription) %>% 
  View()

filter(sagdb, is.na(commonname)) %>% View()

sort(unique(sagdb$stocksizeunits))
filter(sagdb, is.na(fishstocknew)) %>% View()
filter(sagdb, is.na(commonname)) %>% View()
filter(sagdb, is.na(stocksizeunits)) %>% View()
filter(sagdb, stocksizedescription == "stock size" & stocksizeunits == "tonnes") %>% View()
filter(sagdb, grepl("ratio", stocksizeunits)) %>% View()

filter(sagdownload, grepl("cod-arct", fishstockold)) %>% View()
filter(sagexcel, grepl("cod-arct", fishstockold)) %>% View()
filter(sagexcel_toadd, grepl("cod-arct", fishstockold)) %>% View()
filter(sagdb, grepl("cod-arct", fishstockold)) %>% group_by(assessmentyear) %>% filter(row_number()==1) %>% 
  arrange(assessmentyear) %>% View()

filter(sagdb, grepl("mac-nea", fishstockold), assessmentyear==2013) %>% View()
filter(sagexcel_toadd, grepl("her-47d3", fishstock), assessmentyear==2017) %>% View()
filter(sagdownload, grepl("mac-nea", fishstock), assessmentyear==2013) %>% View()

