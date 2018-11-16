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
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

# load(file=paste(advicedir, "/rdata/iStock.RData",sep=""))
# load(file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
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

# -----------------------------------------------------------------------------------------
# Plot of historic retro
# -----------------------------------------------------------------------------------------

iAssess %>% 
  # filter(grepl("her-47d", stockkeylabelold)) %>%
  # filter(grepl("mac-678ab|mac-nea|mac-west", stockkeylabelold)) %>% 
  # filter(grepl("whb", substr(stockkeylabelold,1,3))) %>%
  # filter(grepl("ple-n", stockkeylabelold)) %>% 
  
  filter(grepl("ple-n|her-47|mac-67|mac-nea|mac-west|cod-34|cod-nsea|whb-c|whb-n|sol-ns", stockkeylabelold)) %>%
  mutate(speciesfaocode = substr(stockkeylabelold, 1, 3)) %>% 
  
  filter(tolower(purpose) == "advice") %>% 
  
  # filter(assessmentyear == 2016) %>% 
  # filter(is.na(assessmentmodel)) %>%
  # View()
  
  filter(year >= assessmentyear - 10 ) %>% 
  # filter(assessmentyear == 2010) %>% 
  filter(year <= assessmentyear -3) %>% 
  # filter(assessmentyear < 1990) %>% 
  
  
  mutate(
    decade = as.character(10*floor(assessmentyear/10)),
    tyear  = substr(as.character(assessmentyear),3,4)
  ) %>% 

  ggplot(aes(x=year, y=stocksize, group=assessmentyear)) +
  theme_publication() +
  geom_line(aes(colour=factor(assessmentmodel))) +
  geom_dl(aes(label  = tyear, colour = assessmentmodel), method = list(dl.combine("last.points"), cex = 0.8)) +
  guides(colour=guide_legend(title="Model", nrow=1)) +
  expand_limits(y=0) +
  facet_wrap(~speciesfaocode, scales="free_y")
  # facet_wrap(~decade, scales="free_x") 
  # facet_wrap(~assessmentyear, scales = "free_x") 


# -----------------------------------------------------------------------------------------
# Apply 2 over 3 rule
# -----------------------------------------------------------------------------------------

iAssess %>% 

  filter(grepl("ple-n|her-47|mac-67|mac-nea|mac-west|cod-34|cod-nsea|whb-c|whb-n|sol-ns", stockkeylabelold)) %>%
  mutate(speciesfaocode = substr(stockkeylabelold, 1, 3)) %>% 
  
  filter(tolower(purpose) == "advice") %>% 
  filter(year >= assessmentyear - 10 ) %>% 

  mutate(
    decade = as.character(10*floor(assessmentyear/10)),
    tyear  = substr(as.character(assessmentyear),3,4),
    code   = ifelse(year >= (assessmentyear - 2) & year <= (assessmentyear -1), "x2", NA ),
    code   = ifelse(year >= (assessmentyear - 5) & year <= (assessmentyear -3), "x3", code)
  ) %>% 
  
  filter(!is.na(code)) %>% 
  
  group_by(speciesfaocode, assessmentyear, code) %>% 
  summarize(stocksize = mean(stocksize, na.rm=TRUE)) %>% 
  
  spread(key=code, value=stocksize) %>% 
  mutate(tst = x2/x3) %>% 
  filter(!is.na(tst)) %>% 
  
  ## THIS IS NOT WORKING YET !!!
  
  group_by(speciesfaocode) %>% 
  mutate(trend = ifelse(is.na(lag(tst)), 1000* tst, NA),
         trend = ifelse(is.na(trend), lag(trend)*tst, trend)) %>% 
  View()



  ggplot(aes(x=assessmentyear, y=tst, group=speciesfaocode)) +
  theme_publication() +
  geom_line(colour="blue") +
  expand_limits(y=0) +
  facet_wrap(~speciesfaocode, scales="free_y")
# facet_wrap(~decade, scales="free_x") 
# facet_wrap(~assessmentyear, scales = "free_x") 


# -----------------------------------------------------------------------------------------
# Checks on specific stocks
# -----------------------------------------------------------------------------------------

# Check redfish
sag %>% 
  filter(grepl("reg", stockkeylabel)) %>% 
  distinct(stockkeylabel, assessmentyear)

# Check blue whiting
sag %>% 
  filter(grepl("whb", stockkeylabel)) %>% 
  filter(assessmentyear == 2010) %>% 
  View()

# -----------------------------------------------------------------------------------------
# Check number of assessments per year in SAG database (only with SSB data)
# -----------------------------------------------------------------------------------------

# qcsexcel %>% 
sag %>% 
  
  # filter only with stocksize
  filter(!is.na(stocksize)) %>% 
  
  # remove certain species
  filter(!grepl("nep", stockkeylabel)) %>% 
  filter(!grepl("^rj", stockkeylabel)) %>% 
  
  # generate fao code
  mutate(speciesfaocode = substr(stockkeylabel, 1, 3)) %>% 
  
  # select herring
  # filter(speciesfaocode == "her") %>% 
  
  # select year
  # filter(assessmentyear == 2015) %>% 
  
  distinct(stockkey, stockkeylabelold, speciesfaocode, assessmentyear, purpose) %>% 
  group_by(speciesfaocode, assessmentyear) %>% 
  summarize(n = n()) %>% 
  
  ggplot(aes(x=assessmentyear, y=n)) +
  theme_publication() +
  theme(
    legend.position = "none"
  ) +
  geom_bar(aes(fill=factor(speciesfaocode)), stat="identity", position="stack") +
  facet_wrap(~speciesfaocode)

# plot assessments in common between excel and sag
t1 <-
  incommon %>% 
  left_join(sag, by=c("stockkey","stockkeylabel", "assessmentyear", "purpose")) %>% 
  mutate(source = "sag")

t2 <-
  incommon %>% 
  left_join(qcsexcel, by=c("stockkey","stockkeylabel", "assessmentyear","purpose")) %>% 
  dplyr::select(one_of(names(t1))) %>% 
  mutate(source = "excel")

bind_rows(t1, t2) %>% 
  filter(!is.na(stocksize)) %>% 
  group_by(stockkey, assessmentyear, source) %>% 
  summarize(n=n(), 
            stocksize = sum(stocksize, na.rm = TRUE)) %>% 
  ggplot(aes(x=assessmentyear, y=stocksize, group=source)) +
  theme_publication() +
  geom_line(aes(colour=source)) +
  facet_wrap(~stockkey, scales="free_y")

bind_rows(t1, t2) %>% 
  filter(!is.na(stocksize)) %>% 
  group_by(stockkey, assessmentyear, source, purpose) %>% 
  summarize(n=n(), 
            stocksize = sum(stocksize, na.rm = TRUE)) %>% 
  filter(stockkey == 169123) %>% 
  View()

sag %>% 
  filter(stockkey==169108) %>% 
  filter(assessmentyear ==2014) %>% 
  ggplot(aes(x=year, y=stocksize, group=published)) +
  theme_publication() +
  geom_line(aes(colour=published))

t1 %>% 
  filter(!is.na(stocksize)) %>% 
  distinct() %>% 
  group_by(assessmentkey, stockkey, stockkeylabel, assessmentyear, purpose, year, source) %>% 
  summarize(n=n(),
            stocksize = sum(stocksize, na.rm = TRUE)) %>%
  filter(stockkey == 169142) %>% 
  View()

# make overview of different types of advice per stock and assessmentyear
sag %>% 
  ungroup() %>% 
  filter(!is.na(stockkey)) %>% 
  mutate(combvar = paste(purpose,published, sep="/")) %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, combvar) %>%
  group_by(stockkey, stockkeylabel, assessmentyear) %>% 
  summarize(text = paste(combvar, collapse=" ")) %>% 
  spread(key=assessmentyear, value=text) %>% 
  View()

# make overview of assessments that have more than one assessment in a year
sag %>% 
  filter(!is.na(stockkey)) %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, purpose, published) %>%
  group_by(stockkey, stockkeylabel, assessmentyear) %>% 
  mutate(n = n()) %>% 
  filter(n>1) %>% 
  select(assessmentyear, stockkey, stockkeylabel, purpose, published) %>% 
  arrange(assessmentyear, stockkey, stockkeylabel, purpose, published) %>% 
  write.csv(., file="duploassessments.csv")

# plot by purpose
qcsexcel %>% 
  # sag %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, purpose, published) %>% 
  group_by(assessmentyear, purpose) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=assessmentyear, y=n)) +
  theme_publication() +
  geom_bar(aes(fill=purpose), stat="identity")

# plot by published
sag %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, purpose, published) %>% 
  group_by(assessmentyear, published) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=assessmentyear, y=n)) +
  theme_publication() +
  geom_bar(aes(fill=published), stat="identity")

