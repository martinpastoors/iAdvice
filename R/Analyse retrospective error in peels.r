# -----------------------------------------------------------------------------------------------
# Analyse retrospective error
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# 03/12/2018 added different metrics for retrospective error
# 12/12/2018 specifically for metrics in peels
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours
library(lubridate)
library(scales)
library(directlabels)  # for printing labels at end of geom lines
library(icesAdvice)

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))


# ---------------------------------------------------------------------------------------------
# Create subset of series (stocks) with at least 8 years of data and SSB in tonnes, F in year-1
# ---------------------------------------------------------------------------------------------

d <-
  iAssess %>%
  
  filter(grepl("ple-n|her-47|mac-67|mac-nea|mac-west|cod-34|cod-nsea|whb-c|whb-n|sol-ns|hom-c|hom-w|had-3|had-n|hke-n", 
               stockkeylabelold)) %>%
  
  mutate(
    stockkeylabelold = ifelse(grepl("ple-n",stockkeylabelold)                  , "ple (north sea)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("her-4",stockkeylabelold)                  , "her (north sea)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("mac-67|mac-nea|mac-west",stockkeylabelold), "mac (west)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("cod-34|cod-nsea",stockkeylabelold)        , "cod (north sea)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("whb-c|whb-n",stockkeylabelold)            , "whb (comb)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("sol-ns",stockkeylabelold)                 , "sol (north sea)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("hom-c|hom-w",stockkeylabelold)            , "hom (west)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("had-3|had-n",stockkeylabelold)            , "had (north sea)",stockkeylabelold),
    stockkeylabelold = ifelse(grepl("hke-n",stockkeylabelold)                  , "hke (north)",stockkeylabelold)
  ) %>% 
  
  # filter(grepl("ple", stockkeylabelold)) %>%
  # filter(assessmentyear %in% c(2014,2015,2017,2018)) %>%
  
  filter(!grepl("hke", stockkeylabelold)) %>% 
  
  filter(purpose == "advice") %>%     
  filter(stocksizeunits       == "tonnes") %>%  # Note: this is filtering on absolute vs relative; but not really
  
  select(assessmentyear, year, stockkeylabelold, stocksize, recruitment, fishingpressure, purpose) %>% 
  distinct() %>% 
  gather(key=variable, value=value, stocksize:fishingpressure) %>% 
  filter(!is.na(value)) 


# create meta data on peels
d.peels <-
  d %>% 
  distinct(stockkeylabelold, assessmentyear, variable) %>% 
  group_by(stockkeylabelold, variable) %>% 
  arrange(stockkeylabelold, variable, assessmentyear) %>% 
  mutate(assessmentyear_0   = assessmentyear,
         assessmentyear_1   = lag(assessmentyear, n=1),
         assessmentyear_2   = lag(assessmentyear, n=2),
         assessmentyear_3   = lag(assessmentyear, n=3),
         assessmentyear_4   = lag(assessmentyear, n=4),
         assessmentyear_5   = lag(assessmentyear, n=5) ) %>% 
  rename(assessmentyearbase = assessmentyear) %>%
  gather(key=peel, value=assessmentyear, assessmentyear_0:assessmentyear_5) %>% 
  mutate(peel = as.integer(str_extract(peel, "[[:digit:]]+"))) %>% 
  arrange(stockkeylabelold, assessmentyearbase, variable, peel, assessmentyear) %>% 
  filter(!is.na(assessmentyear)) %>% 
  group_by(stockkeylabelold, assessmentyearbase, variable) %>%
  mutate(maxpeel = max(peel)) %>% 
  filter(maxpeel == 5) %>% 
  select(-maxpeel) %>% 
  ungroup()

# d.peels %>% filter(grepl("had", stockkeylabelold), assessmentyearbase == 2018, variable =="stocksize") %>% View()

# Create datasets with peels
d.inpeels <-
  d.peels %>% 
  left_join(d, by=c("stockkeylabelold", "assessmentyear", "variable")) %>%
  arrange(stockkeylabelold, desc(assessmentyearbase), variable, desc(year), peel) %>% 
  group_by(stockkeylabelold, assessmentyearbase, variable, peel)  %>% 
  arrange(stockkeylabelold, variable, desc(assessmentyearbase), peel, desc(assessmentyear), desc(year)) 
  # %>% 
  # filter(grepl("cod", stockkeylabelold)) %>% 
  # filter(variable == "stocksize") %>% 
  # filter(year >= assessmentyearbase - 10) 
  
d.inpeels %>%
  filter(grepl("had", stockkeylabelold), assessmentyearbase == 2018, variable =="stocksize") %>%
  filter(year >= 2008) %>%
  arrange(desc(assessmentyear), desc(year)) %>%
  View()

d.inpeels %>%
  group_by(stockkeylabelold, variable, assessmentyearbase, peel) %>%
  filter(grepl("cod", stockkeylabelold)) %>%
  filter(year >= 2010) %>%
  filter(variable == "stocksize") %>%
  spread(key=year, value=value) %>%
  arrange(desc(assessmentyearbase), peel, desc(assessmentyear)) %>%
  View()

d.inpeels %>% 
  
  filter(grepl("cod", stockkeylabelold)) %>%
  filter(year >= 2010) %>%
  filter(variable == "stocksize") %>%
  
  group_by(stockkeylabelold, assessmentyearbase, year) %>%
  summarize(n= n()) %>% 
  spread(key=year, value=n) %>%
  arrange(desc(assessmentyearbase)) %>%
  View()
  
# ---------------------------------------------------------------------------------------------
# Calculate Mohn and Jonsson's retrospective metrics in peels
# ---------------------------------------------------------------------------------------------

d.mohnjonsson_inpeels1 <-
  
  d.inpeels %>% 
  
  filter( (peel==0 & year >= max(year-10)) | (peel != 0 & year == max(year)) ) %>% 
  
  group_by(stockkeylabelold, assessmentyearbase, variable, year) %>% 
  arrange(stockkeylabelold, desc(assessmentyearbase), variable, desc(year)) %>% 
  mutate(value_last  = lag(value, n=1),
         jonsson_ab  = log(value/value_last),
         mohn_rho    = ((value - value_last) / value_last),
         mohn_rho_abs= abs(mohn_rho)) %>% 
  filter(!is.na(value_last)) %>% 
  
  group_by(stockkeylabelold, assessmentyearbase, variable) %>% 
  mutate(jonsson_dev  = (log(value/value_last) - mean(jonsson_ab)),
         jonsson_dev2 = (jonsson_dev)^2 ) 

d.mohnjonsson_inpeels2 <-
  d.mohnjonsson_inpeels1 %>% 
  
  group_by(stockkeylabelold, assessmentyearbase, variable) %>% 
  summarize(jonsson_ab   = mean(jonsson_ab),
            jonsson_asd  = sqrt(mean(jonsson_dev2)),
            mohn_rho_abs = mean(mohn_rho_abs),
            mohn_rho     = mean(mohn_rho) ) 

# ggplot(data=d.mohnjonsson1) +
#   geom_histogram(aes(x=jonsson_ab, y = ..density..), colour="black",fill="black", alpha=0.4) + 
#   geom_histogram(aes(x=mohn_rho, y = ..density..), colour="blue",fill="blue", alpha=0.4) + 
#   geom_vline(aes(xintercept=0), linetype="dashed") +
#   facet_wrap(~stockkeylabelold)

# ---------------------------------------------------------------------------------------------
# Calculate adapted Ralston's retrospective measures in peels
# ---------------------------------------------------------------------------------------------

d.ralston_inpeels1 <-
  
  d.inpeels %>% 
  
  filter(variable == "stocksize") %>% 
  filter(year >= assessmentyearbase - 20) %>%
  # filter(grepl("hom", stockkeylabelold)) %>% 
  
  group_by(stockkeylabelold, assessmentyearbase, year) %>%
  arrange(stockkeylabelold, desc(assessmentyearbase), desc(year) ) %>% 
           
  filter(n() >= 2) %>% 
  
  mutate(logB     = log(value),
         meanlogB = mean(log(value)),
         logBdev  = logB - meanlogB,
         logBdev2 = (logBdev)^2,
         n        = n(),
         n_1      = n - 1) %>% 
  
  # now do summing over years by assessmentyearbase
  mutate(sumlogBdev2 = sum(logBdev2) )

# now summarize by year
d.ralston_inpeels2 <-
  d.ralston_inpeels1 %>% 
  group_by(stockkeylabelold, assessmentyearbase, year) %>%
  summarize(
    meanlogBdev = mean(logBdev),
    sumlogBdev2 = sum(logBdev2),
    n_1         = mean(n_1), 
    avglogBdev2 = sumlogBdev2/n_1)

# and calculate by stock and assessmentyearbase
d.ralston_inpeels3 <-
  
  d.ralston_inpeels2 %>% 
  
  group_by(stockkeylabelold, assessmentyearbase) %>%
  distinct(stockkeylabelold, assessmentyearbase, year, n_1, sumlogBdev2, .keep_all = FALSE) %>% 
  summarize(
    sumlogBdev2all = sum(sumlogBdev2),
    n_1            = sum(n_1)) %>%
  
  # calculate overall metrics
  mutate(
    ralston_sigma = sqrt(sumlogBdev2all / n_1),
    cv            = percent(sqrt(exp(ralston_sigma^2)-1)))

# Average over assessmentyearbase
d.ralston_inpeels4 <-
  d.ralston_inpeels3 %>% 
  group_by(stockkeylabelold) %>% 
  summarize(ralston_sigma = mean(ralston_sigma))
  
# plot ralston by assessmentyearbase
d.ralston_inpeels3 %>% 
  ggplot(aes(assessmentyearbase, ralston_sigma)) +
  theme_publication() +
  geom_line() +
  facet_wrap(~stockkeylabelold)

# compare ralston with and without peels
bind_rows(
  mutate(d.ralston3, type="in one"),
  mutate(d.ralston_inpeels4, type="in peels") ) %>% 
  
  ggplot(aes(stockkeylabelold, ralston_sigma, group=type)) +
  theme_publication() +
  geom_bar(aes(fill=type), stat="identity", position="dodge") +
  coord_flip()


# ---------------------------------------------------------------------------------------------
# plot of the retrospective patterns in peels (cod only)
# ---------------------------------------------------------------------------------------------

my.species <- "cod"

d.segment <-
  d.inpeels %>% 
  filter(grepl(my.species, stockkeylabelold)) %>%
  filter(variable == "stocksize") %>%
  filter( (peel==0 & year >= max(year-10)) | (peel != 0 & year == max(year)) ) %>% 
  
  group_by(stockkeylabelold, assessmentyearbase, variable, year) %>% 
  arrange(stockkeylabelold, desc(assessmentyearbase), variable, desc(year)) %>% 
  mutate(base = lag(value, n=1) ) %>% 
  filter(!is.na(base)) %>% 
  arrange(stockkeylabelold, desc(assessmentyearbase), desc(assessmentyear), peel, desc(year)) 

d.inpeels %>%

  filter(grepl(my.species, stockkeylabelold)) %>%
  filter(variable == "stocksize") %>%
  filter(year >= assessmentyearbase - 10) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.position="none") +
  
  geom_segment(data=d.segment, 
               aes(x=year, xend=year,y=value,yend=base), colour="gray", linetype="dashed", size=0.5) +
  geom_line(aes(colour=factor(peel), size=factor(peel))) +
  geom_point(data=filter(d.inpeels, (peel != 0 & year == max(year)) & grepl(my.species, stockkeylabelold) & variable=="stocksize")) +
  scale_colour_manual(breaks = c("0", "1", "2", "3", "4","5"), values=c("red", rep("black", 5))) +
  scale_size_manual (breaks = c("0", "1", "2", "3", "4","5"), values=c(1, rep(0.5, 5))) +
  expand_limits(y=0) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~assessmentyearbase, scales="free")

# ---------------------------------------------------------------------------------------------
# plot of retrospective metrics per assessmentyear (and looking back)
# ---------------------------------------------------------------------------------------------

d.metricinpeels %>% 
  distinct(stockkeylabelold, variable, assessmentyearbase, rho, ab, asd) %>% 
  gather(key=metric, value=value, c(rho, ab, asd)) %>% 
  # filter(metric %in% c("ab","asd")) %>% 
  
  ggplot(aes(x=assessmentyearbase, y=value)) +
  theme_publication() +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(strip.text.y = element_text(angle = 0)) +
  
  geom_hline(aes(yintercept = 0.5), colour="gray80", linetype="dashed", size=0.5) +
  geom_hline(aes(yintercept = 0), colour="gray20", size=0.5) +
  geom_hline(aes(yintercept = -0.5), colour="gray80", linetype="dashed", size=0.5) +
  geom_line(aes(colour=metric)) +
  geom_point(aes(colour=metric)) +
  # geom_point(aes(y=rho), colour="red") +
  coord_cartesian(ylim=c(-1, 1)) +  # set the limits in cartesian space prevents outlier lines to be excluded
  facet_grid(stockkeylabelold~variable)


# plot of retrospective metrics per assessmentyear and per metric (only stock size)

p1 <-
  d.metricinpeels %>% 
  
  distinct(stockkeylabelold, variable, assessmentyearbase, rho, ab, asd) %>%
  
  mutate(    decade = as.character(10*floor(assessmentyearbase/10)) ) %>% 
  
  gather(key=metric, value=value, c(rho, ab, asd)) %>% 
  # filter(metric %in% c("asd")) %>% 
  filter(variable == "stocksize") %>% 
  
  ggplot(aes(x=assessmentyearbase, y=value)) +
  theme_publication() +
  theme(
    panel.spacing    = unit(0.1, "lines"),
    strip.background = element_blank(),
    strip.text.x     = element_text(face="bold", hjust=0.5, margin = margin(2,0,2,0, "mm")),
    strip.text.y     = element_text(face="bold", hjust=0, margin = margin(2,0,2,0, "mm"), angle = 180),
    strip.placement = "outside",
    legend.position  = "none"
  ) +

  geom_hline(aes(yintercept = 0.5), colour="gray80", linetype="dashed", size=0.5) +
  geom_hline(aes(yintercept = 0), colour="gray20", size=0.5) +
  geom_hline(aes(yintercept = -0.5), colour="gray80", linetype="dashed", size=0.5) +
  geom_bar(aes(fill=decade), stat = "identity") +
  # geom_point(aes(y=rho), colour="red") +
  scale_x_continuous(breaks = pretty_breaks(), limits=c(1982, NA)) +
  coord_cartesian(ylim=c(-1, 1)) +  # set the limits in cartesian space prevents outlier lines to be excluded
  labs(x="assessmentyear", y="", title="Retrospective metrics") +
  facet_grid(stockkeylabelold~metric, switch = "y")






