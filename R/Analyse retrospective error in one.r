# -----------------------------------------------------------------------------------------------
# Analyse retrospective error in one (no peels)
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# 03/12/2018 added different metrics for retrospective error
# 11/12/2018 checked for calculations as one time series
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

d %>% filter(stockkeylabelold == "hke-nrtn", assessmentyear==2000) %>% View()

d <-
  iAssess %>%
  
  # Exclude prediction years
  filter(year <= assessmentyear) %>% 
  
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
  
  # filter(!grepl("hke", stockkeylabelold)) %>% 
  
  filter(purpose == "advice") %>%     
  filter(stocksizeunits       == "tonnes") %>%  # Note: this is filtering on absolute vs relative; but not really
  
  select(assessmentyear, year, stockkeylabelold, stocksize, recruitment, fishingpressure, purpose) %>% 
  distinct() %>% 
  gather(key=variable, value=value, stocksize:fishingpressure) %>% 
  filter(!is.na(value)) 

# d %>% 
#   group_by(stockkeylabelold, variable) %>% 
#   filter(grepl("cod", stockkeylabelold)) %>% 
#   filter(year >= 2000) %>% 
#   filter(variable == "stocksize") %>% 
#   spread(key=year, value=value) %>% 
#   arrange(desc(assessmentyear)) %>% 
#   View()

# d %>%
#   filter(grepl("cod", stockkeylabelold)) %>%
#   filter(year >= 1990) %>%
#   filter(purpose == "advice") %>%
#   filter(variable == "stocksize") %>%
#   select(stockkeylabelold, assessmentyear, year, value) %>%
#   View()

  

# ---------------------------------------------------------------------------------------------
# Calculate Mohn's and Jonsson's metrics all compared to the most recent assessment
# ---------------------------------------------------------------------------------------------

d.mohnjonsson1 <-
  d %>% 
  
  group_by(stockkeylabelold, assessmentyear, variable) %>% 
  arrange(stockkeylabelold, variable, desc(assessmentyear)) %>% 

  filter( (assessmentyear == 2018 ) | (assessmentyear != 2018 & year == max(year)) ) %>% 
  
  # TEMP !!!!!
  filter( variable == "stocksize") %>% 
  # filter(grepl("cod", stockkeylabelold)) %>% 
  
  group_by(stockkeylabelold, variable, year) %>% 
  arrange(stockkeylabelold, variable, desc(year), desc(assessmentyear)) %>% 
  mutate(value_last  = lag(value, n=1),
         jonsson_ab  = log(value/value_last),
         mohn_rho    = ((value - value_last) / value_last),
         mohn_rho_abs= abs(mohn_rho)) %>% 
  filter(!is.na(value_last)) %>% 
  
  group_by(stockkeylabelold, variable) %>% 
  mutate(jonsson_dev  = (log(value/value_last) - mean(jonsson_ab)),
         jonsson_dev2 = (jonsson_dev)^2 ) 
  

d.mohnjonsson2 <-
  d.mohnjonsson1 %>% 
  
  group_by(stockkeylabelold, variable) %>% 
  summarize(jonsson_ab   = mean(jonsson_ab),
            jonsson_asd  = sqrt(mean(jonsson_dev2)),
            mohn_rho_abs = mean(mohn_rho_abs),
            mohn_rho     = mean(mohn_rho) ) 

# ggplot(data=d.mohnjonsson1) +
#   geom_histogram(aes(x=jonsson_ab, y = ..density..), colour="black",fill="black", alpha=0.4) + 
#   geom_histogram(aes(x=mohn_rho, y = ..density..), colour="blue",fill="blue", alpha=0.4) + 
#   geom_vline(aes(xintercept=0), linetype="dashed") +
#   facet_wrap(~stockkeylabelold)

# d %>% 
#   filter(grepl("hom", stockkeylabelold)) %>%
#   filter(assessmentyear == 2003) %>% 
#   View()

# d.mohnjonsson1 %>% 
#   filter(grepl("hom", stockkeylabelold)) %>%
#   filter(assessmentyear > 2000) %>% 
#   View()

# ---------------------------------------------------------------------------------------------
# Calculate Ralston's retrospective measures
# ---------------------------------------------------------------------------------------------

d.ralston1 <-
  
  d %>% 
  filter(variable == "stocksize") %>% 
  filter(year >= 1980) %>%
  # filter(grepl("hom", stockkeylabelold)) %>% 
  
  arrange(stockkeylabelold, year, assessmentyear) %>% 
  group_by(stockkeylabelold, year) %>%
  
  filter(n() >= 2) %>% 
  
  mutate(logB     = log(value),
         meanlogB = mean(log(value)),
         logBdev  = logB - meanlogB,
         logBdev2 = (logBdev)^2,
         n        = n(),
         n_1      = n - 1) %>% 
  
  # now do summing over years
  mutate(sumlogBdev2 = sum(logBdev2) )
  
# now summarize by year
d.ralston2 <-
  d.ralston1 %>% 
  group_by(stockkeylabelold, year) %>%
  summarize(
    meanlogBdev = mean(logBdev),
    sumlogBdev2 = sum(logBdev2),
    n_1         = mean(n_1), 
    avglogBdev2 = sumlogBdev2/n_1)
  
# and calculate by stock
d.ralston3 <-
  
  d.ralston2 %>% 
  # and then by stock
  group_by(stockkeylabelold) %>%
  distinct(stockkeylabelold, year, n_1, sumlogBdev2, .keep_all = FALSE) %>% 
  summarize(
    sumlogBdev2all = sum(sumlogBdev2),
    n_1            = sum(n_1)) %>%
  
  # calculate overall metrics
  mutate(
    ralston_sigma = sqrt(sumlogBdev2all / n_1),
    cv            = percent(sqrt(exp(ralston_sigma^2)-1)))


ggplot(data=d.ralston1, aes(x=logBdev)) +
  geom_histogram(aes(y = ..density..), colour="black",fill=NA) + 
  geom_vline(aes(xintercept=0), linetype="dashed") +
  facet_wrap(~stockkeylabelold)


ggplot(data=d.mohnjonsson1) +
  theme_publication() +
  # geom_line(data=d.mohnjonsson1, aes(x=year, y=jonsson_ab), colour="black") +
  
  geom_line(aes(x=year, y=jonsson_dev), colour="green") +
  geom_point(aes(x=year, y=jonsson_dev), colour="green") +
  
  geom_line(aes(x=year, y=mohn_rho), colour="blue") +
  geom_point(aes(x=year, y=mohn_rho), colour="blue") +
  
  geom_boxplot(data=d.ralston1, 
               aes(x=year, y=logBdev, group=year), fill=NA, colour="red", alpha=.5, outlier.shape=1) +
  geom_hline(aes(yintercept=0)) +
  labs(y="deviance")+
  facet_wrap(~stockkeylabelold)

bind_rows(d.mohnjonsson2, d.ralston3) %>% 
  select(stockkeylabelold, jonsson_asd, mohn_rho_abs, ralston_sigma) %>% 
  gather(key=metric, value=value, jonsson_asd:ralston_sigma) %>%
  filter(!is.na(value)) %>% 
  group_by(metric) %>% 
  mutate(value_scaled = value / mean(value)) %>% 
  
  ggplot(aes(x=stockkeylabelold, y=value_scaled)) +
  geom_bar(aes(fill=metric), stat= "identity", alpha=0.8, position="dodge") +
  coord_flip()

# ---------------------------------------------------------------------------------------------
# Plot of all metrics by year calculated over the whole time series
# ---------------------------------------------------------------------------------------------

bind_rows(
    select(d.mohnjonsson1, stockkeylabelold, year,  mohn_rho_abs, jonsson_dev2),
    select(d.ralston2, stockkeylabelold, year, avglogBdev2)
  ) %>% 
  ungroup() %>% 
  select(-variable) %>% 
  gather(key=metric, value=value, mohn_rho_abs:avglogBdev2) %>% 
  filter(!is.na(value)) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(
    panel.spacing    = unit(0.1, "lines"),
    strip.background = element_blank(),
    strip.text.x     = element_text(face="bold", hjust=0.5, margin = margin(2,0,2,0, "mm")),
    strip.text.y     = element_text(face="bold", hjust=0, margin = margin(2,0,2,0, "mm"), angle = 180),
    strip.placement = "outside"
  ) +
  
  # geom_hline(aes(yintercept = 0.5), colour="gray80", linetype="dashed", size=0.5) +
  # geom_hline(aes(yintercept = 0), colour="gray20", size=0.5) +
  # geom_hline(aes(yintercept = -0.5), colour="gray80", linetype="dashed", size=0.5) +
  geom_line(aes(colour=metric)) +
  # coord_cartesian(ylim=c(-1, 1)) +  # set the limits in cartesian space prevents outlier lines to be excluded
  # labs(x="assessmentyear", y="", title="Retrospective metrics") +
  facet_wrap(~stockkeylabelold)

# Look for retrospective measures that capture variation and scale differences
# Jonsson & Hjorleifsson 2000
# Hurtado 2015
# Leseur 2004

# ---------------------------------------------------------------------------------------------
# plot of the retrospective patterns in one (example: hake)
# ---------------------------------------------------------------------------------------------

my.species <- "hke"

d.selection <-
  d %>%
  group_by(stockkeylabelold, variable) %>% 
  arrange(stockkeylabelold, variable, desc(assessmentyear)) %>% 
  
  filter( year >= 1980) %>% 
  filter( variable == "stocksize") %>% 
  # filter(grepl(my.species, stockkeylabelold)) %>% 
  mutate(plot=ifelse(assessmentyear == max(assessmentyear), "last", "other")) %>% 
  group_by(stockkeylabelold, variable, assessmentyear)

d.segment <-
  d.selection %>% 
  
  filter( (assessmentyear == 2018 ) | (assessmentyear != 2018 & year == max(year)) ) %>% 
  group_by(stockkeylabelold, year) %>% 
  arrange(stockkeylabelold, desc(year)) %>% 
  mutate(base = lag(value, n=1) ) %>% 
  filter(!is.na(base)) %>% 
  arrange(stockkeylabelold, desc(year)) 

d.points <-
  d.selection %>% 
  filter( (assessmentyear != 2018 & year == max(year)) ) 

d.selection %>%   
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.position="none") +
  
  geom_segment(data=d.segment, 
               aes(x=year, xend=year,y=value,yend=base), colour="gray", linetype="dashed", size=0.5) +
  geom_line(aes(colour = plot, size=plot, group=assessmentyear)) +
  geom_point(data=d.points) +

  scale_colour_manual(values=c(last = "red", other="black")) +
  scale_size_manual (values=c(last=1, other=0.5)) +
  expand_limits(y=0) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(y="ssb (tonnes)") +
  facet_wrap(~stockkeylabelold, scales="free")



# number of observations included in Ralston calculations
d %>% 
  filter(variable == "stocksize") %>% 
  filter(year >= 1980) %>% 
  group_by(stockkeylabelold, year) %>%
  summarize(n        = n()) %>% 
  # group_by(stockkeylabelold) %>% 
  # mutate(b = (log(value) - meanlogB)^2,
  #        n = mean(n)) %>% 
  
  ggplot(aes(year, n)) +
  theme_publication() +
  geom_line() +
  geom_point() +
  facet_wrap(~stockkeylabelold)

