# -----------------------------------------------------------------------------------------------
# Analyse scale differences
#
# 14/08/2017 added plot for assessment methods
# 29/11/2018 adapted from Mohn's code
# 03/12/2018 redone the logic of the selections; now with left_joins instead of lags; variables: recruitment, stocksize and fishing pressure
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

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))


# ---------------------------------------------------------------------------------------------
# Scale differences: calculate average value for all the years in common. Calculate the number of jumps, 
# defined as a relative difference of more than xx%
# ---------------------------------------------------------------------------------------------

removelastyears <- 0
numberofyears   <- 8

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
  # filter(grepl("cod", stockkeylabelold)) %>% 
  
  filter(grepl("advice", purpose )) %>%
  filter(stocksizeunits == "tonnes", unitofrecruitment == "thousands", fishingpressureunits %in% c("per year", "year-1")) %>% 
  
  select(assessmentyear, year, stockkeylabelold, stocksize, recruitment, fishingpressure) %>% 
  distinct() %>% 
  gather(key=variable, value=value, stocksize:fishingpressure) %>% 
  filter(!is.na(value)) %>% 
  
  # at least 5 years needed for comparison
  group_by(stockkeylabelold, assessmentyear, variable) %>%
  mutate(nyears = n_distinct(year)) %>% 
  filter(nyears >= 5) %>% 
  select(-nyears) 
  

d.benchmarks <-
  
  iAdvice %>% 
  filter(benchmark) %>% 
  select(stockkey, assessmentyear, benchmark) %>% 
  right_join(iAssess, by=c("stockkey", "assessmentyear")) %>% 
  mutate(benchmark = ifelse(is.na(benchmark), FALSE, benchmark)) %>% 

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
  
  filter(benchmark) %>% 
  
  distinct(stockkeylabelold, assessmentyear, benchmark) 

# Overview of purpose
d.purpose <-
  
  iAssess %>% 
  # filter(!(grepl("benchmark", purpose) & benchmark)) %>% 
  filter(!(grepl("benchmark", purpose))) %>% 
  filter(adviceonstock) %>% 
  
  mutate(purpose = tolower(purpose)) %>% 
  
  filter(grepl("ple-n|her-47|mac-67|mac-nea|mac-west|cod-34|cod-nsea|whb-c|whb-n|sol-ns|hom-c|hom-w|had-34|had-n|hke-n", 
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
  
  distinct(stockkeylabelold, assessmentyear, purpose) %>% 
  arrange(stockkeylabelold, assessmentyear) 

  # %>% 
  # group_by(stockkeylabelold, assessmentyear) %>% 
  # mutate(n = n()) %>% 
  # filter(n >= 2) %>% 
  # View()

d.purpose %>% 
  filter(assessmentyear >= 1980) %>% 
  group_by(assessmentyear, purpose) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=assessmentyear, y=n, group=purpose)) +
  theme_publication() +
  # geom_line(aes(colour=factor(assessmentmodel))) +
  geom_bar(aes(fill=factor(purpose)), stat="identity") + 
  scale_y_continuous(breaks=pretty_breaks())

# Overview of assessment models
d.assessments <-
  
  iAdvice %>% 
  filter(grepl("advice", purpose)) %>% 
  mutate(assessmentmodel = tolower(assessmentmodel)) %>% 
  
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
  
  distinct(stockkeylabelold, assessmentyear, assessmentmodel, ncpueseries, nsurveyseries) %>% 
  arrange(stockkeylabelold, assessmentyear)

d.assessments %>% 
  filter(assessmentyear >= 1980) %>% 
  group_by(assessmentyear, assessmentmodel) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=assessmentyear, y=n, group=assessmentmodel)) +
  theme_publication() +
  # geom_line(aes(colour=factor(assessmentmodel))) +
  geom_bar(aes(fill=factor(assessmentmodel)), stat="identity") + 
  scale_y_continuous(breaks=pretty_breaks())

d.assessments %>% filter(is.na(assessmentmodel)) %>% View()

d.pairs <-
  d %>% 
  distinct(stockkeylabelold, assessmentyear, variable) %>% 
  group_by(stockkeylabelold, variable) %>% 
  arrange(stockkeylabelold, variable, assessmentyear) %>% 
  mutate(assessmentyearprior = lag(assessmentyear),
         assessmentpair      = paste(assessmentyearprior, assessmentyear, sep="-")) %>% 
  rename(assessmentyearlast  = assessmentyear) %>% 
  filter(!is.na(assessmentyearprior))


# data series with most recent (last) estimation
d.last <-
  d.pairs %>% 
  select(-assessmentyearprior) %>% 
  left_join(d, by=c("stockkeylabelold", "variable", "assessmentyearlast" = "assessmentyear")) %>% 
  mutate(label="last") %>% 
  rename(assessmentyear = assessmentyearlast)

# data series with previous (prior) estimation
d.prior <-
  d.pairs %>% 
  select(-assessmentyearlast) %>% 
  left_join(d, by=c("stockkeylabelold", "variable", "assessmentyearprior" = "assessmentyear")) %>% 
  mutate(label="prior") %>% 
  rename(assessmentyear = assessmentyearprior)

# merge last and prior and filter appropriate years
d.merged <-
  bind_rows(d.last, d.prior) %>% 
  group_by(stockkeylabelold, assessmentpair, variable) %>% 
  
  # filter the appropriate period
  filter(year <= min(assessmentyear) - removelastyears,
         year >= min(assessmentyear) - removelastyears - numberofyears) %>% 
  
  # filter only years that are in common in last and prior
  group_by(stockkeylabelold, assessmentpair, variable, year) %>% 
  filter(n() == 2) %>% 
  
  # sort
  arrange(stockkeylabelold, assessmentpair, variable, assessmentyear)

# create dataset of means
d.means <-
  d.merged %>% 
  group_by(stockkeylabelold, assessmentpair, variable) %>% 
  mutate(   miny      = min(year),
            maxy      = max(year)) %>% 
  group_by(stockkeylabelold, assessmentpair, variable, assessmentyear, label, miny, maxy) %>% 
  summarise(value = mean(value, na.rm=TRUE)) %>% 
  gather(key=tmp, value=year, miny:maxy) %>% 
  arrange(stockkeylabelold, assessmentpair, variable, assessmentyear, label)

# make list of stocks with required number of assessment years
d.stocks <-
  d.pairs %>% 
  group_by(stockkeylabelold, variable) %>% 
  summarise(n=n()) %>% 
  filter(n >= 8)

# calculate scaling parameters per stock and per year
d.year <- 
  d.means %>%
  filter(tmp=="miny") %>% 
  ungroup() %>% 
  select(stockkeylabelold, assessmentpair, variable, label, value) %>% 
  spread(key=label, value=value) %>% 
  mutate(sigma            = (last-prior)/prior,
         sigma_abs        = abs(sigma),
         assessmentyear = as.integer(substr(assessmentpair, 6,9))) 

# calculate overall scaling parameter
d.scale <-
  d.year %>% 
  group_by(stockkeylabelold, variable) %>% 
  summarise(n         = n(),
            n2        = sum(sigma_abs > 0.1, na.rm=TRUE),
            sigma     = mean(sigma, na.rm=TRUE),
            sigma_abs = mean(sigma_abs, na.rm=TRUE)
  ) %>%
  mutate(sigma_abs    = percent_format(accuracy=1)(sigma_abs),
         ratio        = paste(n2,n,sep="/")) 

# calculate overall scaling parameter by decade
d.scalebydecade <-
  d.year %>% 
  mutate(    decade = as.character(10*floor(assessmentyear/10)) ) %>% 
  group_by(stockkeylabelold, variable, decade) %>% 
  summarise(n         = n(),
            n2        = sum(sigma_abs > 0.1, na.rm=TRUE),
            sigma     = mean(sigma, na.rm=TRUE),
            sigma_abs = mean(sigma_abs, na.rm=TRUE)
  ) %>%
  mutate(sigma_abs    = percent_format(accuracy=1)(sigma_abs),
         ratio        = paste(n2,n,sep="/"),
         ratio2       = percent_format(accuracy=1) (n2/n),
         ratio3       = paste(ratio2," (", n, ")", sep="") ) 

# print table
d.scalebydecade %>% 
  filter(variable == "stocksize") %>% 
  ungroup() %>% 
  select(stockkeylabelold, decade, ratio3) %>% 
  spread(key=decade, value=ratio3) %>% 
  rename(stock=stockkeylabelold) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "left",
               missing=".",
               round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) 


# plot differences between two subsequent assessments

myvar <- "stocksize"

d.merged %>% 
  
  filter(variable == myvar) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(# legend.position = "none",
    axis.text       = element_text(size=9),
    strip.text      = element_text(face="plain"),
    panel.spacing   = unit(0.1, "lines"),
    strip.text.y    = element_text(angle = 0)) +
  geom_line(aes(colour=factor(label))) +
  geom_point(aes(colour=factor(label))) +
  
  # geom_dl(aes(label  = substr(assessmentyear,3,4), colour=factor(label)),
  #         method = list(dl.combine("last.points"), cex = 0.5)) +
  geom_line(data=filter(d.means, variable == myvar), 
            aes(x=year, y=value, colour=factor(label)), linetype="dashed") +
  
  scale_y_continuous(labels=scientific_format(digits=2)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  expand_limits(y=0) +
  # facet_grid(label ~ header, scales="free_y")
  # facet_grid(stockkeylabelold ~ assessmentpair, scales="free")
  facet_wrap( ~ assessmentpair, scales="free")


# set the treshold for scale difference (10%)
t <- 0.1

# plot scale differences between assessments
# p2 <-
  d.year %>% 
  
  distinct(stockkeylabelold, variable, assessmentyear, sigma) %>%
  gather(key=metric, value=value, c(sigma)) %>% 
  
  mutate(    decade = as.character(10*floor(assessmentyear/10)) ) %>% 
  
  filter(variable == myvar) %>% 
  
  ggplot(aes(x=assessmentyear, y=value)) +
  theme_publication() +
  theme(
    panel.spacing    = unit(0.1, "lines"),
    strip.background = element_blank(),
    strip.text.x     = element_text(face="bold", hjust=0.5, margin = margin(2,0,2,0, "mm")),
    strip.text.y     = element_text(face="bold", hjust=0, margin = margin(2,0,2,0, "mm"), angle = 180),
    strip.placement = "outside",
    legend.position  = "none"
  ) +
  
  geom_hline(aes(yintercept = t), colour="gray80", linetype="dashed", size=0.5) +
  geom_hline(aes(yintercept = 0), colour="gray20", size=0.5) +
  geom_hline(aes(yintercept = -t), colour="gray80", linetype="dashed", size=0.5) +
  
  geom_point(data=d.benchmarks, aes(x=assessmentyear, y=0), colour="gray20", shape=18, size=4, inherit.aes = FALSE) +
  
  geom_line (aes(colour=factor(decade))) +
  geom_point(aes(colour=factor(decade), size = ifelse( abs(value) > t,0.3,0.0001))) +
  geom_text(data=filter(d.scale, variable == myvar), 
            aes(label=ratio),  
            x=-Inf, y=-Inf, hjust=-0.2, vjust=-0.8, inherit.aes=FALSE) +
  
  coord_cartesian(ylim=c(-1, 1)) +  # set the limits in cartesian space prevents outlier lines to be excluded
  scale_y_continuous(breaks = pretty_breaks(), labels=percent_format()) +
  scale_x_continuous(breaks = pretty_breaks(), limits=c(1982, NA)) +
  labs(x=" ", y="") +
  # facet_grid(stockkeylabelold~metric, switch = "y")
  facet_wrap(~stockkeylabelold)


# plot scale differences between assessments for stocksize, recruitment and fishing pressure
d.year %>% 
  
  mutate(    decade = as.character(10*floor(assessmentyear/10)) ) %>% 
  
  ggplot(aes(x=assessmentyear, y=sigma)) +
  theme_publication() +
  theme(legend.position="none") +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(strip.text.y = element_text(angle = 0)) +
  
  geom_hline(aes(yintercept = t), colour="gray80", linetype="dashed", size=0.5) +
  geom_hline(aes(yintercept = 0), colour="gray20", size=0.5) +
  geom_hline(aes(yintercept = -t), colour="gray80", linetype="dashed", size=0.5) +
  
  geom_point(data=d.benchmarks, aes(x=assessmentyear, y=0), colour="gray20", shape=18, size=4) +
  
  geom_line (aes(colour=factor(decade))) +
  geom_point(aes(colour=factor(decade), size = ifelse( sigma_abs > t,0.3,0.0001))) +
  geom_text(data=filter(d.scale, variable == myvar), 
            aes(label=sigma_abs),  
            x=-Inf, y=-Inf, hjust=-0.2, vjust=-0.8, inherit.aes=FALSE) +
  
  coord_cartesian(ylim=c(-0.75, 0.75)) +  # set the limits in cartesian space prevents outlier lines to be excluded
  scale_y_continuous(breaks = pretty_breaks(), labels=percent_format()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_grid(stockkeylabelold~variable)

# multiplot of p1 (analyse retrospective error) and p2 (scale difference)
plot_grid(p1 + theme(legend.position = "none", 
                     axis.title      = element_blank(),
                     plot.margin = unit(c(3, 0.2, 3, 3), "mm")), 
          p2 + theme(axis.title      = element_blank(),
                     axis.text.y     = element_blank(),
                     strip.text.y    = element_blank(),
                     axis.ticks.y   = element_blank(),
                     plot.margin = unit(c(3, 3, 3, 0), "mm")),
          ncol=2, align = 'h', rel_widths = c(35, 10))
