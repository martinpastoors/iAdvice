# -----------------------------------------------------------------------------------------------
# Analyse retrospective error
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# 03/12/2018 added different metrics for retrospective error
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
  
  # filter(grepl("cod", stockkeylabelold)) %>% 
  
  filter(grepl("advice", purpose )) %>%
  filter(stocksizeunits == "tonnes", unitofrecruitment == "thousands", fishingpressureunits %in% c("per year", "year-1")) %>% 
  
  select(assessmentyear, year, stockkeylabelold, stocksize, recruitment, fishingpressure) %>% 
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


# Create datasets with peels
d.merged <-
  d.peels %>% 
  left_join(d, by=c("stockkeylabelold", "assessmentyear", "variable")) %>%
  arrange(stockkeylabelold, desc(assessmentyearbase), variable, desc(year), peel) %>% 
  group_by(stockkeylabelold, assessmentyearbase, variable, peel)  
  # %>% 
  # filter(grepl("cod", stockkeylabelold)) %>% 
  # filter(variable == "stocksize") %>% 
  # filter(year >= assessmentyearbase - 10) 
  

# ---------------------------------------------------------------------------------------------
# Calculate retrospective patterns in peels
# ---------------------------------------------------------------------------------------------

d.metricinpeels <-
  d.merged %>% 
  filter( (peel==0 & year >= max(year-10)) | (peel != 0 & year == max(year)) ) %>% 
  group_by(stockkeylabelold, assessmentyearbase, variable, year) %>% 
  arrange(stockkeylabelold, desc(assessmentyearbase), variable, desc(year)) %>% 
  mutate(base = lag(value, n=1),
         rho_ = ((value-base) / base),
         ab_  = log(value/base, base=exp(1))) %>% 
  filter(!is.na(base)) %>% 
  
  group_by(stockkeylabelold, assessmentyearbase, variable) %>% 
  # summarize(rho = mean(rho))
  # group_by(stockkeylabelold, variable) %>% 
  mutate(ab  = mean(ab_),
         asd_= (log(value/base, base=exp(1)) - ab)^2,
         asd = sqrt(mean(asd_)),
         rho = mean(rho_) )

# create segments for retro plot (cod only)

d.segment <-
  d.merged %>% 
  filter(grepl("hke", stockkeylabelold)) %>%
  filter(variable == "stocksize") %>%
  filter( (peel==0 & year >= max(year-10)) | (peel != 0 & year == max(year)) ) %>% 
  
  group_by(stockkeylabelold, assessmentyearbase, variable, year) %>% 
  arrange(stockkeylabelold, desc(assessmentyearbase), variable, desc(year)) %>% 
  mutate(base = lag(value, n=1) ) %>% 
  filter(!is.na(base)) %>% 
  arrange(stockkeylabelold, desc(assessmentyearbase), desc(assessmentyear), peel, desc(year)) 
  
# plot of the retrospective patterns (cod only)
d.merged %>%
  
  filter(grepl("hke", stockkeylabelold)) %>%
  filter(variable == "stocksize") %>%
  filter(year >= assessmentyearbase - 10) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(legend.position="none") +
  
  geom_segment(data=d.segment, 
               aes(x=year, xend=year,y=value,yend=base), colour="gray", linetype="dashed", size=0.5) +
  geom_line(aes(colour=factor(peel), size=factor(peel))) +
  geom_point(data=filter(d.merged, (peel != 0 & year == max(year)) & grepl("hke", stockkeylabelold) & variable=="stocksize")) +
  scale_colour_manual(breaks = c("0", "1", "2", "3", "4","5"), values=c("red", rep("black", 5))) +
  scale_size_manual (breaks = c("0", "1", "2", "3", "4","5"), values=c(1, rep(0.5, 5))) +
  expand_limits(y=0) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_wrap(~assessmentyearbase, scales="free")

# plot of retrospective metrics per assessmentyear (and looking back)
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


# ---------------------------------------------------------------------------------------------
# Calculate metrics all compared to the most recent assessment
# ---------------------------------------------------------------------------------------------

d.metricinone <-
  d %>% 
  
  # filter(grepl("cod", stockkeylabelold)) %>% 
  # filter(variable == "stocksize") %>% 
  
  group_by(stockkeylabelold, assessmentyear, variable) %>% 

  filter( (assessmentyear == 2018 ) | (assessmentyear != 2018 & year == max(year)) ) %>% 
  group_by(stockkeylabelold, variable, year) %>% 
  arrange(stockkeylabelold, variable, desc(year), desc(assessmentyear)) %>% 
  mutate(base = lag(value, n=1),
         ab_  = log(value/base, base=exp(1)),
         rho_ = ((value-base) / base) ) %>% 
  filter(!is.na(base)) %>% 
  
  group_by(stockkeylabelold, variable) %>% 
  mutate(ab  = mean(ab_),
         asd_= (log(value/base, base=exp(1)) - ab)^2,
         asd = sqrt(mean(asd_)),
         rho = mean(rho_) )


# Plot of all metrics relative to the most recent assessment.
d.metricinone %>% 
  
  distinct(stockkeylabelold, variable, year, rho_, ab_, asd_) %>% 
  gather(key=metric, value=value, c(rho_, ab_, asd_)) %>% 
  # filter(metric %in% c("asd")) %>% 
  filter(variable == "stocksize") %>% 
  
  ggplot(aes(x=year, y=value)) +
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
  geom_bar(aes(fill=metric), stat = "identity") +
  coord_cartesian(ylim=c(-1, 1)) +  # set the limits in cartesian space prevents outlier lines to be excluded
  labs(x="assessmentyear", y="", title="Retrospective metrics") +
  facet_grid(stockkeylabelold~metric, switch = "y")

# Look for retrospective measures that capture variation and scale differences
# Jonsson & Hjorleifsson 2000
# Hurtado 2015
# Leseur 2004

# Compare the two metrics
d.metricinone %>% 
  # select(stockkeylabelold, variable, year, ab_, asd_) %>% 
  # left_join(d.mohn, by=c("stockkeylabelold", "variable", "year"="assessmentyearbase")) %>% 
  
  ggplot(aes(x=rho_,y=ab_)) +
  theme_publication() +
  geom_point() +
  facet_grid(stockkeylabelold~variable, scales="free")

  View()

