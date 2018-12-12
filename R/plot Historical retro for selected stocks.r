# -----------------------------------------------------------------------------------------------
# plot Historical retro for selected stocks
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
library(scales)    # scales and formatting

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

# load(file=paste(advicedir, "/rdata/iStock.RData",sep=""))
# load(file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
# load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))
load(file=paste(advicedir, "/rdata/iAssess.RData",sep=""))


# -----------------------------------------------------------------------------------------
# Plot of historic retro (SSB)
# -----------------------------------------------------------------------------------------
colourby <- "decade"

iAssess %>%
  
  filter(grepl("ple-n|her-47|mac-67|mac-nea|mac-west|cod-34|cod-nsea|whb-c|whb-n|sol-ns|hom-c|hom-w|had-3|had-n|hke-n", 
               stockkeylabelold)) %>%
  # filter(grepl("her-47|mac-67|mac-nea|mac-west", 
  #              stockkeylabelold)) %>%
  
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
  
  filter(!grepl("benchmark|replace", purpose)) %>% 
  filter(stocksizeunits == "tonnes") %>% 
  

  filter(year >= assessmentyear - 10 ) %>% 
  # filter(assessmentyear == 2010) %>% 
  # filter(year <= assessmentyear -3) %>% 
  # filter(assessmentyear < 1990) %>% 
  
  
  mutate(
    decade = as.character(10*floor(assessmentyear/10)),
    tyear  = substr(as.character(assessmentyear),3,4)
  ) %>% 

  ggplot(aes(x=year, y=stocksize, group=assessmentyear)) +
  theme_publication() +
  theme(
    panel.spacing.x = unit(1, "mm"),
    panel.spacing.y = unit(1, "mm"),
    strip.background = element_blank(),
    strip.text       = element_text(face="bold", hjust=0, margin = margin(2,0,2,0, "mm")),
    legend.position  = "none"
  ) +  
  geom_line(aes(colour=factor(get(colourby)))) +
  geom_dl(aes(label  = tyear, colour = get(colourby)),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  guides(colour=guide_legend(title=colourby, nrow=1)) +
  expand_limits(y=0, x=1980) +
  scale_y_continuous(labels=scientific_format(digits=2), breaks=pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(x="year", y="SSB (tonnes)", title="SSB") +
  # facet_wrap(~stockkeylabelold, scales="free_y", ncol = 1)
  facet_wrap(~stockkeylabelold, scales="free_y")
  # facet_wrap(~speciesfaocode, scales="free_y")
  # facet_wrap(~decade, scales="free_x") 
  # facet_wrap(~assessmentyear, scales = "free_x") 


# -----------------------------------------------------------------------------------------
# Plot of historic retro (F)
# -----------------------------------------------------------------------------------------
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
  
  filter(!grepl("benchmark|exploratory", purpose)) %>% 
  filter(fishingpressureunits %in% c("per year","year-1")) %>% 
  filter(year >= assessmentyear - 10 ) %>% 

  mutate(
    decade = as.character(10*floor(assessmentyear/10)),
    tyear  = substr(as.character(assessmentyear),3,4)
  ) %>% 
  
  ggplot(aes(x=year, y=fishingpressure, group=assessmentyear)) +
  theme_publication() +
  theme(
    panel.spacing.x = unit(1, "mm"),
    panel.spacing.y = unit(1, "mm"),
    strip.background = element_blank(),
    strip.text       = element_text(face="bold", hjust=0, margin = margin(2,0,2,0, "mm"))
  ) +  
  geom_line(aes(colour=factor(get(colourby)))) +
  geom_dl(aes(label  = tyear, colour = get(colourby)), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  guides(colour=guide_legend(title=colourby, nrow=1)) +
  expand_limits(y=0, x=1980) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(x="year", y="F (year-1)", title="Fishing mortality") +
  facet_wrap(~stockkeylabelold, scales="free_y")
# facet_wrap(~speciesfaocode, scales="free_y")
# facet_wrap(~decade, scales="free_x") 
# facet_wrap(~assessmentyear, scales = "free_x") 

