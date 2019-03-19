# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph plotting
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# 04/10/2017 only plot for retro. Updated for iAssess
# 07/03/2019 adapted for new database layout
# -----------------------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# load the data
load(file=paste(dropboxdir,"/rdata/iAssess.RData", sep=""))

# filter(iAssess, stockkeylabel=="her.27.6a7bc", assessmentyear==2017) %>% View()
# filter(sagdata, stockkeylabel %in% c("her-noss","her.27.1-24a514a"), assessmentyear==2017) %>% 
#   View()

# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

d <-
  iAssess %>% 

  # filter(grepl("cod-iceg", stockkeylabelold) ) %>% 
  
  # filter(grepl("her.27.6a7bc", stockkeylabel),
  #        purpose %in% c("advice","benchmark","interbenchmark","withdrawn"),
  #        assessmentyear>=2017) %>% 
  
  # filter(grepl("mac-nea", stockkeylabelold),
  #        purpose %in% c("advice","benchmark","interbenchmark","withdrawn"),
  #        assessmentyear>=2017) %>% 
  
  # filter(grepl("hom-west", stockkeylabelold),purpose %in% c("update","bench"),assessmentyear>=2016) %>% 
  # filter(grepl("ple-nsea", stockkeylabelold) ) %>% 
  # filter(grepl("cod-347d", stockkeylabelold) ) %>% 
  
  # filter(grepl("spr-nsea", stockkeylabelold) | grepl("spr.27.3a4", stockkeylabelnew) ) %>%
  # mutate(stockkeylabelold = "spr.27.3a4") %>% 
  
  # filter(grepl("her-3a22", stockkeylabelold) ) %>% 
  filter(grepl("her-47d3", stockkeylabelold) ) %>% 
  # filter(grepl("hom-west", stockkeylabelold) ) %>% 
  
  # filter(grepl("mac-nea|hom-west|whb-comb|her-noss", stockkeylabelold) ) %>% 
  # filter(grepl("whb", stockkeylabelold) ) %>% 
  # filter(grepl("noss", stockkeylabelold) ) %>% 
  
  ungroup() %>% 
  filter(year             >   2000) %>% 
  # filter(year             >  2000) %>%  
  # filter(year             <= assessmentyear) %>% 
  # filter(assessmentyear   >  1980) %>% 
  filter(assessmentyear   >  2017) %>% 
  
  select(assessmentyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew,
         purpose, 
         recruitment, lowrecruitment, highrecruitment, 
         fishingpressure, lowfishingpressure, highfishingpressure, 
         stocksize, lowstocksize, highstocksize) %>%
  
  mutate(purpose = ifelse(assessmentyear == max(assessmentyear) &
                                  purpose %in% c("advice","update"),
                                  "last",purpose)) %>%
   
  mutate(tyear     = ifelse(purpose == "advice", as.character(assessmentyear), NA),
         tyear     = ifelse(purpose == "update", as.character(assessmentyear), tyear),
         tyear     = ifelse(purpose == "last"  , paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(purpose == "withdrawn"   , paste(assessmentyear,"-W",sep="") ,tyear),
         tyear     = ifelse(purpose == "benchmark" , paste(assessmentyear,"-B",sep="") ,tyear),
         tyear     = ifelse(purpose == "interbenchmark" , paste(assessmentyear,"-B",sep="") ,tyear),
         tyear     = ifelse(purpose == "alt"   , paste(assessmentyear,"-A",sep="") ,tyear)) %>%

  mutate(tyear     = as.character(assessmentyear),
         tyear     = substr(tyear,3,4)) %>% 

  data.frame()


# d %>% 
#   filter(assessmentyear >= 2018,
#          year == 2018) %>% 
#   select(year, assessmentyear, stocksize) %>%
#   mutate(assessmentyear = paste0("n",assessmentyear)) %>% 
#   spread(key=assessmentyear, value=stocksize) %>% 
#   mutate(ratio = n2019/n2018-1)

# get the last assessment year and stock name
lastyear        <- unique(unlist(select(filter(d, purpose=="last"), assessmentyear)))

last <-
  d %>% 
  filter(purpose == "last") %>%
  select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, year, 
         laststocksize = stocksize, lowstocksize, highstocksize, 
         lastfishingpressure=fishingpressure, lowfishingpressure, highfishingpressure, 
         lastrecruitment = recruitment, lowrecruitment, highrecruitment, purpose)

withdrawn <-
  d %>% 
  filter(purpose == "withdrawn") %>% 
  select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, year, stocksize, lowstocksize, highstocksize, lastfishingpressure=fishingpressure, lowfishingpressure, highfishingpressure, 
         lastrecruitment = recruitment, lowrecruitment, highrecruitment, purpose)

# scale to last year ?
# d <-
#   d %>% 
#   left_join(last, by=c("fishstock","year")) %>% 
#   mutate(recruitment = recruitment/lastr,
#          stocksize         = stocksize/laststocksize,
#          fishingpressure           = fishingpressure / lastfishingpressure)

# plot stocksize
p1 <-
  d %>% 
  filter(!is.na(stocksize)) %>%  
  # filter(grepl("2016", tyear)) %>% 
  # filter(purpose == "withdrawn") %>% 
  
  ggplot(aes(year,stocksize, group=paste0(tyear,purpose))) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        plot.margin      = unit(c(0,0,0,0), "cm"), 
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(aes(x=year, ymin=lowstocksize, ymax=highstocksize, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  # geom_ribbon(data=last, aes(x=year, ymin=lowstocksize, ymax=highstocksize, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  # geom_ribbon(data=withdrawn, aes(x=year, ymin=lowstocksize, ymax=highstocksize, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  
  geom_line(aes(colour = purpose, size=purpose) ) +
  # geom_line(aes(colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual  (values=c(last = "red",advice="black",interbenchmark = "green",benchmark = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",advice="black",interbenchmark = "green",benchmark = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",advice="solid",interbenchmark = "dashed",benchmark ="dashed",withdrawn = "dotdash",   alt="dotted")) +
  scale_size_manual    (values=c(last= 1.5,   advice=0.8,    interbenchmark = 1.2,    benchmark = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  +
  facet_grid(stockkeylabelold ~ ., scales="free_y")



# plot f
p2 <-
  d %>% 
  filter(!is.na(fishingpressure)) %>%  
  
  ggplot(aes(year,fishingpressure, group=paste0(tyear,purpose))) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        plot.margin      = unit(c(0,0,0,0), "cm"), 
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(data=last, aes(x=year, ymin=lowfishingpressure, ymax=highfishingpressure, fill = purpose), alpha=0.3, inherit.aes = FALSE) +

  geom_line(aes(colour = purpose, size=purpose) ) +
  # geom_line(aes(colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual  (values=c(last = "red",advice="black",interbenchmark = "green",benchmark = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",advice="black",interbenchmark = "green",benchmark = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",advice="solid",interbenchmark = "dashed",benchmark ="dashed",withdrawn = "dotdash",   alt="dotted")) +
  scale_size_manual    (values=c(last= 1.5,   advice=0.8,    interbenchmark = 1.2,    benchmark = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "F")   +
  facet_grid(stockkeylabelold ~ ., scales="free_y")

# plot_grid(p1 + theme(legend.position  = "none", 
#                      axis.title       = element_blank(),
#                      strip.background = element_rect(colour=NA, fill=NA),
#                      strip.text       = element_text(colour=NA)), 
#           p2 + theme(axis.title       = element_blank()),
#           ncol=2, align = 'h', rel_widths = c(3.5,3))


# filter(iAssess, stockkeylabelold == "cod-iceg") %>% View()


# recruitment
p3 <-
  d %>% 
  filter(!is.na(recruitment)) %>%  
  
  ggplot(aes(year,recruitment, group=paste0(tyear,purpose))) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        plot.margin      = unit(c(0,0,0,0), "cm"), 
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(data=last, aes(x=year, ymin=lowrecruitment, ymax=highrecruitment, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  
  geom_line(aes(colour = purpose, size=purpose) ) +
  # geom_line(aes(colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual  (values=c(last = "red",advice="black",interbenchmark = "green",benchmark = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",advice="black",interbenchmark = "green",benchmark = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",advice="solid",interbenchmark = "dashed",benchmark ="dashed",withdrawn = "dotdash",   alt="dotted")) +
  scale_size_manual    (values=c(last= 1.5,   advice=0.8,    interbenchmark = 1.2,    benchmark = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "recruitment")   +
  facet_grid(stockkeylabelold ~ ., scales="free_y")


plot_grid(p1 + theme(legend.position  = "none", 
                     axis.title       = element_blank(),
                     strip.background = element_rect(colour=NA, fill=NA),
                     strip.text       = element_text(colour=NA)), 
          p2 + theme(axis.title       = element_blank(), 
                     strip.background = element_rect(colour=NA, fill=NA),
                     strip.text       = element_text(colour=NA)),
          p3 + theme(axis.title       = element_blank()),
          ncol=3, align = 'h')


# filter(iAssess, stockkeylabelold == "cod-iceg") %>% View()
