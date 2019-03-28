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

sag <- 
  get(load(file=paste(dropboxdir, "/rdata/iSAG.RData",sep=""))) %>% 
  lowcase()


# filter(iAssess, stockkeylabel=="her.27.6a7bc", assessmentyear==2017) %>% View()
# filter(sagdata, stockkeylabel %in% c("her-noss","her.27.1-24a514a"), assessmentyear==2017) %>% 
#   View()

# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

# sag %>% filter(assessmentyear == 2019) %>% distinct(stockkeylabel) %>%   View()
# sag %>% filter(assessmentyear == 2019, stockkeylabel=="her.27.3a47d") %>% View()
# d %>% filter(assessmentyear == 2019) %>% distinct(stockkeylabelold, purpose) %>% arrange(stockkeylabelold) %>% View()

d <-
  iAssess %>% 

  # filter(grepl("her-irls|her-nirs", stockkeylabelold) ) %>%
  filter(grepl("her-3a22|her-47d3|her-nirs|her-irls", stockkeylabelold) | grepl("her.27.6a7bc|spr.27.3a4", stockkeylabelnew) ) %>%
  # filter(grepl("her-irls|her-67bc|her-3a22|her-47d3|her-nirs", stockkeylabelold) ) %>%
  filter(assessmentyear   %in%  2019) %>% 
  filter(year             >   1992) %>% 
  # filter(purpose %in% c("advice", "update", "interbenchmark")) %>%
  filter(purpose %in% c("advice")) %>%
  
  ungroup() %>% 
  
  select(assessmentyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew,
         purpose, 
         recruitment, lowrecruitment, highrecruitment, 
         fishingpressure, lowfishingpressure, highfishingpressure, 
         stocksize, lowstocksize, highstocksize) %>%
  
  group_by(assessmentyear, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, purpose) %>% 
  mutate(relstocksize = stocksize/mean(stocksize, na.rm=TRUE)) %>% 
  
  data.frame()



# plot stocksize
d %>% 
  
  filter(!is.na(relstocksize)) %>%  

  ggplot(aes(year,relstocksize, group=stockkeylabelold)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        plot.margin      = unit(c(0,0,0,0), "cm"), 
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = stockkeylabelold), size=1 ) +

  # geom_dl(aes(label  = stockkeylabelold, colour = stockkeylabelold), 
  #         method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "relSSB") +
  facet_wrap(~stockkeylabel)




# d %>% 
#   filter(stockkeylabelold=="her-47d3", assessmentyear %in% 2018:2019, purpose == "advice", year==2015) %>% 
#   select(assessmentyear, stocksize) %>% 
#   mutate(assessmentyear = paste0("y",assessmentyear)) %>% 
#   spread(key=assessmentyear, value=stocksize) %>% 
#   mutate(rate = (y2019/y2018) - 1)



# plot f
d %>% 
  
  ggplot(aes(year,fishingpressure, group=stockkeylabelold)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        plot.margin      = unit(c(0,0,0,0), "cm"), 
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = stockkeylabelold), size=1 ) +
  
  # geom_dl(aes(label  = stockkeylabelold, colour = stockkeylabelold), 
  #         method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "F") +
  facet_wrap(~stockkeylabel, scales="free_y")






