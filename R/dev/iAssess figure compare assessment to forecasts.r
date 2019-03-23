# -----------------------------------------------------------------------------------------------
# iAssess figure compare assessment with forecasts
#
# 05/10/2017 first coding
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# load the data
load(file=paste(dropboxdir,"/rdata/iAssess.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iForecast.RData", sep=""))

# convert iForecast to the appropriate years
t <-
  iForecast %>% 
  mutate(year = lag(managementyear), 
         ssbpreviousyear = ssbpreviousyear * 1000)

# ---------------------------------------------------------------------------------------------
# plot recent assessment with historic forecast values 
# ---------------------------------------------------------------------------------------------

d <-
  iAssess %>% 
  filter(grepl("mac-nea", stockkeylabel),
         assessmentyear == 2017,
         assessmenttype2 == "assess") %>% 
  ungroup() %>% 
  filter(year             >  1980,
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, 
         recruitment:lowrecruitment, f:lowf, ssb:lowssb,
         assessmenttype2) %>%    
  left_join(t, by=c("stockkey","stockkeylabel","year")) %>% 
  data.frame()

# plot ssb
p1 <-
  d %>% 
  filter(!is.na(ssb)) %>%  
  
  ggplot(aes(year,ssb)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(aes(x=year, ymin=lowssb, ymax=highssb, fill = assessmenttype2), alpha=0.3, inherit.aes = FALSE) +

  geom_line(aes(colour = assessmenttype2, size=assessmenttype2, linetype=assessmenttype2) ) +
  
  geom_errorbarh(aes(x=year, y=ssbpreviousyear, xmin=year-0.5, xmax=year+0.5), colour="blue",size=1) +
  geom_line     (aes(x=year, y=ssbpreviousyear)                              , colour="blue",size=0.5) +

  scale_colour_manual(values=c(assess = "red") ) +
  scale_fill_manual(values=c(assess = "red")) +
  scale_linetype_manual(values=c(assess = "solid")) +
  scale_size_manual(values=c(assess = 1.5)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  


# plot f
p2 <-
  d %>% 
  filter(!is.na(f)) %>%  
  
  ggplot(aes(year,f)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(aes(x=year, ymin=lowf, ymax=highf, fill = assessmenttype2), alpha=0.3, inherit.aes = FALSE) +

  geom_line(aes(colour = assessmenttype2, size=assessmenttype2, linetype=assessmenttype2) ) +
  
  geom_errorbarh(aes(x=year, y=fsqpreviousyear, xmin=year-0.5, xmax=year+0.5), colour="blue",size=1) +
  geom_line     (aes(x=year, y=fsqpreviousyear)                              , colour="blue",size=0.5) +
  
  geom_errorbarh(aes(x=managementyear, y=fadvmax, xmin=managementyear-0.5, xmax=managementyear+0.5), colour="red",size=1, alpha=0.5, inherit.aes = FALSE) +
  
  scale_colour_manual(values=c(assess = "red") ) +
  scale_fill_manual(values=c(assess = "red")) +
  scale_linetype_manual(values=c(assess = "solid")) +
  scale_size_manual(values=c(assess = 1.5)) +
  
  expand_limits(y = 0) +
  xlim(1980,2020) +
  labs(x = NULL, y = NULL , title = "F")   +
  facet_grid(stockkeylabel ~ .)

plot_grid(p1 + theme(legend.position = "none", axis.title      = element_blank()), 
          p2 + theme(axis.title      = element_blank()),
          ncol=2, align = 'h', rel_widths = c(3,3))

