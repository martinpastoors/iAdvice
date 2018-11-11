# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph plotting
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# 04/10/2017 only plot for retro. Updated for iAssess
# -----------------------------------------------------------------------------------------------

rm(list=ls())

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

# filter(exceldata, stockkeylabel=="her-noss", assessmentyear==2017)
# filter(sagdata, stockkeylabel %in% c("her-noss","her.27.1-24a514a"), assessmentyear==2017) %>% 
#   View()

# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

d <-
  iAssess %>% 

  # filter(grepl("cod-iceg", stockkeylabelold) ) %>% 
  # filter(grepl("mac-nea", stockkeylabelold),assessmenttype %in% c("update","bench"),assessmentyear>=2016) %>% 
  # filter(grepl("hom-west", stockkeylabelold),assessmenttype %in% c("update","bench"),assessmentyear>=2016) %>% 
  # filter(grepl("ple-nsea", stockkeylabelold) ) %>% 
  # filter(grepl("cod-347d", stockkeylabelold) ) %>% 
  filter(grepl("hom-west", stockkeylabelold) ) %>% 
  
  # filter(grepl("mac-nea|hom-west|whb-comb|her-noss", stockkeylabelold) ) %>% 
  # filter(grepl("whb", stockkeylabelold) ) %>% 
  # filter(grepl("noss", stockkeylabelold) ) %>% 
  
  ungroup() %>% 
  filter(year             >   1985, 
         assessmentyear   >  2015,
  # filter(year             >  2000, 
  #        assessmentyear   >  1980,
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew,
         recruitment:lowrecruitment, f:lowf, ssb:lowssb,
         assessmenttype) %>%
  
  mutate(assessmenttype = ifelse(assessmentyear == max(assessmentyear) &
                                  assessmenttype %in% c("assess","update"),
                                  "last",assessmenttype)) %>%
   
  filter(assessmenttype %in% c("assess","update","last","bench", "withdrawn")) %>%

  mutate(tyear     = ifelse(assessmenttype == "assess", as.character(assessmentyear), NA),
         tyear     = ifelse(assessmenttype == "update", as.character(assessmentyear), tyear),
         tyear     = ifelse(assessmenttype == "last"  , paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(assessmenttype == "withdrawn"   , paste(assessmentyear,"-W",sep="") ,tyear),
         tyear     = ifelse(assessmenttype == "bench" , paste(assessmentyear,"-B",sep="") ,tyear),
         tyear     = ifelse(assessmenttype == "alt"   , paste(assessmentyear,"-A",sep="") ,tyear)) %>%

  # mutate(tyear     = as.character(assessmentyear)) %>% 

  data.frame()

# filter(iAssess, assessmentyear == 2016 & grepl("mac", stockkeylabelold)) %>% View()
# filter(d      , assessmentyear == 2016 & grepl("mac", stockkeylabel)) %>% View()

# get the last assessment year and stock name
lastyear        <- unique(unlist(select(filter(d, assessmenttype=="last"), assessmentyear)))

last <-
  d %>% 
  filter(assessmenttype == "last") %>%
  select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, year, 
         lastssb = ssb, lowssb, highssb, 
         lastf=f, lowf, highf, 
         lastrecruitment = recruitment, lowrecruitment, highrecruitment, assessmenttype)

withdrawn <-
  d %>% 
  filter(assessmenttype == "withdrawn") %>% 
  select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, year, ssb, lowssb, highssb, lastf=f, lowf, highf, 
         lastrecruitment = recruitment, lowrecruitment, highrecruitment, assessmenttype)

# scale to last year ?
# d <-
#   d %>% 
#   left_join(last, by=c("fishstock","year")) %>% 
#   mutate(recruitment = recruitment/lastr,
#          ssb         = ssb/lastssb,
#          f           = f / lastf)

# plot ssb
p1 <-
  d %>% 
  filter(!is.na(ssb)) %>%  
  # filter(grepl("2016", tyear)) %>% 
  # filter(assessmenttype == "withdrawn") %>% 
  
  ggplot(aes(year,ssb, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(data=last, aes(x=year, ymin=lowssb, ymax=highssb, fill = assessmenttype), alpha=0.3, inherit.aes = FALSE) +
  geom_ribbon(data=withdrawn, aes(x=year, ymin=lowssb, ymax=highssb, fill = assessmenttype), alpha=0.3, inherit.aes = FALSE) +
  
  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual  (values=c(last = "red",update="black",assess = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",assess = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",assess = "solid",bench ="dashed",withdrawn = "dotdash",   alt="dotted")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,assess = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  +
  facet_grid(stockkeylabelold ~ ., scales="free_y")



# plot f
p2 <-
  d %>% 
  filter(!is.na(f)) %>%  
  
  ggplot(aes(year,f, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(data=last, aes(x=year, ymin=lowf, ymax=highf, fill = assessmenttype), alpha=0.3, inherit.aes = FALSE) +

  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual  (values=c(last = "red",update="black",assess = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",assess = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",assess = "solid",bench ="dashed",withdrawn = "dotdash",   alt="dotted")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,assess = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "F")   +
  facet_grid(stockkeylabelold ~ ., scales="free_y")

plot_grid(p1 + theme(legend.position  = "none", 
                     axis.title       = element_blank(),
                     strip.background = element_rect(colour=NA, fill=NA),
                     strip.text       = element_text(colour=NA)), 
          p2 + theme(axis.title       = element_blank()),
          ncol=2, align = 'h', rel_widths = c(3.5,3))


# filter(iAssess, stockkeylabelold == "cod-iceg") %>% View()


# recruitment
p3 <-
  d %>% 
  filter(!is.na(recruitment)) %>%  
  
  ggplot(aes(year,recruitment, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_ribbon(data=last, aes(x=year, ymin=lowrecruitment, ymax=highrecruitment, fill = assessmenttype), alpha=0.3, inherit.aes = FALSE) +
  
  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual  (values=c(last = "red",update="black",assess = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",assess = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",assess = "solid",bench ="dashed",withdrawn = "dotdash",   alt="dotted")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,assess = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "recruitment")   +
  facet_grid(stockkeylabelold ~ ., scales="free_y")


plot_grid(p1 + theme(legend.position  = "none", 
                     axis.title       = element_blank(),
                     strip.background = element_rect(colour=NA, fill=NA),
                     strip.text       = element_text(colour=NA)), 
          p2 + theme(axis.title       = element_blank()),
          p3 + theme(axis.title       = element_blank()),
          ncol=3, align = 'h', rel_widths = c(3.5,3))


# filter(iAssess, stockkeylabelold == "cod-iceg") %>% View()
