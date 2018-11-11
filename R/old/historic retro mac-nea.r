# ===========================================================================
# R-script for ICES assessment summary 1982-2016
# Martin Pastoors

# Original:09/07/2014
# updated: 08/09/2014
# updated: 10/03/2015
# updated: 05/04/2016
# 10/04/2016 removed forecast years to separate file
# 04/09/2016 updated during WGWIDE
# 24/02/2014 adapted for all lowercase
# ===========================================================================

# Reset lists
rm(list=ls())

library(readxl)
require(ggplot2)
library(dplyr)
library(tidyr)
library(directlabels)  # for printing labels at end of geom lines

# Set working directory
setwd("C:/Users/Martin Pastoors/Documents/Dropbox/ICES Assessment database")

# code for combining plots
source("r/multiplot.r")
source("r/theme_publication.r")

# ===========================================================================
# Read and convert data; or load the dataframe
# ===========================================================================

load("ICES Assessment Summary database.RData")

# rby <-
#   read_excel("ICES Assessment Summary database.xlsx",
#              sheet = "DATA",
#              col_names = TRUE,
#              skip = 0) %>%
#   mutate_each(funs(as.numeric), c(F, Recruitment, SSB,
#                                   Landings, Catches,
#                                   Year, AssYear))
# 
# names(rby) <- tolower(names(rby))

# save(rby, file="ICES Assessment Summary database.RData")

# Store the forecast years
rbyF <-
  rby %>% 
  tbl_df() %>% 
  filter(year >= assyear) %>% 
  mutate(forecastyear = year - assyear)
# write.csv(rbyF,"rbyF.csv")

# Store the assessments
rbyA <-
  rby %>% 
  tbl_df() %>% 
  filter(year <= assyear)
# write.csv(rbyA,"rbyA.csv")

# ========================================================================================
# Historic retros: plot stock data over different assessment years and assessment versions
# ========================================================================================

d <-
  rbyA %>% 
  filter(fishstock %in% c("mac-nea", "mac-nea-old","mac-nea-bench"),
         year      >  1990, 
         assyear   >= 2010,
         year      <= assyear) %>% 
  select(assyear, year, fishstock, ssb, f, recruitment,
         low_ssb, high_ssb) %>% 
  gather(key=variable, value, ssb:high_ssb) %>% 
  mutate(value = ifelse(grepl("ssb", variable), value/1000000, value),
         value = ifelse(variable == "recruitment", value/1000000, value),
         value = ifelse(variable == "f" & year == assyear, NA, value) ) %>% 
  arrange(variable) %>% 
  data.frame()
  
  
# dataset with final year's assessment
d.last  <- d %>% filter(assyear == max(assyear), fishstock == "mac-nea")

# dataset from other years
d.ass    <- d %>% filter(assyear != max(assyear), fishstock == "mac-nea") %>% as.data.frame()

# dataset with benchmarks
d.bench <- d %>% filter(fishstock == "mac-nea-bench")

# dataset with benchmarks
d.old <- d %>% filter(fishstock == "mac-nea-old")

d.ribbon <- spread(filter(d.last, grepl("ssb", variable), nchar(variable)>3), key=variable, value=value)

# unique(d.old$assyear)
# filter(rby, fishstock=="mac-nea-old") %>% View()

# plot labels yes or no
plotlabels <- TRUE
plotribbon <- TRUE

p1 <-
  d.ass %>% 
  filter(variable == "ssb") %>% 
  ggplot(aes(year,value, group=assyear)) +
  theme_bw() +
  theme(text = element_text(size=16)) +
  geom_line(lwd=0.5) +
  
  geom_line(data = filter(d.bench,variable=="ssb"), col = "blue"  , lwd = .5) +
  geom_line(data = filter(d.old,variable=="ssb")  , col = "gray15", lwd = .5, linetype=2) +
  geom_line(data = filter(d.last,variable=="ssb") , col = "red"   , lwd =  1) +

  { if(plotribbon)       
    geom_ribbon(data= d.ribbon ,
                aes(x    = year,
                    ymin = low_ssb, 
                    ymax = high_ssb),
                inherit.aes = FALSE,
                colour      = "gray80",
                alpha       = 0.3)   }  +
  
  { if(plotlabels)  
    geom_dl(aes(label = substr(assyear,3,5)), 
            method    = list(dl.combine("last.points"), cex = 0.8)) } +
  { if(plotlabels)   
    geom_dl(data= filter(d.last,variable=="ssb"),
            aes(label = substr(assyear,3,5)), 
            method    = list(dl.combine("last.points"), cex = 0.8),
            colour    = "red")                                     } +
  { if(plotlabels)     
    geom_dl(data= filter(d.bench,variable=="ssb"),
            aes(label = substr(assyear,3,5)), 
            method    = list(dl.combine("last.points"), cex = 0.8),
            colour    = "blue")                                    } +
  { if(plotlabels)       
    geom_dl(data= filter(d.old,variable=="ssb"),
            aes(label = substr(assyear,3,5)), 
            method    = list(dl.combine("last.points"), cex = 0.8),
            colour    = "gray30")                                  }  +
  
  expand_limits(y = 0) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.text.y = element_blank() , 
        strip.background = element_blank(),
        plot.margin = unit( c(0,0,0,0.2) , units = "lines" ) ) +
  labs(x = NULL, y = NULL , title = "ssb") 

p2 <-
  d.ass %>% 
  filter(variable == "f") %>% 
  ggplot(aes(year,value, group=assyear)) +
  theme_bw() +
  geom_line(lwd = .5) +
  
  geom_line(data = filter(d.bench,variable=="f"), col = "blue"  , lwd = .5) +
  geom_line(data = filter(d.old  ,variable=="f"), col = "gray15", lwd = .5, linetype=2) +
  geom_line(data = filter(d.last ,variable=="f"), col = "red"   , lwd =  1) +
  
  expand_limits(y = 0) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.text.y = element_blank() , 
        strip.background = element_blank(),
        plot.margin = unit( c(0,0,0,0.2) , units = "lines" )  ) +
  labs(x = NULL, y = NULL , title = "f") 

# p3 <-
#   d.ass %>% 
#   filter(variable == "recruitment") %>% 
#   filter(!(fishstock == "her-noss" & variable == "recruitment" & assyear < 2016)) %>% 
#   ggplot(aes(year,value, group=assyear)) +
#   theme_bw() +
#   geom_line(lwd=1) +
#   
#   geom_line(data = filter(d.bench,variable=="recruitment"), col = "blue", lwd = 1) +
#   geom_line(data = filter(d.old  ,variable=="recruitment"), col = "gray", lwd = 1, linetype=2) +
#   geom_line(data = filter(d.last ,variable=="recruitment"), col = "red", lwd = 1) +
#   
#   expand_limits(y = 0) +
#   theme(legend.title = element_blank(),
#         axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
#         axis.text.y = element_text(size=9),
#         plot.margin = unit( c(0,0,0,0.2) , units = "lines" )) +
#   labs(x = NULL, y = NULL , title = "recruitment")

# bind together
layout <- matrix(c(1, 2), ncol = 2, byrow = TRUE)
multiplot(p1, p2, layout=layout)

# layout <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
# multiplot(p1, p2, p3, layout=layout)

