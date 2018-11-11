# ===========================================================================
# R-script for ICES assessment summary 1982-2016
# Martin Pastoors

# Original:09/07/2014
# updated: 08/09/2014
# updated: 10/03/2015
# updated: 05/04/2016
# 10/04/2016 removed forecast years to separate file
# 04/09/2016 updated during WGWIDE
# 24/02/2014 adapted for all lowercase; added directlabels (optional)
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
# 
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

# ===========================================================================
# Historic retros: plot stock data over different assessment years 
# ===========================================================================


d <-
  rbyA %>% 
  filter(fishstock %in% c("her-noss", "whb-comb","mac-nea","hom-west"),
  # filter(fishstock %in% c("mac-nea"),
  # filter(fishstock %in% c("hom-west"),
         year      >  1990, 
         assyear   >= 2010,
         year      <= assyear) %>% 
  select(assyear, year, fishstock, ssb, f, recruitment) %>% 
  gather(key=variable, value, ssb:recruitment) %>% 
  mutate(value = ifelse(variable == "ssb", value/1000000, value),
         value = ifelse(variable == "recruitment", value/1000000, value),
         value = ifelse(variable == "f" & year == assyear, NA, value),
         variable=factor(variable, levels=c("ssb", "f", "recruitment"))) %>% 
  arrange(variable) %>% 
  data.frame()
  
  
# dataset with last year
d.last <- d %>% filter(assyear == max(assyear))

# dataset from other years
d <- d %>% filter(assyear != max(assyear)) %>% as.data.frame()

# plot labels yes or no
plotlabels <- TRUE

# start preparing plots
p1 <-
  d %>% 
  filter(variable == "ssb") %>% 
  ggplot(aes(year,value, group=assyear)) +
  theme_bw() +
  geom_line() +
  geom_line(data = filter(d.last,variable=="ssb"), col = "red", lwd = 1) +
  
  {if(plotlabels) geom_dl(aes(label = substr(assyear,3,5)), 
                      method    = list(dl.combine("last.points"), cex = 0.8)) }+
  {if(plotlabels) geom_dl(data= filter(d.last,variable=="ssb"),
                          aes(label = substr(assyear,3,5)), 
                          method    = list(dl.combine("last.points"), cex = 0.8),
                          colour    = "red") } +
  
  expand_limits(y = 0) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.text.y = element_blank() , 
        strip.background = element_blank(),
        plot.margin = unit( c(0,0,0,0.2) , units = "lines" ) ) +
  labs(x = NULL, y = NULL , title = "ssb") +
  facet_grid( fishstock ~ ., scales = "free_y", switch = "y")

p2 <-
  d %>% 
  filter(variable == "f") %>% 
  ggplot(aes(year,value, group=assyear)) +
  theme_bw() +
  
  geom_line() +
  geom_line(data = filter(d.last,variable=="f"), col = "red", lwd = 1) +
  
  {if(plotlabels) geom_dl(aes(label = substr(assyear,3,5)), 
                          method    = list(dl.combine("last.points"), cex = 0.8)) }+
  {if(plotlabels) geom_dl(data= filter(d.last,variable=="f"),
                          aes(label = substr(assyear,3,5)), 
                          method    = list(dl.combine("last.points"), cex = 0.8),
                          colour    = "red") } +
  
  expand_limits(y = 0) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.text.y = element_blank() , 
        strip.background = element_blank(),
        plot.margin = unit( c(0,0,0,0.2) , units = "lines" )  ) +
  labs(x = NULL, y = NULL , title = "f") +
  facet_grid( fishstock ~ ., scales = "free_y")

p3 <-
  d %>% 
  filter(variable == "recruitment") %>% 
  filter(!(fishstock == "her-noss" & variable == "recruitment" & assyear < 2016)) %>% 
  ggplot(aes(year,value, group=assyear)) +
  theme_bw() +
  geom_line() +
  geom_line(data = filter(d.last,variable=="recruitment"), col = "red", lwd = 1) +
  
  {if(plotlabels) geom_dl(aes(label = substr(assyear,3,5)), 
                          method    = list(dl.combine("last.points"), cex = 0.8)) }+
  {if(plotlabels) geom_dl(data= filter(d.last,variable=="recruitment"),
                          aes(label = substr(assyear,3,5)), 
                          method    = list(dl.combine("last.points"), cex = 0.8),
                          colour    = "red") } +
  
  expand_limits(y = 0) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.text.y = element_blank() , 
        plot.margin = unit( c(0,0,0,0.2) , units = "lines" )) +
  labs(x = NULL, y = NULL , title = "recruitment") +
  facet_grid( fishstock ~ ., scales = "free_y")

# bind together
layout <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)
multiplot(p1, p2, p3, layout=layout)


# ===========================================================================
# historic comparisons: plot stock data for a single year
# ===========================================================================

d <-
  rbyA %>% 
  filter(fishstock %in% c("her-noss", "whb-comb","mac-nea","hom-west"),
         assyear   %in% 2015:2016) %>% 
  select(assyear, year, fishstock, ssb, f, recruitment) %>% 
  gather(key=variable, value, ssb:recruitment) %>% 
  mutate(value = ifelse(variable == "ssb", value/1000000, value),
         value = ifelse(variable == "recruitment", value/1000000, value),
         value = ifelse(variable == "f" & year == assyear, NA, value),
         variable=factor(variable, levels=c("ssb", "f", "recruitment"))) %>% 
  arrange(variable) %>% 
  data.frame()

p1 <-
  d %>% 
  filter(variable == "ssb", year == 2015) %>% 
  ggplot(aes(factor(year),value, fill=factor(assyear))) +
  theme_bw() +
  geom_bar(stat = "identity", position="dodge", width=0.5, colour="black") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "NULL") +
  labs(x = NULL, y = NULL , title = "ssb") +
  facet_grid( fishstock ~ ., scales = "free_y")

p2 <-
  d %>% 
  filter(variable == "f", year == 2014) %>% 
  ggplot(aes(factor(year),value, fill=factor(assyear))) +
  theme_bw() +
  geom_bar(stat = "identity", position="dodge", width=0.5, colour="black") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9)) +
  labs(x = NULL, y = NULL , title = "f") +
  facet_grid( fishstock ~ ., scales = "free_y")

# bind together
layout <- matrix(c(1, 2), ncol = 2, byrow = TRUE)
multiplot(p1, p2, layout=layout)

# ===========================================================================
# trends per ecoregion
# ===========================================================================

d <-
  rbyA %>% 
  filter(year      %in% 1980:2016, 
         assyear   %in% c(2016)) %>% 
  select(assyear, year, ecoregion, fishstock, species, ssb, f, recruitment) %>% 
  gather(key=variable, value, ssb:recruitment) %>% 
  mutate(value = ifelse(variable == "ssb", value/1000000, value),
         value = ifelse(variable == "recruitment", value/1000000, value),
         value = ifelse(variable == "f" & year == assyear, NA, value),
         variable=factor(variable, levels=c("ssb", "f", "recruitment"))) %>% 
  arrange(variable) %>% 
  data.frame() 


d %>% 
  filter(variable %in% c("ssb"),
         species %in% c("her", "mac", "whb", "hom"),
         !is.na(value) ) %>% 
  
  ggplot(aes(year,value, group=assyear)) +
  theme_bw() +
  geom_line(colour="red") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_text(size=7, colour="black") , 
        strip.background = element_blank(),
        panel.border     = element_rect(colour="black"),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "ssb" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=5)

d %>% 
  filter(variable %in% c("f"),
         !is.na(value)) %>% 
  
  ggplot(aes(year,value, group=assyear)) +
  theme_bw() +
  geom_line(colour="blue") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_blank() , 
        strip.background = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "f" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=8)
