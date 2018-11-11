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
# 27/06/2017 more plots for western horse mackerel
# ===========================================================================

# Reset lists
rm(list=ls())

library(directlabels)  # for printing labels at end of geom lines
library(readxl)
require(ggplot2)
library(dplyr)
library(tidyr)

# Set working directory
setwd("D:/Dropbox/ICES Assessment database")

# code for combining plots
source("r/multiplot.r")

# code for combining plots with single legend
source("r/grid_arrange_shared_legend.r")

# publication theme
source("r/theme_publication.r")


upcase <- function(df) {
  names(df) <- toupper(names(df)) %>% gsub("\\s+","_",.) 
  df
}

lowcase <- function(df) {
  names(df) <- tolower(names(df)) %>% gsub("\\s+","_",.) 
  df
}

# ===========================================================================
# Read and convert data; or load the dataframes
# ===========================================================================

load("../ICES advice database/rdata/IAD.RData")

iad <-
  iad %>% 
  lowcase 
  

# rby <-
#   read_excel("ICES Assessment Summary database.xlsx",
#              sheet = "DATA",
#              col_names = TRUE,
#              skip = 0) %>%
#   lowcase %>%
#   mutate_each(funs(as.numeric), c(low_f:high_f,
#                                   low_recruitment:high_recruitment,
#                                   low_ssb:high_ssb,
#                                   landings, catches,
#                                   year, assyear)) %>%
#   select(-matches("custom"))
 
# save(rby, file="ICES Assessment Summary database.RData")
load(file="ICES Assessment Summary database.RData")

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
  filter(year <= assyear)
# write.csv(rbyA,"rbyA.csv")



# ===========================================================================
# Historic retros: plot stock data over different assessment years 
# ===========================================================================


d <-
  rbyA %>% 
  # filter(fishstock %in% c("her-noss", "whb-comb","mac-nea","hom-west"),
  # filter(fishstock %in% c("mac-nea"),
  # filter(fishstock %in% c("hom-west"),
  filter(grepl("hom-west", fishstock)) %>% 
  # filter(fishstock %in% c("her-47d3"),
  filter(year      >  1980, 
         assyear   >= 2016,
         year      <= assyear) %>% 
  select(assyear, year, fishstock, low_recruitment:high_recruitment, low_f:high_f, low_ssb:high_ssb) %>% 
  data.frame()
  
# dataset with last year
# d.last <- d %>% filter(assyear == max(assyear))

# dataset from other years
# d <- d %>% filter(assyear != max(assyear)) %>% as.data.frame()

# plot labels yes or no
plotlabels <- FALSE

# ssb
p1 <-
  d %>% 
  filter(!is.na(ssb)) %>% 
  
  ggplot(aes(year,ssb, group=interaction(fishstock, assyear))) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.background = element_blank() ) +
  geom_ribbon(aes(ymin = low_ssb, 
                  ymax = high_ssb, 
                  fill = factor(interaction(fishstock, assyear)) ), 
              alpha=0.3 ) +
  
  geom_line(aes(colour = factor(interaction(fishstock,assyear))), size=1 )  +
  
  {if(plotlabels) geom_dl(aes(label  = substr(assyear,3,5),
                              colour = factor(interaction(fishstock,assyear)) ), 
                          method    = list(dl.combine("last.points"), 
                                           cex = 0.8)) } +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "ssb")   # +
  # facet_grid( fishstock ~ ., scales = "free_y", switch = "y")

# f
p2 <-
  d %>% 
  filter(!is.na(f)) %>% 
  
  ggplot(aes(year,f, group=interaction(fishstock, assyear))) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.background = element_blank() ) +
  geom_ribbon(aes(ymin = low_f, 
                  ymax = high_f, 
                  fill = factor(interaction(fishstock, assyear)) ), 
              alpha=0.3 ) +
  
  geom_line(aes(colour = factor(interaction(fishstock,assyear))), size=1 )  +
  
  {if(plotlabels) geom_dl(aes(label  = substr(assyear,3,5),
                              colour = factor(interaction(fishstock,assyear)) ), 
                          method    = list(dl.combine("last.points"), 
                                           cex = 0.8)) } +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "F")   # +
# facet_grid( fishstock ~ ., scales = "free_y", switch = "y")

# recruitment
p3 <-
  d %>% 
  filter(!is.na(recruitment)) %>% 
  
  ggplot(aes(year,recruitment, group=interaction(fishstock, assyear))) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.background = element_blank() ) +
  geom_ribbon(aes(ymin = low_recruitment, 
                  ymax = high_recruitment, 
                  fill = factor(interaction(fishstock, assyear)) ), 
              alpha=0.3 ) +
  
  geom_line(aes(colour = factor(interaction(fishstock,assyear))), size=1 )  +
  
  {if(plotlabels) geom_dl(aes(label  = substr(assyear,3,5),
                              colour = factor(interaction(fishstock,assyear)) ), 
                          method    = list(dl.combine("last.points"), 
                                           cex = 0.8)) } +
  expand_limits(y = 0) +
  # scale_y_log10() +
  labs(x = NULL, y = NULL , title = "Recruitment")   # +
# facet_grid( fishstock ~ ., scales = "free_y", switch = "y")

# stock and recruitment
# see further down

# bind together
grid_arrange_shared_legend(p1, p2, ncol = 2, nrow = 1)
grid_arrange_shared_legend(p1, p2, p3, ncol = 3, nrow = 1)


# check 2001 biomass
# d %>% 
#   filter(year==2001) %>% 
#   select(assyear, year, fishstock, ssb)

# calculate deviance
dev <-
  d %>%
  filter(fishstock == "hom-west-bench") %>% 
  select(assyear, year, fnew=f, rnew=recruitment, ssbnew=ssb) %>% 
  left_join(filter(d, fishstock=="hom-west", assyear==2016), by=c("assyear", "year")) %>% 
  mutate(r_dev = (rnew - recruitment)/recruitment,
         s_dev = (ssbnew-ssb)/ssb,
         f_dev = (fnew-f)/f)

s1 <-
  ggplot(dev, aes(year, s_dev, group=fishstock)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_bar(aes(fill=fishstock), stat="identity") +
#  scale_y_continuous(labels=scales::percent, limits=c(-1.25,1.25)) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="ssb deviance") 
  

s2 <-
  ggplot(dev, aes(year, f_dev)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_bar(aes(fill=fishstock), stat="identity") +
# scale_y_continuous(labels=scales::percent, limits=c(-1.25,1.25)) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="F deviance")

s3 <-
  ggplot(dev, aes(year, r_dev)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_bar(aes(fill=fishstock), stat="identity") +
  scale_y_continuous(labels=scales::percent, limits=c(-1.25,1.25)) +
  labs(title="R deviance")

layout <- matrix(c(1, 2), nrow = 3, byrow = TRUE)
multiplot(s1, s2, s3, layout=layout)

# plot with shared legend
grid_arrange_shared_legend(s1, s2, s3, ncol = 3, nrow = 1)

# plot SSB and deviance in one plot
multiplot(p1, s1, layout=matrix(c(1, 2), nrow = 1, byrow = TRUE))
multiplot(p2, s2, layout=matrix(c(1, 2), nrow = 1, byrow = TRUE))


# Plot with horse mackerel reference points
rp <-
  iad %>% 
  ungroup() %>% 
  filter(grepl("hom-west",stock), year==2016) %>% 
  select(year, stock, fpa, fmsy, blim, bpa, msybtrig) %>% 
  rbind(data.frame(year=2016,stock="hom-west-bench", fpa=0.108, fmsy=0.108, blim=662, bpa=912, msybtrig=912))

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
# stock and recruitment
# ===========================================================================

ra <- rbyA %>% filter(fishstock %in% c("hom-west"), assyear == 2016) %>% select(recruitmentage) %>% unique() %>% as.numeric()

p4 <-
  rbyA %>% 
  filter(grepl("hom-west", fishstock)) %>% 
  filter(assyear   %in% 2014:2016,
         year      <= assyear) %>% 
  select(assyear, year, fishstock, low_recruitment:high_recruitment, low_f:high_f, low_ssb:high_ssb, recruitmentage) %>% 
  arrange(interaction(fishstock,assyear), year) %>% 
  group_by(interaction(fishstock, assyear)) %>% 
  # mutate(recruitment      = lag(recruitment, n=ra )) %>% 
  # mutate(low_recruitment  = lag(low_recruitment, n=ra)) %>% 
  # mutate(high_recruitment = lag(high_recruitment, na=ra )) %>%
  # View()
  ggplot(aes(ssb,recruitment, group=interaction(fishstock,assyear))) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        strip.background = element_blank() ) +
  geom_point(aes(colour=factor(interaction(fishstock,assyear))), alpha=0.5) +
  geom_errorbar(aes(ymax=high_recruitment, ymin=low_recruitment, colour=factor(interaction(fishstock,assyear))),
                width=0.2) +
  geom_errorbarh(aes(xmax=high_ssb, xmin=low_ssb, colour=factor(interaction(fishstock,assyear)))) +
  expand_limits(x=0,y = 0) +
  labs(x = "SSB", y = "Recruitment" , title = "SRR") 


grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)


# ===========================================================================
# trends per ecoregion
# ===========================================================================

d <-
  rbyA %>% 
  filter(year      %in% 1980:2016, 
         assyear   %in% c(2016)) %>% 
  mutate_each(funs(./1000000), low_ssb:high_ssb) %>% 
  mutate_each(funs(./1000000), low_recruitment:high_recruitment) %>% 
  data.frame() 


p1 <-
  d %>% 
  filter(species %in% c("her", "mac", "whb", "hom", "spr", "san") ) %>% 
  
  ggplot(aes(year,ssb, group=assyear)) +
  theme_bw() +
  geom_line(colour="black") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_text(colour="black", size=7),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "ssb" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=5)

p1 <-
  d %>% 
  filter(fishstock %in% c("her-3a22", "her-47d3", "her-67bc", "mac-nea", "whb-comb", "spr-nsea") ) %>% 
  
  ggplot(aes(year,ssb, group=assyear)) +
  theme_bw() +
  geom_line(colour="black") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_text(colour="black", size=7, hjust=0),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "ssb" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=1)

p2 <-
  d %>% 
  filter(fishstock %in% c("her-3a22", "her-47d3", "her-67bc", "mac-nea", "whb-comb", "spr-nsea") ) %>% 
  
  ggplot(aes(year,f, group=assyear)) +
  theme_bw() +
  geom_line(colour="black") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_blank(),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "F" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=1)

p3 <-
  d %>% 
  filter(fishstock %in% c("her-3a22", "her-47d3", "her-67bc", "mac-nea", "whb-comb", "spr-nsea") ) %>% 
  
  ggplot(aes(year,recruitment, group=assyear)) +
  theme_bw() +
  geom_line(colour="black") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_blank(),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "recruitment" ) ) +
  facet_wrap(  ~ fishstosck, scales = "free_y", ncol=1)

# bind together
# layout <- matrix(c(1, 2), ncol = 3, byrow = TRUE)
multiplot(p1, p2, p3, cols=3)

# ===========================================================================
# trends for pelagic and demersal
# ===========================================================================

# create stocktype dataset
stocktype <-
  iad %>% 
  filter(year == 2016) %>%
  group_by(stock) %>% 
  filter(row_number()==1) %>% 
  select(species, fishstock=stock, stocktype)


d <-
  rbyA %>% 
  filter(year      %in% 1988:2016, 
         assyear   %in% c(2015)) %>% 
  mutate_each(funs(./1000000), low_ssb:high_ssb) %>% 
  mutate_each(funs(./1000000), low_recruitment:high_recruitment) %>% 
  left_join(stocktype, by=c("fishstock")) %>% 
  filter(species.x %in% c("her","mac","hom","whb",
                        "cod","had","sai","ple","sol", "hke")) %>% 
  filter(!grepl("bench$|old$", fishstock)) %>% 
  filter(ssb != 0) %>% 
  data.frame() 

# unique(d$fishstock)
# filter(rbyA, species=="mac") %>% View()

d %>% 
  group_by(year, stocktype,fishstock, species.x) %>% 
  summarize(ssb = sum(ssb, na.rm=TRUE),
            f   = mean(f, na.rm=TRUE) ) %>% 
  ggplot(aes(year,ssb)) +
  theme_publication() +
  theme(panel.border     = element_rect(colour="black" , size=0.2),
        panel.grid.major = element_blank(),
        strip.background = element_rect(colour="black", size =0.2),
        plot.margin      = unit(c(0,0,0.5,0),"mm")
  ) +
  geom_bar(aes(colour=species.x, fill=species.x), stat="identity", position="stack", alpha=0.7) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL, title=paste( "ssb" ) ) +
  facet_wrap(  ~ stocktype)

d %>% 
  group_by(year, stocktype) %>% 
  summarize(f   = mean(f, na.rm=TRUE) ) %>% 
  ggplot(aes(year,f)) +
  theme_publication() +
  theme(panel.border     = element_rect(colour="black" , size=0.2),
        panel.grid.major = element_blank(),
        strip.background = element_rect(colour="black", size =0.2),
        plot.margin      = unit(c(0,0,0.5,0),"mm")
  ) +
  geom_line(aes(colour=factor(stocktype)),size=1.5) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL, title=paste( "F" ) ) +
  facet_wrap(  ~ stocktype)

p1 <-
  d %>% 
  filter(fishstock %in% c("her-3a22", "her-47d3", "her-67bc", "mac-nea", "whb-comb", "spr-nsea") ) %>% 
  
  ggplot(aes(year,ssb, group=assyear)) +
  theme_bw() +
  geom_line(colour="black") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_text(colour="black", size=7, hjust=0),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "ssb" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=1)

p2 <-
  d %>% 
  filter(fishstock %in% c("her-3a22", "her-47d3", "her-67bc", "mac-nea", "whb-comb", "spr-nsea") ) %>% 
  
  ggplot(aes(year,f, group=assyear)) +
  theme_bw() +
  geom_line(colour="black") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_blank(),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "F" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=1)

p3 <-
  d %>% 
  filter(fishstock %in% c("her-3a22", "her-47d3", "her-67bc", "mac-nea", "whb-comb", "spr-nsea") ) %>% 
  
  ggplot(aes(year,recruitment, group=assyear)) +
  theme_bw() +
  geom_line(colour="black") +
  expand_limits(y = 0) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_blank(),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "recruitment" ) ) +
  facet_wrap(  ~ fishstosck, scales = "free_y", ncol=1)

# bind together
# layout <- matrix(c(1, 2), ncol = 3, byrow = TRUE)
multiplot(p1, p2, p3, cols=3)
