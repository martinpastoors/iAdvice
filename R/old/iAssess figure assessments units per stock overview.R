# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph data plotting
#
# 01/08/2017 extracted code from SAG download code
# 11/08/2017 adapted for R 3.4.1
# 28/08/2017 small modification to variable names
# -----------------------------------------------------------------------------------------------

setwd("D:/Dropbox/ICES Assessment database")

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours

# Load utils code
source("D:/XXX/PRF/r/my_utils.r")

# Load dataset
load(file="rdata/sagdb.RData")

# ---------------------------------------------------------------------------------------------
# plots of ssb or f descriptions by assessment year and stock and fisheries guild
# ---------------------------------------------------------------------------------------------

myvar  <- "stocksizedescription"
myvar2 <- "ssb"

myvar  <- "fishingpressuredescription"
myvar2 <- "f"

x <-
  sagdb %>% 
  filter(assessmentyear >= 1990) %>% 
  filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 
  filter(fisheriesguild %in% c("crustacean","elasmobranch","pelagic","demersal","benthic")) %>%
  mutate(fishstockold = ifelse(is.na(fishstockold)|fishstockold=="", fishstocknew, fishstockold)) %>% 
  group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
  data.frame() %>% 
  mutate(fishstockold = factor(fishstockold), 
         fishstockold = factor(fishstockold, levels = rev(levels(fishstockold)))) %>% 

  # here is the variable bit with myvar and myvar2
  mutate_(myvar = ifelse(is.na(myvar2), NA, myvar))

# x <-
#   sagdb %>% 
#   filter(assessmentyear >= 1990) %>% 
#   filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 
#   filter(fisheriesguild %in% c("crustacean","elasmobranch","pelagic","demersal","benthic")) %>%
#   mutate(fishstockold = ifelse(is.na(fishstockold)|fishstockold=="", fishstocknew, fishstockold)) %>% 
#   group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
#   data.frame() %>% 
#   mutate(fishstockold = factor(fishstockold), 
#          fishstockold = factor(fishstockold, levels = rev(levels(fishstockold))),
#          stocksizedescription = ifelse(is.na(ssb), NA, stocksizedescription))

# define colour scale
myColors        <- brewer.pal(length(levels(factor(unlist(dplyr::select(x, eval(myvar)))))),"Set1")
names(myColors) <- levels(factor(unlist(dplyr::select(x, eval(myvar)))))

p1 <-
  filter(x, fisheriesguild %in% c("crustacean","elasmobranch")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour=factor(substitute(myvar)))) +
  scale_y_discrete(position="right") +
  labs(x = " ", y = NULL ) +
  scale_colour_manual(name = myvar,values = myColors, na.value="lightgray") +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2a <-
  filter(x, fisheriesguild %in% c("pelagic")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour=factor(substitute(myvar)))) +
  scale_y_discrete(position="right") +
  labs(x = NULL, y = NULL ) +
  scale_colour_manual(name = myvar,values = myColors, na.value="lightgray") +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2b <-
  filter(x, fisheriesguild %in% c("benthic")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour=factor(substitute(myvar)))) +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  scale_colour_manual(name = myvar,values = myColors, na.value="lightgray") +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2 <- plot_grid(p2a + 
                  theme(legend.position="none", 
                        axis.title.y=element_blank(), 
                        axis.text.x=element_blank(), 
                        plot.margin = unit(c(10, 5, 1, 5), "mm")), 
                p2b + 
                  theme(legend.position="none", 
                        axis.title.y=element_blank(), 
                        plot.margin = unit(c(1, 5, 5, 5), "mm")),
                ncol=1,align="v",
                rel_heights = c(4.7, 2))

p3 <-
  filter(x, fisheriesguild %in% c("demersal")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        legend.position  = "right",
        legend.direction = "vertical",
        legend.title     = element_blank()) +
  
  geom_point(aes(colour=factor(substitute(myvar)))) +
  scale_y_discrete(position="right") +
  labs(y = "fishstock", x=" " ) +
  scale_colour_manual(name = myvar,values = myColors, na.value="lightgray") +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE)

plot_grid(p1 + theme(legend.position="none") + theme(axis.title.y=element_blank()), 
          p2 ,
          p3 ,
          ncol=3,
          align = 'v', rel_widths = c(1,1,1.2))
