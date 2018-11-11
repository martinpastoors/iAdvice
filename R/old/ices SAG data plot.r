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
# summarize dataset
# ---------------------------------------------------------------------------------------------

sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number()==1) %>% 
  filter(assessmentyear >= 2001) %>% 
  mutate(species = paste0(commonname," (", faocode,")"), sep="") %>% 
  group_by(species, assessmentyear) %>% 
  summarise(nassess = n()) %>% 
  
  dcast(species ~ assessmentyear, sum, value.var="nassess", margins = TRUE) %>% 
  no.emphasis.table() %>% 
  select(-1) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "right",
               missing=".",
               round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
               caption="number of assessments per species")



sagdb %>% 
  filter(assessmentyear >= 2001) %>% 
  filter(assessmentcat == "assess") %>% 
  group_by(fishstockold, assessmentyear) %>%
  summarise(nyears = n()) %>% 
  dcast(fishstockold ~ assessmentyear, value.var="nyears", fun.aggregate=sum, margins=TRUE) %>% 
  View()


# ---------------------------------------------------------------------------------------------
# Overviews
# ---------------------------------------------------------------------------------------------
overviews <-
  sagdb %>% 
  group_by(fishstockold, fishstocknew, assessmentyear) %>% 
  summarise(firstyear = min(year),
            lastyear  = max(year),
            nyear     = n())

overviews2 <-
  sagdb %>% 
  group_by(fishstockold, fishstocknew, assessmentyear, fisheriesguild) %>% 
  filter(row_number()==1) %>% 
  select(fishstockold, fishstocknew, assessmentyear, stocksizedescription, stocksizeunits, fisheriesguild) %>% 
  mutate(stocksize = paste(stocksizedescription,"(", stocksizeunits, ")")) %>% 
  group_by(fishstockold, fishstocknew, stocksize, fisheriesguild) %>% 
  arrange(fishstockold, fishstocknew, assessmentyear) %>% 
  summarise(years = paste(assessmentyear, collapse = " ")) 
  # spread(key=stocksize, value=years)

overviews3 <-
  sagdb %>% 
  group_by(fishstockold, fishstocknew, assessmentyear, assessmenttype) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  mutate(fishstock = ifelse(!is.na(fishstockold), fishstockold, fishstocknew)) %>% 
  select(fishstock, assessmentyear, assessmenttype) %>% 
  group_by(assessmenttype, assessmentyear) %>% 
  arrange(assessmenttype) %>% 
  # summarise(years = paste(assessmentyear, collapse = " ")) %>% 
  summarise(n = n()) %>% 
  spread(key=assessmentyear, value=n)

write.csv(overviews3, file="downloads/assessmenttypes.csv")

filter(overviews2, stocksize=="b/bmsy ( NA )")
filter(sagdb, stocksizedescription=="total biomass index" & is.na(stocksizeunits)) %>% View()

# ---------------------------------------------------------------------------------------------
# Checker of consistency
# ---------------------------------------------------------------------------------------------

checker <-
  sagdb %>% 
  # only use ssb in tonnes and f in year-1
  filter(stocksizeunits=="tonnes",
         fishingpressureunits=="year-1", 
         year >= 1990, 
         assessmenttype2 == "assess") %>% 
  filter(!is.na(ssb)) %>% 
  filter(assessmentyear >= 2005) %>% 
  group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
  summarise(ssb = mean(ssb, na.rm=TRUE),
            f   = mean(f  , na.rm=TRUE),
            ny  = n()) %>% 
  ungroup() %>% 
  data.frame() 

checker %>% 
  ggplot(aes(x=assessmentyear, y=ssb)) +
  theme_publication() +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(text = element_text(size=8)) +
  geom_line(aes(group=1)) +
  geom_point(aes(colour=factor(source))) +
  expand_limits(y=0)+
  scale_y_continuous(breaks=scales::pretty_breaks(n=2))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=2))+
  facet_wrap(~fishstockold, scales="free_y")

# ---------------------------------------------------------------------------------------------
# plots of assessments by assessment year and stock and fisheries guild (for a single variable)
# ---------------------------------------------------------------------------------------------

# To do: convert this into a function that can plot the different variables. I have struggled with this a lot but 
# without success. 1/8/2017

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
  
  # here is the bit to add the colour variable
  mutate(source = factor(source))

# define colour scale
myColors        <- brewer.pal(length(levels(x$source)),"Set1")
names(myColors) <- levels(x$source)

p1 <-
  filter(x, fisheriesguild %in% c("crustacean","elasmobranch")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = " ", y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2a <-
  filter(x, fisheriesguild %in% c("pelagic")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = NULL, y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2b <-
  filter(x, fisheriesguild %in% c("benthic")) %>% 
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
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
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(y = "fishstock", x=" " ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE)

plot_grid(p1 + theme(legend.position="none") + theme(axis.title.y=element_blank()), 
          p2 ,
          p3 ,
          ncol=3,
          align = 'v', rel_widths = c(1,1,1.2))

# unique(sagdb$fisheriesguild)
# unique(x$stockpublishnote)
# unique(x$assessmentcat)
# unique(x$status)


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
