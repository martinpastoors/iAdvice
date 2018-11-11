# -----------------------------------------------------------------------------------------------
# ICES SAG checker
#
# 10/08/2017 created code from ICES SAG download.r
# -----------------------------------------------------------------------------------------------

setwd("D:/Dropbox/ICES Assessment database")

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots

# Load utils code
source("D:/XXX/PRF/r/my_utils.r")

load(file="rdata/sagdb.RData")

count_not_na(sagdb)

sort(unique(sagdb$assessmentcat))
sort(unique(sagdb$stocksizedescription))
sort(unique(sagdb$stocksizeunits))
sort(unique(sagdb$fishingpressuredescription))
sort(unique(sagdb$fishingpressureunits))
sort(unique(sagdb$assesstype))
sort(unique(sagdb$assessmenttype))
sort(unique(sagdb$assessmentmodel))

sagdb %>% 
  filter(assessmentmodel %in% c("xsa","ica","vpa","ls","sam","xsm", "tsa", "trends only", "surba","sms")) %>% 
  View()


# check assessmentypes used
sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number() == 1) %>% 
  filter(!is.na(assesstype)) %>% 
  select(fishstock, assessmentyear, stockpublishnote, status, assesstype) %>% 
  View()

# check stocksizedescriptions used
sagdb %>% 
  group_by(fishstock, assessmentyear) %>% 
  filter(row_number() == 1) %>% 
  group_by(stocksizedescription) %>% 
  summarise(n = n()) %>% 
  arrange(stocksizedescription) %>% 
  View()

filter(sagdb, is.na(commonname)) %>% View()

sort(unique(sagdb$stocksizeunits))
filter(sagdb, is.na(fishstocknew)) %>% View()
filter(sagdb, is.na(commonname)) %>% View()
filter(sagdb, is.na(stocksizeunits)) %>% View()
filter(sagdb, stocksizedescription == "stock size" & stocksizeunits == "tonnes") %>% View()
filter(sagdb, grepl("ratio", stocksizeunits)) %>% View()

filter(sagdownload, grepl("cod-arct", fishstockold)) %>% View()
filter(sagexcel, grepl("cod-arct", fishstockold)) %>% View()
filter(sagexcel_toadd, grepl("cod-arct", fishstockold)) %>% View()
filter(sagdb, grepl("cod-arct", fishstockold)) %>% group_by(assessmentyear) %>% filter(row_number()==1) %>% 
  arrange(assessmentyear) %>% View()

filter(sagdb, grepl("mac-nea", fishstockold), assessmentyear==2013) %>% View()
filter(sagexcel_toadd, grepl("her-47d3", fishstock), assessmentyear==2017) %>% View()
filter(sagdownload, grepl("mac-nea", fishstock), assessmentyear==2013) %>% View()

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
               round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))



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

filter(overviews2, stocksize=="b/bmsy ( NA )")
filter(sagdb, stocksizedescription=="total biomass index" & is.na(stocksizeunits)) %>% View()

# ---------------------------------------------------------------------------------------------
# Checker of consistency
# ---------------------------------------------------------------------------------------------

checker <-
  sagdb %>% 
  # only use ssb in tonnes and f in year-1
  filter(stocksizeunits=="tonnes",fishingpressureunits=="year-1", year >= 1990, assessmentcat == "assess") %>% 
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
  
p1 <-
  sagdb %>% 
  filter(assessmentyear >= 1990) %>% 
  filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 
  
  filter(fisheriesguild %in% c("crustacean","elasmobranch")) %>% 
  
  mutate(fishstockold = ifelse(is.na(fishstockold)|fishstockold=="", fishstocknew, fishstockold)) %>% 
  
  group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
  data.frame() %>% 
  mutate(fishstockold = factor(fishstockold), 
         fishstockold = factor(fishstockold, levels = rev(levels(fishstockold)))) %>% 
  
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  
  geom_point(aes(colour=factor(source))) +
  scale_y_discrete(position="right") +
  
  labs(x = " ", y = NULL ) +

  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2a <-
  sagdb %>% 
  filter(assessmentyear >= 1990) %>% 
  filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 
  filter(fisheriesguild %in% c("pelagic")) %>% 
  
  mutate(fishstockold = ifelse(is.na(fishstockold)|fishstockold=="", fishstocknew, fishstockold)) %>% 
  
  group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
  data.frame() %>% 
  mutate(fishstockold = factor(fishstockold), 
         fishstockold = factor(fishstockold, levels = rev(levels(fishstockold)))) %>% 
  
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  
  geom_point(aes(colour=factor(source))) +
  scale_y_discrete(position="right") +
  
  labs(x = NULL, y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2b <-
  sagdb %>% 
  filter(assessmentyear >= 1990) %>% 
  filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 
  filter(fisheriesguild %in% c("benthic")) %>% 
  
  mutate(fishstockold = ifelse(is.na(fishstockold)|fishstockold=="", fishstocknew, fishstockold)) %>% 
  
  group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
  data.frame() %>% 
  mutate(fishstockold = factor(fishstockold), 
         fishstockold = factor(fishstockold, levels = rev(levels(fishstockold)))) %>% 
  
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  
  geom_point(aes(colour=factor(source))) +
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
  sagdb %>% 
  filter(assessmentyear >= 1990) %>% 
  filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 

  mutate(fishstockold = ifelse(is.na(fishstockold), fishstocknew, fishstockold)) %>% 
  
  filter(fisheriesguild %in% c("demersal")) %>% 
  
  group_by(fishstockold, fishstocknew, assessmentyear, source) %>% 
  data.frame() %>% 
  mutate(fishstockold = factor(fishstockold), 
         fishstockold = factor(fishstockold, levels = rev(levels(fishstockold)))) %>% 
  
  ggplot(aes(x=assessmentyear, y=fishstockold)) +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        legend.position  = "right",
        legend.direction = "vertical",
        legend.title     = element_blank()) +
  
  geom_point(aes(colour=factor(source))) +
  scale_y_discrete(position="right") +

  labs(y = "fishstock", x=" " ) +
  
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE)

plot_grid(p1 + theme(legend.position="none") + theme(axis.title.y=element_blank()), 
          p2 ,
          p3 ,
          ncol=3,
          align = 'v', rel_widths = c(1,1,1.2))

unique(sagdb$fisheriesguild)


# Check NOSS herring
x <- 1990
sagdb %>% 
  filter(fishstockold == "her-noss", assessmentyear == 2016) %>% 
  ggplot(aes(x=year, y=recruitment)) +
  geom_bar(stat="identity") +
  geom_vline(aes(xintercept=x)) +
  geom_vline(aes(xintercept=x+8)) +
  geom_vline(aes(xintercept=x+16)) +
  geom_vline(aes(xintercept=x+24)) 

