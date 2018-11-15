# -----------------------------------------------------------------------------------------------
# Analyse assessments per stock
#
# 25/09/2017 taken from ices SAG data plot
# 16/10/2017 now includes datapublished
# 15/11/2018 updated for new database layout
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours
library(lubridate)

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# Load dataset
load(file=paste(advicedir, "/rdata/iAssess.RData",sep=""))

# ---------------------------------------------------------------------------------------------
# plots of assessments by assessment year and purpose
# ---------------------------------------------------------------------------------------------

x <-
  iAssess %>% 
  mutate_at(c("stockkeylabel","stockkeylabelold"), funs(tolower)) %>% 
  filter(!grepl("nep", stockkeylabel)) %>% 
  filter(!grepl("^rj", stockkeylabel)) %>% 
  
  distinct(stockkey, stockkeylabel, stockkeylabelold, assessmentyear, purpose, published, source) %>% 
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  arrange(stockkeylabelold) %>% 
  mutate(id = group_indices(., stockkeylabelold)) %>% 
  data.frame() %>% 
  mutate(stockkeylabelold = factor(stockkeylabelold),
         stockkeylabelold = factor(stockkeylabelold, levels = rev(levels(stockkeylabelold))),
         purpose          = factor(purpose),
         purpose          = factor(purpose, levels=rev(levels(purpose))),
         source           = factor(source, levels=c("excel","sag")),
         column              = ifelse(id <=  90            , 1, NA),
         column              = ifelse(id  >  90 & id <= 180, 2, column),
         column              = ifelse(id  > 180            , 3, column),
         speciesfaocode   = substr(stockkeylabel, 1, 3)) %>% 
  left_join(iSpecies, by="speciesfaocode")

# define colour scales
mySourceColors        <- brewer.pal(max(length(levels(x$source)),3),"Set1")
names(mySourceColors) <- levels(x$source)

myPurposeColors        <- brewer.pal(length(levels(x$purpose)),"Set1")
names(myPurposeColors) <- levels(x$purpose)

# define headers for columns
y <-
  x %>% 
  group_by(column) %>% 
  filter(row_number()==1| row_number() == n()) %>% 
  ungroup() %>% 
  mutate(id = group_indices(., column)) %>% 
  select(column, id, stockkeylabelold) %>% 
  group_by(column) %>% 
  summarise(code = paste(stockkeylabelold, collapse=" : "))


# plot by stock and purpose
x %>% 
  left_join(y, by="column") %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(colour = "grey70"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = purpose)) +
  scale_colour_manual(name = "purpose", values = myPurposeColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)

# plot by stock and source
x %>% 
  left_join(y, by="column") %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(colour = "grey70"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = mySourceColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)


# Alternative plot below: by fisheries guild
p2a <-
  filter(x, fisheriesguild %in% c("pelagic")) %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = mySourceColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = NULL, y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2b <-
  filter(x, fisheriesguild %in% c("benthic")) %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = mySourceColors, na.value="lightgray") +
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
  ggplot(aes(x=assessmentyear, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        legend.position  = "right",
        legend.direction = "vertical",
        legend.title     = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = mySourceColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(y = "fishstock", x=" " ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE)

plot_grid(p2 + theme(legend.position="none") + theme(axis.title.y=element_blank()), 
          p3 ,
          ncol=2,
          align = 'v', rel_widths = c(1,1.2))

# unique(sagdb$fisheriesguild)
# unique(x$stockpublishnote)
# unique(x$assessmentcat)
# unique(x$status)



