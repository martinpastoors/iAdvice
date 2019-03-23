# -----------------------------------------------------------------------------------------------
# Analyse assessments per stock
#
# 25/09/2017 taken from ices SAG data plot
# 16/10/2017 now includes datapublished
# 15/11/2018 updated for new database layout
# 23/03/2019 updated for new database again; only source is working so far
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours
library(lubridate)
# library(ggstance)  # extension to ggplot; vertical extensions; only works for ggplot >= 3.0.0

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iSpecies.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iSAG.RData",sep=""))

load(file=paste(dropboxdir, "/rdata/iRename.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iStockkey.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iSpecies.RData",sep=""))

# ---------------------------------------------------------------------------------------------
# plots from iAssess by assessment year and purpose
# ---------------------------------------------------------------------------------------------

splitter <- 80

x <-
  # iAdvice %>% 
  iAssess %>% 
  # iSAGstock %>% 
  # dplyr::select(-stockkey, -icesareas) %>% 
  # left_join(iRename[,c("stockkeylabel","stockkey")], by="stockkeylabel") %>%
  # left_join(iStockkey, by="stockkey") %>% 
  
  mutate_at(c("stockkeylabel","stockkeylabelold"), funs(tolower)) %>% 
  filter(!grepl("nep", stockkeylabel)) %>% 
  filter(!grepl("^rj", stockkeylabel)) %>% 
  
  # filter(speciesfaocode %in% c("cod","her","hom","mac","whb","ple","sol")) %>% 
  # filter(speciesfaocode %in% c("mac")) %>% 
  # filter(speciesfaocode %in% c("her")) %>% 
  
  distinct(stockkey, stockkeylabel, stockkeylabelold, speciesfaocode, assessmentyear, purpose, published, source) %>% 
  # distinct(stockkey, stockkeylabel, stockkeylabelold, speciesfaocode, assessmentyear, purpose, published) %>% 
  
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  arrange(stockkeylabelold) %>% 
  mutate(id = group_indices(., stockkeylabelold)) %>% 
  data.frame() %>% 
  mutate(stockkeylabelold = factor(stockkeylabelold),
         stockkeylabelold = factor(stockkeylabelold, levels = rev(levels(stockkeylabelold))),
         purpose          = factor(purpose),
         purpose          = factor(purpose, levels=rev(levels(purpose))),
         published          = factor(published),
         published          = factor(published, levels=rev(levels(published))),
         column              = ifelse(id <=  splitter            , 1, NA),
         column              = ifelse(id  >  splitter & id <= 2*splitter, 2, column),
         column              = ifelse(id  > 2*splitter            , 3, column)) %>% 
  
  mutate(source           = factor(source, levels=c("wg","qcs", "excel","sag"))) %>% 
  #mutate(source = "SAG") %>% 
  left_join(iSpecies, by="speciesfaocode")


# define colour scales for source
mySourceColors        <- brewer.pal(max(length(levels(x$source)),3),"Set1")
names(mySourceColors) <- levels(x$source)

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

# plot by stock and source
x %>% 
  left_join(y, by="column") %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold, group=source)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(colour = "grey70"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source), position=position_dodge(width=0.8), size=2 ) +
  scale_colour_manual(name = "source", values = mySourceColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)


# Plot purpose

myPurposeColors        <- brewer.pal(length(levels(x$purpose)),"Set1")
names(myPurposeColors) <- rev(levels(x$purpose))

# plot by stock and purpose
x %>% 
  left_join(y, by="column") %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(colour = "grey70"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = purpose), position=position_dodge(width=0.8) ) +
  scale_colour_manual(name = "purpose", values = myPurposeColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)



# myPublishedColors        <- brewer.pal(length(levels(x$published)),"Set1")
# names(myPublishedColors) <- rev(levels(x$published))

# plot by stock and published
x %>% 
  left_join(y, by="column") %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold, group=published)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(colour = "grey70"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = published), position=position_dodge(width=0.8) ) +
  scale_colour_manual(name = "source", values = myPublishedColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)


# ---------------------------------------------------------------------------------------------
# plots from iAdvice by assessment year and other variables
# ---------------------------------------------------------------------------------------------

splitter <- 37

x <-
  iAdvice %>% 
  
  mutate_at(c("stockkeylabel","stockkeylabelold"), funs(tolower)) %>% 
  filter(speciesfaocode %in% c("mac")) %>% 
  filter(adviceonstock=="Y") %>% 
  
  distinct(stockkey, stockkeylabel, stockkeylabelold, speciesfaocode, assessmentyear, purpose, published) %>% 
  
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  arrange(stockkeylabelold) %>% 
  mutate(id = group_indices(., stockkeylabelold)) %>% 
  data.frame() %>% 
  mutate(stockkeylabelold = factor(stockkeylabelold),
         stockkeylabelold = factor(stockkeylabelold, levels = rev(levels(stockkeylabelold))),
         purpose          = factor(purpose),
         purpose          = factor(purpose, levels=rev(levels(purpose))),
         published          = factor(published),
         published          = factor(published, levels=rev(levels(published))),
         column              = ifelse(id <=  splitter            , 1, NA),
         column              = ifelse(id  >  splitter & id <= 2*splitter, 2, column),
         column              = ifelse(id  > 2*splitter            , 3, column)) %>% 
  left_join(iSpecies, by="speciesfaocode")

# define colour scales
myPurposeColors        <- brewer.pal(length(levels(x$purpose)),"Set1")
names(myPurposeColors) <- rev(levels(x$purpose))

myPublishedColors        <- brewer.pal(length(levels(x$published)),"Set1")
names(myPublishedColors) <- rev(levels(x$published))


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
  geom_point(aes(colour = purpose), position=position_dodge(width=0.8), size = 2 ) +
  scale_colour_manual(name = "purpose", values = myPurposeColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)

# plot by stock and published
x %>% 
  left_join(y, by="column") %>% 
  ggplot(aes(x=assessmentyear, y=stockkeylabelold, group=published)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(colour = "grey70"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = published), position=position_dodge(width=0.8) ) +
  scale_colour_manual(name = "source", values = myPublishedColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)

