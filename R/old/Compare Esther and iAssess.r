# -----------------------------------------------------------------------------------------------
# compare Esther and iAssess
#
# 26/11/2018 comparing Esther and iAssess
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(directlabels)  # for printing labels at end of geom lines
library(scales)    # scales and formatting
library(dataCompareR) # compare r datasets

# library(comparer)  # compare r datasets
# remove.packages("comparer")

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

# load(file=paste(advicedir, "/rdata/iStock.RData",sep=""))
# load(file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
# load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))

# -----------------------------------------------------------------------------------------
# Compare number of assessments 
# -----------------------------------------------------------------------------------------


t1 <- 
  iAssess %>% 
  filter(!grepl("benchmark", purpose)) %>% 
  dplyr::select(stockkeylabelold, assessmentyear ) %>% 
  distinct()

t2 <- 
  esther %>% 
  dplyr::select(stockkeylabelold, assessmentyear ) %>% 
  distinct()

incommon <-
  inner_join(t1, t2)

only_in_iassess <-
  anti_join(t1, t2)

only_in_esther <-
  anti_join(t2, t1)

# plot the meta data on assessments
incommon %>% 
  mutate(source = "in common") %>% 
  bind_rows(mutate(only_in_iassess, source= "only in iAssess")) %>% 
  bind_rows(mutate(only_in_esther, source= "only in Esther")) %>% 
  mutate(id                  = group_indices(., stockkeylabelold)) %>% 
  mutate(column              = ifelse(id <=  83            , 1, NA),
         column              = ifelse(id  >  83 & id <= 166, 2, column),
         column              = ifelse(id  > 166            , 3, column)) %>% 
  
  ggplot(aes(x=assessmentyear, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(colour = "grey70"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source), position=position_dodge(width=0.8) ) +
  scale_y_discrete(position="right") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~column, scales="free_y", shrink=TRUE, ncol=3)




# -----------------------------------------------------------------------------------------
# Check the values of stocks in common 
# -----------------------------------------------------------------------------------------

x1 <- 
  incommon %>% 
  left_join(iAssess, by=c("stockkeylabelold","assessmentyear")) %>% 
  select(one_of(names(esther))) 
  
x2 <- 
  incommon %>% 
  left_join(esther, by=c("stockkeylabelold","assessmentyear")) 

comp <- dataCompareR::rCompare(x1, x2, keys=c("stockkeylabelold", "assessmentyear","year"))
summary(comp)
saveReport(comp, reportName = 'compEsther-iAssess')

x <- 
  incommon %>% 
  left_join(iAssess, by=c("stockkeylabelold","assessmentyear")) %>% 
  left_join(select(esther,
                   stockkeylabelold, assessmentyear, year, stocksize,recruitment, fishingpressure, fpa), 
            by = c("stockkeylabelold","assessmentyear","year")) %>% 
  gather(key=variable, value=value, c(stocksize.x, recruitment.x, fishingpressure.x, fpa.x, 
                                      stocksize.y, recruitment.y, fishingpressure.y, fpa.y)) %>% 
  group_by(stockkeylabelold, assessmentyear, variable) %>% 
  summarize(value = mean(value, na.rm=TRUE)) %>% 
  mutate(
    source   = ifelse(grepl("\\.x",variable), "iassess", "esther"),
    variable = gsub("\\.x|\\.y","", variable) 
  ) %>% 
  spread(key=source, value=value) %>% 
  mutate(diff = (iassess-esther)/iassess)

# compare assessment years with difference in average stock size larger than 5%
x %>% 
  filter(abs(diff) > 0.05) %>% 
  distinct(stockkeylabelold, assessmentyear, variable) %>% 
  left_join(x, by=c("stockkeylabelold", "assessmentyear", "variable")) %>% 
  filter(variable == "stocksize") %>% 
  gather(key=source, value=value, esther:iassess) %>% 
  
  ggplot(aes(x=assessmentyear,y=value, group=source)) +
  theme_publication() +
  geom_point(aes(colour=source), size=2) +
  expand_limits(y=0) +
  facet_wrap(~stockkeylabelold, scales="free_y")


# plot specific cases
iAssess %>% 
  # filter(stockkeylabelold == "anp-78ab", assessmentyear == 2006) %>% 
  # filter(stockkeylabelold == "cod-scow", assessmentyear == 2006) %>% 
  filter(stockkeylabelold == "sol-kask", assessmentyear == 2015) %>% 
  left_join(select(esther,
                   stockkeylabelold, assessmentyear, year, stocksize,recruitment, fishingpressure, fpa), 
            by = c("stockkeylabelold","assessmentyear","year")) %>% 
  gather(key=variable, value=value, c(stocksize.x, recruitment.x, fishingpressure.x, fpa.x,
                                      stocksize.y, recruitment.y, fishingpressure.y, fpa.y)) %>% 
  mutate(
    source   = ifelse(grepl("\\.x",variable), "iassess", "esther"),
    variable = gsub("\\.x|\\.y","", variable) 
  ) %>% 
  select(one_of(c(names(esther), "source","variable", "value"))) %>% 

  View()

  
  ggplot(aes(x=year,y=value, group=source)) +
  theme_publication() +
  geom_point(aes(colour=source), alpha=0.5) +
  geom_line(aes(colour=source), alpha=0.5) +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y")

# plot average trend in overfishing and overfished  
esther %>% 
  filter(assessmentyear >= 2001) %>% 
  group_by(assessmentyear) %>% 
  summarize(overfishing = mean(overfishing)) %>% 
  ggplot(aes(assessmentyear, overfishing)) +
  theme_publication() +
  geom_line() +
  expand_limits(y=0)

esther %>% 
  filter(assessmentyear >= 2001) %>% 
  group_by(assessmentyear) %>% 
  summarize(overfished = mean(overfished, na.rm=TRUE)) %>% 
  ggplot(aes(assessmentyear, overfished)) +
  theme_publication() +
  geom_line() +
  expand_limits(y=0)

esther %>% 
  filter(assessmentyear >= 2001) %>% 
  group_by(assessmentyear) %>% 
  summarize(stocksize = mean(stocksize, na.rm=TRUE)) %>% 
  ggplot(aes(assessmentyear, stocksize)) +
  theme_publication() +
  geom_line() +
  expand_limits(y=0)

iAdvice %>% 
  filter(assessmentyear >= 2001) %>% 
  group_by(assessmentyear) %>% 
  summarize(fmsy = mean(fmsy, na.rm=TRUE)) %>% 
  ggplot(aes(assessmentyear, fmsy)) +
  theme_publication() +
  geom_line() +
  expand_limits(y=0)


x <-
  iAssess %>% 
  filter(assessmentyear >= 2018) %>%
  filter(year > 1980) %>% 
  filter(year <= assessmentyear) %>% 
  filter(stockkeylabelold %in% c("ple-nsea", "cod-347d", "her-47d3", "sol-nsea", "sai-3a46", "had-346a")) %>% 
  # filter(grepl("27.4",stockarea)) %>%
  filter(grepl("advice", purpose)) %>% 
  distinct(stockkeylabel, assessmentyear, year, fishingpressure, stocksize) 

x %>% 
  group_by(year) %>% 
  summarize(
    fishingpressure = mean(fishingpressure, na.rm=TRUE),
    stocksize       = sum(stocksize, na.rm=TRUE)
  ) %>% 
  gather(key=variable, value=value, fishingpressure:stocksize)  %>% 
  
  ggplot(aes(year, value)) +
  theme_publication() +
  geom_line(aes(colour=variable), size=2) +
  geom_line(data = gather(x, key=variable, value=value, fishingpressure:stocksize), 
            aes(x=year, y=value, group=stockkeylabel), colour="gray", inherit.aes=FALSE) +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y")

