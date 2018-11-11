# -----------------------------------------------------------------------------------------------
# plots for pelagic forum
#
# 03/11/2017 first coding
# 15/10/2018 updated for 2018 version of the talk
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)
library(stringr)
library(readxl)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# load the data

#load(file=paste(dropboxdir,"/rdata/iAssess.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iAdvice.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iForecast.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iRename.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iStockkey.RData", sep=""))
# write.csv(iStockkey, file="iStockkey,csv", row.names = FALSE)

iSpecies <-
  get(load(file=paste(dropboxdir,"/rdata/iSpecies.RData", sep=""))) %>% 
  ungroup() %>% 
  dplyr::select(speciesfaocode, trophicguild, fisheriesguild, sizeguild) 

icesSAGfull <-
  get(load(file=file.path(dropboxdir, "rdata/icesSAGfull 20181016.RData"))) %>% 
  mutate(speciesfaocode = substr(stockkeylabel, 1, 3)) %>% 
  left_join(iSpecies, by="speciesfaocode") %>% 
  
  # corrections
  mutate(
    catches        = ifelse(stockkeylabelold == "ghl-grn" & assessmentyear == 2018,catches/1000,catches ),
    fisheriesguild = ifelse(stockkeylabelold == "whb-comb", "pelagic", fisheriesguild),
    fisheriesguild = ifelse(speciesfaocode == "san", "demersal", fisheriesguild)
  ) %>% 
  
  # remove duplicated rows
  group_by(stockkey, assessmentyear, year, catches, landings, stocksize, fishingpressure, recruitment) %>% 
  filter(row_number() == 1)

  # %>%  dplyr::select(-c(grepl("custom", colnames(.))))


# show catch in 2017
icesSAGfull %>% 
  filter(assessmentyear == 2018 & year == 2017) %>% 
  filter(fisheriesguild == "pelagic") %>% 
  filter(catchesladingsunits == "tonnes") %>% 
  dplyr::select(year, stockkeylabelold, catches, landings) %>% 
  arrange(-catches)

icesSAGfull %>% 
  filter(assessmentyear == 2018) %>% 
  filter(fisheriesguild == "pelagic") %>% 
  filter(catchesladingsunits == "tonnes") %>% 
  filter(speciesfaocode == "spr") %>% 
  View()



# add intermediate and forecast data
forecasts <-
  read_excel(path=paste(dropboxdir,"/data/Pelagic forecasts.xlsx", sep=""), col_names = TRUE) %>% 
  left_join(iStockkey, by="stockkeylabelold") %>% 
  mutate(stockkeylabel = stockkeylabelnew) %>% 
  left_join(iRename,   by=c("stockkey", "stockkeylabel")) %>% 
  left_join(iSpecies,  by="speciesfaocode") %>% 
  mutate(catchesladingsunits = "tonnes")
  

t <-
  icesSAGfull %>%
  filter(year < assessmentyear) %>% 
  bind_rows(forecasts) 

# filter(icesSAGfull, stockkeylabelold == "whb-comb", assessmentyear == 2018, year >= 2016) %>% View()  
# filter(forecasts, stockkeylabelold == "whb-comb", assessmentyear == 2018, year >= 2016) %>% View()  
# filter(t, stockkeylabelold == "whb-comb", assessmentyear == 2018, year >= 2016) %>% View()  
# filter(t, stockkeylabel == "her.27.3031", year >= 2016) %>% View()  
# glimpse(forecasts)


# ---------------------------------------------------------------------------------------------
# plots of relative stock trends
# ---------------------------------------------------------------------------------------------

d <-
  t %>% 
  filter(stockkeylabelold %in% c("her-47d3", "cod-347d", "sai-3a46", "had-346a", "hke-nrtn",
                                 "spr-nsea", "mac-nea", "whb-comb")) %>% 
  ungroup() %>% 
  filter(year             >=  1990, 
         assessmentyear   ==  2018,
         year             <= assessmentyear) %>% 
  filter(purpose %in% c("Advice"))

# icesSAGfull %>% filter(speciesfaocode == "had") %>% group_by(stockkeylabelold) %>% 
#   filter(row_number() ==1 ) %>% distinct(stockkeylabelold)

# plot standardized recruitment
d %>% 
  group_by(stockkeylabelold) %>% 
  filter(!is.na(recruitment)) %>% 
  mutate(relrecruit = log(recruitment) / mean(log(recruitment))-1) %>% 
  
  ggplot(aes(year,relrecruit)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.position="none") +
  theme(legend.title = element_blank()) +
  geom_bar(aes(fill=stockkeylabelold), stat="identity", position="stack") +
  scale_y_continuous(labels = comma) +
  labs(x="",y="",title="Relative log recruitment") +
  scale_x_continuous(limits=c(1990,2017)) +
  facet_wrap(~stockkeylabelold)

# plot standardized ssb
d %>% 
  filter(!is.na(stocksize)) %>% 
  group_by(stockkeylabelold) %>% 
  mutate(relstocksize = stocksize / mean(stocksize)-1) %>% 

  ggplot(aes(year,relstocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.position="none") +
  theme(legend.title = element_blank()) +
  geom_bar(aes(fill=stockkeylabelold), stat="identity", position="stack") +
  scale_y_continuous(labels = comma) +
  labs(x="",y="",title="Relative stock size") +
  scale_x_continuous(limits=c(1990,2017)) +
  facet_wrap(~stockkeylabelold)


# plot standardized fishing pressure
d %>% 
  filter(!is.na(fishingpressure)) %>% 
  group_by(stockkeylabelold) %>% 
  mutate(relfishingpressure = fishingpressure / mean(fishingpressure)-1) %>% 
  
  ggplot(aes(year,relfishingpressure)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.position="none") +
  theme(legend.title = element_blank()) +
  geom_bar(aes(fill=stockkeylabelold), stat="identity", position="stack") +
  scale_y_continuous(labels = comma) +
  labs(x="",y="",title="Relative fishing pressure") +
  scale_x_continuous(limits=c(1990,2017)) +
  facet_wrap(~stockkeylabelold)

