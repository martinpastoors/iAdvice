# -----------------------------------------------------------------------------------------------
# plots for pelagic forum
#
# 03/11/2017 first coding
# 15/10/2018 updated for 2018 version of the talk
# 28/09/2019 updated for 2019 version of the talk
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
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# load the data
iSpecies <-
  get(load(file=paste(dropboxdir,"/rdata/iSpecies.RData", sep=""))) %>% 
  ungroup() %>% 
  dplyr::select(speciesfaocode, trophicguild, fisheriesguild, sizeguild) 

iAssess <-
  get(load(file=paste(dropboxdir,"/rdata/iAssess.RData", sep=""))) %>% 
  left_join(iSpecies, by="speciesfaocode")

load(file=paste(dropboxdir,"/rdata/iAdvice.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iForecast.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iRename.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iStockkey.RData", sep=""))

lay               <- 2019
ly                <- 2019
my.firstyear      <- 1990

# show catch in selected year
iAssess %>% 
  group_by(year, stockkeylabelold) %>% 
  filter(assessmentyear == lay & year == my.lastyear) %>% 
  filter(fisheriesguild %in% c("pelagic")) %>% 
  # filter(speciesfaocode%in% c("her","whb","mac","hom")) %>% 
  filter(purpose == "advice") %>% 
  # filter(catcheslandingsunits == "tonnes") %>% 
  dplyr::select(year, stockkeylabelold, speciesfaocode, speciesname, catches, landings) %>%
  mutate(catch = max(catches, landings, na.rm=TRUE)) %>% 
  arrange(-catch)


# add intermediate and forecast data
# forecasts <-
#   read_excel(path=paste(dropboxdir,"/data/Pelagic forecasts.xlsx", sep=""), col_names = TRUE) %>% 
#   left_join(iStockkey, by="stockkeylabelold") %>% 
#   mutate(stockkeylabel = stockkeylabelnew) %>% 
#   left_join(iRename,   by=c("stockkey", "stockkeylabel")) %>% 
#   left_join(iSpecies,  by="speciesfaocode") %>% 
#   mutate(catchesladingsunits = "tonnes")
  

# t <-
#   icesSAGfull %>%
#   filter(year < assessmentyear) %>% 
#   bind_rows(forecasts) 
# 
# filter(icesSAGfull, stockkeylabelold == "whb-comb", assessmentyear == 2018, year >= 2016) %>% View()  
# filter(forecasts, stockkeylabelold == "whb-comb", assessmentyear == 2018, year >= 2016) %>% View()  
# filter(t, stockkeylabelold == "whb-comb", assessmentyear == 2018, year >= 2016) %>% View()  
# filter(t, stockkeylabel == "her.27.3031", year >= 2016) %>% View()  
# glimpse(forecasts)

# ---------------------------------------------------------------------------------------------
# plot of catch per fisheries guild
# ---------------------------------------------------------------------------------------------

d <-
  iAssess %>% 
  ungroup() %>% 
  # filter(assessmentyear      == lay | (assessmentyear == 2017 & stockkeylabelold == "cap-icel") ) %>% 
  filter(assessmentyear      == lay  ) %>% 
  # filter(catcheslandingsunits == "tonnes") %>% 
  filter(year >= my.firstyear, year < my.lastyear) %>% 
  filter(purpose %in% c("advice")) %>% 

  mutate(catches = ifelse(is.na(catches) | catches == 0, landings, catches))  %>% 
  mutate(fisheriesguild = ifelse(fisheriesguild %in% c("pelagic","demersal"), fisheriesguild, "other"))  %>% 
  mutate(fisheriesguild = factor(fisheriesguild, levels=c("pelagic","demersal","other"))) %>% 
  
  group_by(fisheriesguild, speciesfaocode, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=2)) 


# plot stacked catches: pelagic, demersal and other
p1 <-
  d %>% 
  group_by(fisheriesguild, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=2)) %>% 
  
  ggplot(aes(year,catches)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_bar(aes(fill=(fisheriesguild)), stat="identity", position="stack", colour="black") +
  geom_dl(aes(label= fisheriesguild, colour = fisheriesguild),
          position = position_stack(vjust = 0.5),
          method   = list(dl.trans(x=x+0.9), dl.combine("last.points"), 
                          fontface = 'bold',cex=.9, hjust = 0)) +
  labs(x="",y="tonnes",title="catches") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits=c(my.firstyear,lay+2)) 


# plot of filled relative catches (how to add the direct labels?)
d %>% 
  group_by(year, fisheriesguild) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=2)) %>% 
  
  ggplot(aes(year,perc)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_bar(aes(fill=fisheriesguild), stat="identity", position="stack", colour="black") +
  geom_dl(aes(label= fisheriesguild, colour = fisheriesguild),
          position = position_stack(vjust = 0.5),
          method   = list(dl.trans(x=x+0.9), dl.combine("last.points"),
                          fontface = 'bold',cex=.9, hjust = 0)) +
  labs(x="",y="percent",title="relative catches") +
  # scale_y_continuous(labels = comma) +
  scale_x_continuous(limits=c(my.firstyear,lay+2)) 


# check demersal catches
icesSAGfull %>% 
  filter(assessmentyear      == 2018) %>% 
  filter(catchesladingsunits == "tonnes") %>% 
  filter(year >= my.firstyear, year < 2018) %>% 
  filter(purpose %in% c("Advice")) %>% 
  
  mutate(catches = ifelse(is.na(catches) | catches == 0, landings, catches))  %>% 
  group_by(fisheriesguild, speciesfaocode, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=0)) %>% 
  
  filter(fisheriesguild == "demersal") %>% 
  
  ggplot(aes(year,catches)) +
  theme_publication() +
  # theme(legend.position="none") +
  geom_bar(aes(fill=fisheriesguild), stat="identity", position="stack") +
  scale_y_continuous(labels = comma) +
  labs(x="",y="tonnes",title="catches") +
  geom_dl(aes(label= fisheriesguild, colour = fisheriesguild),
          position = position_stack(vjust = 0.5),
          method   = list(dl.trans(x=x+0.9), dl.combine("last.points"), fontface = 'bold',cex=.9, hjust = 0)) +
  scale_x_continuous(limits=c(my.firstyear,lay+2)) +
  facet_wrap(~speciesfaocode)



# now check the composition of the pelagic catches
d %>% 
  filter(fisheriesguild == "pelagic") %>% 
  mutate(speciesfaocode = ifelse(speciesfaocode %in% c("mac","whb","her", "hom", "pil","spr", "cap"), 
                                 speciesfaocode, "zzz")) %>% 
  group_by(fisheriesguild, speciesfaocode, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=0)) %>% 
  
  ggplot(aes(year,catches)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_bar(aes(fill=speciesfaocode), stat="identity", position="stack", colour="black") +
  scale_y_continuous(labels = comma) +
  labs(x="",y="tonnes",title="catches") +
  geom_dl(aes(label= speciesfaocode, colour = speciesfaocode), 
          position = position_stack(vjust = 0.5),
          method   = list(dl.trans(x=x+0.9), dl.combine("last.points"), fontface = 'bold',cex=.9, hjust = 0)) +
  scale_x_continuous(limits=c(my.firstyear,lay+2)) 


# plot filled catches
d %>% 
  filter(fisheriesguild == "pelagic") %>% 
  mutate(speciesfaocode = ifelse(speciesfaocode %in% c("mac","whb","her", "hom", "pil","spr", "cap"), 
                                 speciesfaocode, "zzz")) %>% 
  group_by(fisheriesguild, speciesfaocode, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=2)) %>% 
  
  ggplot(aes(year,perc)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_bar(aes(fill=speciesfaocode), stat="identity", position="stack", colour="black") +
  scale_y_continuous(labels = comma) +
  labs(x="",y="tonnes",title="catches") +
  geom_dl(aes(label= speciesfaocode, colour = speciesfaocode), 
          position = position_stack(vjust = 0.5),
          method   = list(dl.trans(x=x+0.9), dl.combine("last.points"), fontface = 'bold',cex=.9, hjust = 0)) +
  scale_x_continuous(limits=c(my.firstyear,lay+2)) 

d %>% 
  filter(fisheriesguild == "pelagic") %>% 
  mutate(speciesfaocode = ifelse(speciesfaocode %in% c("mac","whb","her", "hom", "pil","spr", "cap"), 
                                 speciesfaocode, "zzz")) %>% 
  group_by(year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE))  %>% 
  View()


# ---------------------------------------------------------------------------------------------
# plot of catch and forecast for key pelagic species 
# ---------------------------------------------------------------------------------------------

d <-
  t %>% 
  filter(stockkeylabelold %in% c("whb-comb","mac-nea","her-47d3")) %>% 
  filter(assessmentyear == 2018) %>% 
  filter(year >= 2000, year <= 2019) %>%
  filter(!is.na(catches)) %>% 
  filter(purpose %in% c("Advice")) %>% 
  group_by(stockkeylabelold, year) %>% 
  filter(row_number()==1) %>% 
  group_by(year) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=0)) %>% 
  arrange(stockkeylabelold, year)

# filter(d, stockkeylabelold=="whb-comb", year >= 2015) %>% View()
# filter(d, year == 2018) %>% View()
# d %>% ungroup() %>% filter(year == 2018) %>% summarize(catches=sum(catches, na.rm=TRUE))

# plot stacked catches
d %>% 
  ggplot(aes(year,catches)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "null") +
  geom_bar(aes(fill=stockkeylabelold), stat="identity", position="stack") +
  geom_dl(aes(label= stockkeylabelold, colour = stockkeylabelold), 
          position = position_stack(vjust = 0.5),
          method   = list(dl.trans(x=x+0.9), dl.combine("last.points"), fontface = 'bold',cex=.9, hjust = 0)) +
  labs(x="",y="tonnes",title="catches and advice for 2019") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits=c(1999,2022))

filter(d, year==2018) %>% summarize(catches=sum(catches, na.rm=TRUE))

# ---------------------------------------------------------------------------------------------
# FROM HERE MOVE OVER TO PLOT_SAG.R
# ---------------------------------------------------------------------------------------------





# ---------------------------------------------------------------------------------------------
# plots of herring
# ---------------------------------------------------------------------------------------------

d <-
  t %>% 
  filter(speciesfaocode %in% c("her")) %>% 
  ungroup() %>% 
  filter(year             >=  my.firstyear, 
         assessmentyear   >=  1980,
         year             <= assessmentyear) %>% 
  mutate(purpose = ifelse(assessmentyear == max(assessmentyear) &
                                  purpose %in% c("Advice"), 
                                  "last",purpose)) %>% 
  
  filter(purpose %in% c("Advice","last","bench", "withdrawn")) %>% 
  
  mutate(tyear     = ifelse(purpose == "Advice", as.character(assessmentyear), NA),
         tyear     = ifelse(purpose == "update", as.character(assessmentyear), tyear),
         tyear     = ifelse(purpose == "last"  , paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(purpose == "withdrawn"   , paste(assessmentyear,"-W",sep="") ,tyear),
         tyear     = ifelse(purpose == "bench" , paste(assessmentyear,"-B",sep="") ,tyear),
         tyear     = ifelse(purpose == "alt"   , paste(assessmentyear,"-A",sep="") ,tyear)) %>% 
  
  data.frame()

# set lastyear
lastyear  <- 
  unique(unlist(select(filter(d, purpose=="last"), assessmentyear)))


# plot stacked ssb
d %>% 
  filter(purpose %in% c("Advice", "last")) %>% 
  filter(assessmentyear == 2018) %>% 
  
  ggplot(aes(year,stocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  # theme(legend.position="none") +
  theme(legend.title = element_blank()) +
  geom_bar(aes(fill=stockkeylabelold), stat="identity", position="stack") +
  scale_y_continuous(labels = comma) +
  labs(x="",y="tonnes",title="ssb") +
  # geom_dl(aes(label= stockkeylabelold, colour = stockkeylabelold), 
  #         position = position_stack(vjust = 0.5),
  #         method   = list(dl.trans(x=x+0.9), dl.combine("last.points"), fontface = 'bold',cex=.9, hjust = 0)) +
  scale_x_continuous(limits=c(my.firstyear,2017))

ribbon <-
  d %>% 
  filter(stockkeylabelold %in% c("her-noss","her-47d3")) %>% 
  filter(purpose == "last") 

# filter(d, stockkeylabelold=="her-noss") %>% View()

lastpoints <-
  d %>% 
  group_by(assessmentyear, purpose) %>% 
  filter(year == max(year))

# plot ssb
d %>% 
  filter(!is.na(stocksize)) %>%  
  filter(stockkeylabelold %in% c("her-noss","her-47d3")) %>% 
  filter(purpose != "bench") %>% 
  filter(assessmentyear >= 2015) %>% 
  mutate(assessmenttype3 = paste(stockkeylabelold, purpose, assessmentyear, sep="")) %>% 

  ggplot(aes(year,stocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "null") +
  
  scale_colour_manual  (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",Advice = "solid",bench ="solid",withdrawn = "solid",   alt="solid")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,Advice = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  geom_ribbon(data=ribbon,
              aes(x=year, ymin=lowstocksize, ymax=highstocksize, group=purpose, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  geom_line  (aes(group=assessmenttype3, colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = "tonnes" , title = NULL)  +
  # facet_grid(assessmentyear ~ stockkeylabelold) +
  facet_wrap(~ stockkeylabelold, ncol=2, scales="free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits=c(1999,lay+2))

# plot for her-noss
d %>% 
  filter(!is.na(stocksize)) %>%  
  filter(stockkeylabelold %in% c("her-noss")) %>% 
  filter(purpose != "bench") %>% 
  filter(assessmentyear >= 2015) %>% 
  mutate(assessmenttype3 = paste(stockkeylabelold, purpose, assessmentyear, sep="")) %>% 
  
  ggplot(aes(year,stocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "null") +
  
  scale_colour_manual  (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",Advice = "solid",bench ="solid",withdrawn = "solid",   alt="solid")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,Advice = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  geom_line  (aes(group=assessmenttype3, colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_point (data= filter(lastpoints, stockkeylabelold=="her-noss", 
                                       assessmentyear >= 2015,
                                       purpose != "bench"),
              aes(colour=purpose), size=2) +
  
  geom_hline (aes(yintercept=5000000), size=1.2, linetype="dotted", colour="red") +
  geom_hline (aes(yintercept=2500000), size=1.2, linetype="dashed", colour="red") +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.trans(x=x+0.2), dl.combine("last.points"), cex = 1.0)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = "tonnes" , title = NULL)  +
  # facet_grid(assessmentyear ~ stockkeylabelold) +
  facet_wrap(~ stockkeylabelold, ncol=2, scales="free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits=c(1999,2018))


# ---------------------------------------------------------------------------------------------
# plots of mackerel
# ---------------------------------------------------------------------------------------------

d <-
  t %>% 
  filter(speciesfaocode %in% c("mac")) %>% 
  ungroup() %>% 
  filter(year             >=  2000, 
         assessmentyear   >=  1980,
         year             <= assessmentyear) %>% 
  mutate(purpose = ifelse(assessmentyear == max(assessmentyear) &
                                    purpose %in% c("Advice"), 
                                  "last",purpose)) %>% 
  
  filter(purpose %in% c("Advice","last","bench", "withdrawn")) %>% 
  
  mutate(tyear     = ifelse(purpose == "Advice", as.character(assessmentyear), NA),
         tyear     = ifelse(purpose == "update", as.character(assessmentyear), tyear),
         tyear     = ifelse(purpose == "last"  , paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(purpose == "withdrawn"   , paste(assessmentyear,"-W",sep="") ,tyear),
         tyear     = ifelse(purpose == "bench" , paste(assessmentyear,"-B",sep="") ,tyear),
         tyear     = ifelse(purpose == "alt"   , paste(assessmentyear,"-A",sep="") ,tyear)) %>% 
  
  data.frame()

# set lastyear
lastyear  <- 
  unique(unlist(select(filter(d, purpose=="last"), assessmentyear)))

ribbon <-
  d %>% 
  filter(stockkeylabelold %in% c("mac-nea")) %>% 
  filter(purpose == "last") 

lastpoints <-
  d %>% 
  group_by(assessmentyear, purpose) %>% 
  filter(year == max(year))

# plot for mac-nea (most recent assessment only)
d %>% 
  filter(!is.na(stocksize)) %>%  
  filter(purpose != "bench") %>% 
  filter(assessmentyear >= 2015) %>% 
  filter(assessmentyear == 2017) %>% 
  mutate(assessmenttype3 = paste(stockkeylabelold, purpose, assessmentyear, sep="")) %>% 
  
  ggplot(aes(year,stocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "null") +
  
  scale_colour_manual  (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",Advice = "solid",bench ="solid",withdrawn = "solid",   alt="solid")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,Advice = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  geom_ribbon(data=ribbon,
              aes(x=year, ymin=lowstocksize, ymax=highstocksize, group=purpose, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  geom_line  (aes(group=assessmenttype3, colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_point (data= filter(lastpoints, stockkeylabelold=="mac-nea", 
                           assessmentyear == 2017,
                           purpose != "bench"),
              aes(colour=purpose), size=2) +
  
  geom_hline (aes(yintercept=2570000), size=1.2, linetype="dotted", colour="red") +
  geom_hline (aes(yintercept=1940000), size=1.2, linetype="dashed", colour="red") +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.trans(x=x+0.2), dl.combine("last.points"), cex = 1.0)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = "tonnes" , title = NULL)  +
  # facet_grid(assessmentyear ~ stockkeylabelold) +
  facet_wrap(~ stockkeylabelold, ncol=2, scales="free_y") +
  scale_y_continuous(limits=c(0,6000000), labels = comma) +
  scale_x_continuous(limits=c(1999,2018))


# step 2: with a few years of assessments

d %>% 
  filter(!is.na(stocksize)) %>%  
  filter(assessmentyear >= 2015) %>% 
  mutate(assessmenttype3 = paste(stockkeylabelold, purpose, assessmentyear, sep="")) %>% 
  
  ggplot(aes(year,stocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "null") +
  
  scale_colour_manual  (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",Advice = "solid",bench ="solid",withdrawn = "solid",   alt="solid")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,Advice = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  geom_ribbon(data=ribbon,
              aes(x=year, ymin=lowstocksize, ymax=highstocksize, group=purpose, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  geom_line  (aes(group=assessmenttype3, colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_point (data= filter(lastpoints, stockkeylabelold=="mac-nea", 
                           assessmentyear == 2017,
                           purpose != "bench"),
              aes(colour=purpose), size=2) +
  
  geom_hline (aes(yintercept=2570000), size=1.2, linetype="dotted", colour="red") +
  geom_hline (aes(yintercept=1940000), size=1.2, linetype="dashed", colour="red") +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.trans(x=x+0.2), dl.combine("last.points"), cex = 1.0)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = "tonnes" , title = NULL)  +
  # facet_grid(assessmentyear ~ stockkeylabelold) +
  facet_wrap(~ stockkeylabelold, ncol=2, scales="free_y") +
  scale_y_continuous(limits=c(0,6000000), labels = comma) +
  scale_x_continuous(limits=c(1999,2018))

# Step 3: with many years

d %>% 
  filter(!is.na(stocksize)) %>%  
  filter(assessmentyear >= 2002) %>% 
  mutate(assessmenttype3 = paste(stockkeylabelold, purpose, assessmentyear, sep="")) %>% 
  
  ggplot(aes(year,stocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "null") +
  
  scale_colour_manual  (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",Advice = "solid",bench ="solid",withdrawn = "solid",   alt="solid")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,Advice = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  geom_ribbon(data=ribbon,
              aes(x=year, ymin=lowstocksize, ymax=highstocksize, group=purpose, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  geom_line  (aes(group=assessmenttype3, colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_point (data= filter(lastpoints, stockkeylabelold=="mac-nea", 
                           assessmentyear == 2017,
                           purpose != "bench"),
              aes(colour=purpose), size=2) +
  
  geom_hline (aes(yintercept=2570000), size=1.2, linetype="dotted", colour="red") +
  geom_hline (aes(yintercept=1940000), size=1.2, linetype="dashed", colour="red") +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.trans(x=x+0.2), dl.combine("last.points"), cex = 1.0)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = "tonnes" , title = NULL)  +
  # facet_grid(assessmentyear ~ stockkeylabelold) +
  facet_wrap(~ stockkeylabelold, ncol=2, scales="free_y") +
  scale_y_continuous(limits=c(0,6000000), labels = comma) +
  scale_x_continuous(limits=c(1999,2018))

# ---------------------------------------------------------------------------------------------
# plots of blue whiting
# ---------------------------------------------------------------------------------------------

d <-
  t %>% 
  filter(speciesfaocode %in% c("whb")) %>% 
  ungroup() %>% 
  filter(year             >=  2000, 
         assessmentyear   >=  1980,
         year             <= assessmentyear) %>% 
  mutate(purpose = ifelse(assessmentyear == max(assessmentyear) &
                                    purpose %in% c("Advice"), 
                                  "last",purpose)) %>% 
  
  filter(purpose %in% c("Advice","last","bench", "withdrawn")) %>% 
  
  mutate(tyear     = ifelse(purpose == "Advice", as.character(assessmentyear), NA),
         tyear     = ifelse(purpose == "update", as.character(assessmentyear), tyear),
         tyear     = ifelse(purpose == "last"  , paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(purpose == "withdrawn"   , paste(assessmentyear,"-W",sep="") ,tyear),
         tyear     = ifelse(purpose == "bench" , paste(assessmentyear,"-B",sep="") ,tyear),
         tyear     = ifelse(purpose == "alt"   , paste(assessmentyear,"-A",sep="") ,tyear)) %>% 
  
  data.frame()

# set lastyear
lastyear  <- 
  unique(unlist(select(filter(d, purpose=="last"), assessmentyear)))

ribbon <-
  d %>% 
  filter(stockkeylabelold %in% c("whb-comb")) %>% 
  filter(purpose == "last") 

lastpoints <-
  d %>% 
  group_by(assessmentyear, purpose) %>% 
  filter(year == max(year))

# blue whiting with a few years of assessments

d %>% 
  filter(!is.na(stocksize)) %>%  
  filter(assessmentyear >= 2014) %>% 
  mutate(assessmenttype3 = paste(stockkeylabelold, purpose, assessmentyear, sep="")) %>% 
  
  ggplot(aes(year,stocksize)) +
  theme_publication() +
  theme(text = element_text(size=18)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "null") +
  
  scale_colour_manual  (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_fill_manual    (values=c(last = "red",update="black",Advice = "black",bench = "blue", withdrawn = "darkgreen", alt="gray")) +
  scale_linetype_manual(values=c(last="solid",update="solid",Advice = "solid",bench ="solid",withdrawn = "solid",   alt="solid")) +
  scale_size_manual    (values=c(last= 1.5,   update=0.8,Advice = 0.8,    bench = 1.2,    withdrawn = 0.8,         alt=0.8)) +
  
  geom_ribbon(data=ribbon,
              aes(x=year, ymin=lowstocksize, ymax=highstocksize, group=purpose, fill = purpose), alpha=0.3, inherit.aes = FALSE) +
  geom_line  (aes(group=assessmenttype3, colour = purpose, size=purpose, linetype=purpose) ) +
  
  geom_point (data= filter(lastpoints, stockkeylabelold=="whb-comb", 
                           assessmentyear == 2017),
              aes(colour=purpose), size=2) +
  
  geom_hline (aes(yintercept=2570000), size=1.2, linetype="dotted", colour="red") +
  geom_hline (aes(yintercept=1940000), size=1.2, linetype="dashed", colour="red") +
  
  geom_dl(aes(label  = tyear, colour = purpose), 
          method = list(dl.trans(x=x+0.2), dl.combine("last.points"), cex = 1.0)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = "tonnes" , title = NULL)  +
  # facet_grid(assessmentyear ~ stockkeylabelold) +
  facet_wrap(~ stockkeylabelold, ncol=2, scales="free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits=c(1999,2018))

