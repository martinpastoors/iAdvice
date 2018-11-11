# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph plotting
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
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
source("D:/XXX/PRF/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))


# ---------------------------------------------------------------------------------------------
# Create subset of series (stocks) with at least 8 years of data and SSB in tonnes, F in year-1
# ---------------------------------------------------------------------------------------------
selected <-
  iAssess %>% 
  
  group_by(stockkey, stockkeylabelold, assessmentyear) %>%
  filter(row_number() == 1) %>%
  filter(assessmentyear >= 1990, assessmentyear <= 2017) %>% 
  
  group_by(stockkey, stockkeylabelold) %>%
  summarise(nyears = n()) %>% 
  filter(nyears >= 15) %>% 
  select(stockkey, stockkeylabelold)


# Check certain assessments
# filter(sagdb, stockkeylabelold == "cod-2532", assessmentyear==2017, stocksizeunits=="biomass") %>% View()

# ---------------------------------------------------------------------------------------------
# Calculate Mohn's rho; first in code then in function
# ---------------------------------------------------------------------------------------------
d <-
  selected %>% 
  left_join(iAssess, by=c("stockkey","stockkeylabelold")) %>% 
  filter(year             <= assessmentyear) %>% 
  select(assessmentyear, year, stockkey, stockkeylabelold, stockkeylabelnew, recruitment:lowrecruitment, f:lowf, ssb:lowssb,
         assessmenttype2) %>% 
  data.frame()

# dataset with last year
d.last <- 
  d %>% 
  group_by(stockkeylabelold) %>% 
  filter(assessmentyear == max(assessmentyear)) %>% 
  data.frame() %>% 
  # select(year, ssb) %>% 
  select(stockkeylabelold, year, ssb) %>% 
  arrange(stockkeylabelold, year)

# dataset from other years
d.oth <- 
  d %>% 
  filter(assessmentyear != max(assessmentyear)) %>% 
  as.data.frame() %>% 
  select(stockkeylabelold, stockkeylabelnew, assessmentyear, year, ssb) %>% 
  arrange(stockkeylabelold, assessmentyear, year)

# merge datasets to calculate relative differences (a la Mohn's rho)
m <-
  d.oth %>% 
  group_by(stockkeylabelold, stockkeylabelnew, assessmentyear) %>% 
  
  # merge the last year to each assessmentyear
  dplyr::full_join(d.last, by=c("stockkeylabelold","year")) %>% 
  
  # calculate yeardifference and filter for the number of required years. 
  mutate  (yeardiff = assessmentyear - year,
           rho  = (ssb.x - ssb.y) / ssb.y) 

# set number of years to include in calculation
n <- 3

# calculate average Mohn's rho over all selected assessment years
m %>% 
  filter(yeardiff <= (n - 1)) %>% 
  
  # calculate mohn's rho
  group_by(stockkeylabelold) %>% 
  summarise(mrho = sum(rho, na.rm=TRUE),
            n    = n()) %>% 
  mutate(mrho = mrho / n)


myspecies <- "cod"

# Plot absolute time trends compared to last data year
d.oth %>% 
  filter(grepl(myspecies, stockkeylabelold)) %>% 
  
  ggplot(aes(x=year,y=ssb, group=assessmentyear)) +
  theme_publication() +
  theme(legend.position = "none",
        axis.text       = element_text(size=9),
        strip.text      = element_text(face="plain")) +
  geom_line(aes(colour=factor(assessmentyear))) + 
  geom_line(data=filter(d.last, grepl(myspecies, stockkeylabelold)), 
            aes(x=year,y=ssb), size=1, colour="black", inherit.aes = FALSE) +
  # scale_y_continuous(labels=function(n){format(n, scientific = TRUE)}) +
  scale_y_continuous(labels=scientific_format(digits=2)) +
  expand_limits(y=0) +
  facet_wrap(~stockkeylabelold, scales="free_y", ncol=5)

# Plot relative time trends compared to last data year
m %>% 
  filter(grepl(myspecies, stockkeylabelold)) %>% 
  ggplot(aes(x=year,y=rho, group=assessmentyear)) +
  theme_publication() +
  theme(legend.position = "none",
        axis.text       = element_text(size=9),
        strip.text      = element_text(face="plain", size=8, lineheight=0.2) ) +
  geom_hline(aes(yintercept = 0), colour="black", size=1) +
  geom_line(aes(colour=factor(assessmentyear))) +
  facet_wrap(~stockkeylabelold, ncol=5, scale="free_y")



# Look for retrospective measures that capture variation and scale differences
# Jonsson & Hjorleifsson 2000
# Hurtado 2015
# Leseur 2004


# ---------------------------------------------------------------------------------------------
# Calculate scale difference
#
# Scale differences: calculate average value for all the years in common. Calculate the number of jumps, 
# defined as a relative difference of more than xx%
# ---------------------------------------------------------------------------------------------
e <-
  d %>% 
  filter(year             >  1990, 
         assessmentyear   >= 2007,
         year             <= assessmentyear-2,
         year             >= assessmentyear-12) %>% 
  select(assessmentyear, year, fishstocknew, fishstockold, ssb, assessmentmodel, assessmenttype) %>% 
  group_by(fishstockold, year) %>%
  arrange(fishstockold, year, assessmentyear) %>% 
  mutate(lyear = lead(assessmentyear),
         lssb  = lead(ssb)) %>% 
  arrange(fishstockold, assessmentyear, year)

# data series with first estimation
e.1a <-
  e %>% 
  filter(assessmentyear < max(assessmentyear, na.rm=TRUE)) %>% 
  select(assessmentyear, year, fishstocknew, fishstockold, assessmentmodel, assessmenttype, ssb) %>% 
  mutate(series=assessmentyear,
         header=paste(assessmentyear,assessmentyear+1,sep="-") )

# data series with second estimation
e.1b <-
  e %>% 
  ungroup() %>% 
  filter(!is.na(lyear)) %>% 
  filter(lyear == assessmentyear + 1) %>% 
  select(-ssb) %>% 
  mutate(series=lyear,
         header=paste(assessmentyear,assessmentyear+1,sep="-")) %>% 
  select(assessmentyear, year, fishstocknew, fishstockold, ssb=lssb, series, assessmentmodel, 
         assessmenttype,header) %>% 
  bind_rows(e.1a)

# create dataset of means
e.means <-
  e.1b %>% 
  group_by(fishstockold, assessmentyear, series, header, assessmentmodel, assessmenttype) %>% 
  summarise(ssb = mean(ssb, na.rm=TRUE),
            miny=min(year),
            maxy=max(year)) %>% 
  gather(key=tmp, value=year, miny:maxy) 
  
# make list of stocks with required number of assessment years
e.stocks <-
  e.means %>% 
  filter(tmp == "miny") %>% 
  filter(assessmentyear == series) %>% 
  group_by(fishstockold) %>% 
  summarise(n=n()) %>% 
  filter(n >= 8)

# dataset with benchmarks
benchmarks <-
  e %>% 
  group_by(fishstockold, assessmentyear, assessmenttype) %>% 
  filter(fishstockold %in% e.stocks$fishstockold) %>% 
  filter(row_number()==1) %>% 
  filter(grepl("bench", assessmenttype)) %>% 
  ungroup() %>% 
  mutate(assessmentyear = assessmentyear -1)

# calculate scaling parameters per stock
e.scale <- 
  e.means %>%
  filter(tmp=="miny") %>% 
  ungroup() %>% 
  arrange(fishstockold, year, assessmentyear, assessmentmodel, assessmenttype) %>% 
  mutate(ssb2    = lead(ssb),
         rho     = (ssb2-ssb)/ssb,
         rho_abs = abs(rho)) %>% 
  group_by(fishstockold) %>% 
  summarise(rho     = mean(rho, na.rm=TRUE),
            rho_abs = mean(rho_abs, na.rm=TRUE)) %>% 
  arrange(-rho_abs)

# create dataset of relative scaling difference by year
e.year <-
  e %>% 
  group_by(fishstockold, assessmentyear, year) %>% 
  mutate(test = (lssb-ssb)/ssb,
         test2= abs(test)) %>% 
  filter(!is.na(test)) %>% 
  group_by(fishstockold, assessmentyear) %>% 
  summarise(test = mean(test, na.rm=TRUE),
            test2= mean(test2, na.rm=TRUE)) 


# set species to plot
# myspecies <- c("ane","cod","had","her")
myspecies <- c("hke","hom","mac","ple","sai","sar","sol","spr","whb","whg")

# plot differences between two subsequent assessments
e.1b %>% 
  filter(fishstockold %in% e.stocks$fishstockold) %>% 
  filter(substr(fishstockold,1,3) %in% myspecies) %>% 
  # filter(fishstockold == "ple-eche") %>% 
  
  ggplot(aes(x=year, y=ssb)) +
  theme_publication() +
  theme(legend.position = "none",
        axis.text       = element_text(size=9),
        strip.text      = element_text(face="plain"),
        panel.spacing   = unit(0.1, "lines"),
        strip.text.y    = element_text(angle = 0)) +
  geom_line(aes(colour=factor(series))) +
  geom_dl(aes(label  = series, colour=factor(series)),
          method = list(dl.combine("last.points"), cex = 0.5)) +
  geom_line(data=filter(e.means, 
                        fishstockold %in% e.stocks$fishstockold,
                        substr(fishstockold,1,3) %in% myspecies),
            aes(x=year, y=ssb, colour=factor(series)), inherit.aes=FALSE, linetype="dashed") +
  
  scale_y_continuous(labels=scientific_format(digits=2)) +
  expand_limits(y=0) +
  facet_grid(fishstockold ~ header, scales="free_y")
  # facet_wrap( ~ header)


# set the treshold for scale difference (10%)
t <- 0.1

# plot scale differences between assessments
e.year %>% 
  filter(fishstockold %in% e.stocks$fishstockold) %>% 

  ggplot(aes(x=assessmentyear, y=test)) +
  theme_publication() +
  theme(legend.position="none") +
  theme(panel.spacing = unit(0.1, "lines")) +
  theme(strip.text.y = element_text(angle = 0)) +
  
  geom_line (aes(colour=factor(fishstockold))) +
  geom_point(aes(colour=factor(fishstockold), size = ifelse(abs(test) > t,0.3,0.0001))) +
  
  geom_hline(aes(yintercept = t), colour="gray80", linetype="dashed", size=0.5) +
  geom_hline(aes(yintercept = 0), colour="black", size=0.5) +
  geom_hline(aes(yintercept = -t), colour="gray80", linetype="dashed", size=0.5) +
  
  # add benchmark information
  geom_vline(data=benchmarks, aes(xintercept=assessmentyear), colour="black", linetype="dashed") +
  
  coord_cartesian(ylim=c(-0.5, 0.5)) +  # set the limits in cartesian space prevents outlier lines to be excluded
  scale_y_continuous(labels=percent_format()) +
  facet_wrap(~fishstockold)


# ---------------------------------------------------------------------------------------------
# Developments in stock assessment methods
# ---------------------------------------------------------------------------------------------

f<-
  sagdb %>% 
  group_by(fishstockold, assessmentyear, assessmentmodel, assessmenttype) %>% 
  filter(row_number()==1) %>% 
  select(fishstockold, assessmentyear, assessmentmodel, assessmenttype) %>% 
  ungroup() %>% 
  mutate(
    assessmentmodel = ifelse(grepl("aspic", assessmentmodel), "aspic", assessmentmodel),
    assessmentmodel = ifelse(grepl("stochastic state", assessmentmodel), "sam", assessmentmodel),
    assessmentmodel = ifelse(grepl("trends-based assessment", assessmentmodel), "trends only", assessmentmodel),
    assessmentmodel = ifelse(grepl("trends only", assessmentmodel), "trends only", assessmentmodel),
    
    assessmentmodel = ifelse(grepl("^xsam$", assessmentmodel), "ysam", assessmentmodel), 
    assessmentmodel = ifelse(grepl("xsa", assessmentmodel), "xsa", assessmentmodel), 
    assessmentmodel = ifelse(grepl("ysam", assessmentmodel), "xsam", assessmentmodel),
    
    assessmentmodel = ifelse(grepl("flica", assessmentmodel), "ica", assessmentmodel), 
    
    assessmentmodel = ifelse(grepl("xsam", assessmentmodel), "xsap", assessmentmodel),
    assessmentmodel = ifelse(grepl("sam", assessmentmodel), "sam", assessmentmodel),
    assessmentmodel = ifelse(grepl("xsap", assessmentmodel), "xsam", assessmentmodel)
    
  ) %>% 
  
  filter(assessmentmodel %in% c("xsa","ica","amci","seastar","sam","xsam", "vpa", "ls")) %>% 
  group_by(assessmentmodel, assessmentyear) %>% 
  summarise(n = n()) %>% 
  
  # calculate relative proportion
  group_by(assessmentyear) %>% 
  mutate(nrel = n/sum(n))


f %>% 
  ggplot(aes(x=assessmentyear, y=nrel)) + 
  theme_publication() +
  geom_bar(aes(fill=assessmentmodel), stat="identity")
