# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph plotting
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- get_dropbox()
setwd(paste(dropboxdir, "/ICES Assessment database", sep=""))

# load the data
load(file="rdata/sagdb.RData")

# ---------------------------------------------------------------------------------------------
# plots for single year over multiple species
# ---------------------------------------------------------------------------------------------

# ssb over MSY Btrigger
p1 <-
  sagdb %>% 
  filter(fishstocknew %in% c("her.27.20-24", 
                             "her.27.3a47d", 
                             "her.27.6a7bc",
                             "her.27.1-24a514a",
                             "mac.27.nea", 
                             "whb.27.1-91214", 
                             "hom.27.2a4a5b6a7a-ce-k8", 
                             "spr.27.4") ) %>% 
  filter(assessmentyear == 2016) %>% 
  ggplot(aes(year,ssb, group=assessmentyear)) +
  theme_publication() +
  geom_line(aes(col = ifelse(ssb > msybtrig,'red','green')), size=1.5) +
  # geom_line(col = "black") +
  geom_hline(aes(yintercept=msybtrig), colour="gray80", linetype="dashed", size=0.5) +
  expand_limits(y = 0) +
  theme(legend.position  = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_text(colour="black", size=8, hjust=0),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "ssb" ) ) +
  facet_wrap(  ~ fishstocknew, scales = "free_y", ncol=3)

p1

# F over Fmsy
p2 <-
  sagdb %>% 
  filter(fishstock %in% c("her-3a22", "her-47d3", "her-67bc", 
                          "mac-nea", "whb-comb", "hom-west", 
                          "spr-nsea") ) %>% 
  
  ggplot(aes(year,f, group=assessmentyear)) +
  theme_bw() +
  geom_line(aes(col = ifelse(f < fmsy,'red','green')), size=1.5) +
  # geom_line(col = "black") +
  geom_hline(aes(yintercept=fmsy), colour="gray80", linetype="dashed", size=0.5) +
  expand_limits(y = 0) +
  theme(legend.position  = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank(),
        axis.text.x      = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y      = element_blank(),
        strip.text.x     = element_blank(),
        strip.background = element_blank(),
        panel.border     = element_blank(),
        plot.margin      = unit( c(0,0,0,0.0) , units = "lines" )) +
  labs(x = NULL, y = NULL, title=paste( "f over fmsy" ) ) +
  facet_wrap(  ~ fishstock, scales = "free_y", ncol=1)

# multiplot(p1, p2, p3, cols=3) ; have to update this to cowplot ....


# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

d <-
  sagdb %>% 
  # filter(fishstocknew %in% c("her-noss", "whb-comb","mac-nea","hom-west"),
  # filter(fishstockold %in% c("mac-nea","mac-nea-bench","mac-nea-old")) %>% 
  # filter(fishstockold %in% c("hom-west","hom-west-bench")) %>% 
  
  # filter(grepl("hom-west", fishstockold) ) %>% 
  filter(grepl("mac-nea", fishstockold) ) %>% 
  # filter(grepl("whb", fishstockold) ) %>% 
  # filter(grepl("noss", fishstockold) ) %>% 
  
  # filter(fishstocknew %in% c("her-47d3"),
  # filter(grepl("hom.27.2a", fishstocknew)) %>% 
  # filter(grepl("her.27.3a", fishstocknew)) %>% 
  ungroup() %>% 
  filter(year             >  1980, 
         assessmentyear   >  2003,
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, fishstock, fishstockold, fishstocknew, 
         recruitment:lowrecruitment, f:lowf, ssb:lowssb,
         assessmenttype2, assessmentmodel) %>% 
  mutate(assessmenttype2 = ifelse(assessmentyear == max(assessmentyear),"last",assessmenttype2)) %>% 
  mutate(fishstock = gsub("-bench","",fishstock, fixed=TRUE),
         fishstock = gsub("-old","",fishstock, fixed=TRUE)) %>% 
  mutate(tyear     = ifelse(assessmenttype2 == "assess", as.character(assessmentyear), NA),
         tyear     = ifelse(assessmenttype2 == "last", paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(assessmenttype2 == "old", paste(assessmentyear,"-O",sep="") ,tyear),
         tyear     = ifelse(assessmenttype2 == "bench", paste(assessmentyear,"-B",sep="") ,tyear)) %>% 
  data.frame()

# get the last assessment year and stock name
lastyear        <- unique(unlist(select(filter(d, assessmenttype2=="last"), assessmentyear)))
last <-
  d %>% 
  filter(assessmenttype2 == "last") %>% 
  select(fishstock, year, lastssb = ssb, lastf=f, lastr = recruitment)

# scale to last year ?
d <-
  d %>% 
  left_join(last, by=c("fishstock","year")) %>% 
  mutate(recruitment = recruitment/lastr,
         ssb         = ssb/lastssb,
         f           = f / lastf)


# plot ssb
p1 <-
  d %>% 
  filter(!is.na(ssb)) %>%  
  
  ggplot(aes(year,ssb, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype2, size=assessmenttype2, linetype=assessmenttype2) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype2), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +

  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  


# plot f
p2 <-
  d %>% 
  filter(!is.na(f)) %>%  
  
  ggplot(aes(year,f, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype2, size=assessmenttype2, linetype=assessmenttype2) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype2), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "F")   +
  facet_grid(fishstock ~ .)

plot_grid(p1 + theme(legend.position = "none", axis.title      = element_blank()), 
          p2 + theme(axis.title      = element_blank()),
          ncol=2, align = 'h', rel_widths = c(3,3))


# ---------------------------------------------------------------------------------------------
# Create subset of series (stocks) with at least 8 years of data and SSB in tonnes, F in year-1
# ---------------------------------------------------------------------------------------------
selected <-
  sagdb %>% 
  
  # filter(fishstockold == "cod-2532") %>% 
  
  # only use ssb in tonnes and f in year-1
  filter(stocksizeunits=="tonnes",fishingpressureunits=="year-1", year >= 1990) %>% 
  filter(!is.na(ssb)) %>% 
  
  group_by(fishstockold) %>% 
  filter(n_distinct(stocksizeunits) == 1) %>% 
  
  group_by(fishstockold, assessmentyear) %>%
  filter(row_number() == 1) %>%
  filter(assessmentyear >= 2005, assessmentyear <= 2016) %>% 
  
  group_by(fishstockold) %>% 
  filter(max(assessmentyear) == 2016) %>% 
  
  summarise(nyears = n()) %>% 
  filter(nyears >= 8) %>% 
  select(fishstockold)


# Check certain assessments
filter(sagdb, fishstockold == "cod-2532", assessmentyear==2017, stocksizeunits=="biomass") %>% View()

# ---------------------------------------------------------------------------------------------
# Calculate Mohn's rho; first in code then in function
# ---------------------------------------------------------------------------------------------
d <-
  selected %>% 
  left_join(sagdb, by=c("fishstockold")) %>% 
  filter(year             >  1990, 
         assessmentyear   >= 2005,
         assessmentyear   <= 2016,
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, fishstockold, fishstocknew, recruitment:lowrecruitment, f:lowf, ssb:lowssb,
         assessmentmodel, assessmenttype) %>% 
  data.frame()

# dataset with last year
d.last <- 
  d %>% 
  group_by(fishstockold) %>% 
  filter(assessmentyear == max(assessmentyear)) %>% 
  data.frame() %>% 
  # select(year, ssb) %>% 
  select(fishstockold, year, ssb) %>% 
  arrange(fishstockold, year)

# dataset from other years
d.oth <- 
  d %>% 
  filter(assessmentyear != max(assessmentyear)) %>% 
  as.data.frame() %>% 
  select(fishstockold, fishstocknew, assessmentyear, year, ssb) %>% 
  arrange(fishstockold, assessmentyear, year)

# merge datasets to calculate relative differences (a la Mohn's rho)
m <-
  d.oth %>% 
  group_by(fishstockold, fishstocknew, assessmentyear) %>% 
  
  # merge the last year to each assessmentyear
  dplyr::full_join(d.last, by=c("fishstockold","year")) %>% 
  
  # calculate yeardifference and filter for the number of required years. 
  mutate  (yeardiff = assessmentyear - year,
           rho  = (ssb.x - ssb.y) / ssb.y) 

# set number of years to include in calculation
n <- 3

# calculate average Mohn's rho over all selected assessment years
m %>% 
  filter(yeardiff <= (n - 1)) %>% 
  
  # calculate mohn's rho
  group_by(fishstockold) %>% 
  summarise(mrho = sum(rho, na.rm=TRUE),
            n    = n()) %>% 
  mutate(mrho = mrho / n)


# Plot absolute time trends compared to last data year
ggplot(data=d.oth, aes(x=year,y=ssb, group=assessmentyear)) +
  theme_publication() +
  theme(legend.position = "none",
        axis.text       = element_text(size=9),
        strip.text      = element_text(face="plain")) +
  geom_line(aes(colour=factor(assessmentyear))) + 
  geom_line(data=d.last, aes(x=year,y=ssb), size=1, colour="black", inherit.aes = FALSE) +
  # scale_y_continuous(labels=function(n){format(n, scientific = TRUE)}) +
  scale_y_continuous(labels=scientific_format(digits=2)) +
  expand_limits(y=0) +
  facet_wrap(~fishstockold, scales="free_y", ncol=5)

# Plot relative time trends compared to last data year
m %>% 
  ggplot(aes(x=year,y=rho, group=assessmentyear)) +
  theme_publication() +
  theme(legend.position = "none",
        axis.text       = element_text(size=9),
        strip.text      = element_text(face="plain", size=8, lineheight=0.2) ) +
  geom_hline(aes(yintercept = 0), colour="black", size=1) +
  geom_line(aes(colour=factor(assessmentyear))) +
  facet_wrap(~fishstockold, ncol=5, scale="free_y")



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
