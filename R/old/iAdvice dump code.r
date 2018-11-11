# ===========================================================================
# iAdvice plot.r

# 06/09/2016 adapted after WGWIDE
# 18/09/2016 prepared for ICES ASC talk with Inigo and David
# 23/09/2016 After adapting the horse mackerel data; new way of aggregateAt
# 05/01/2017 After revision of database structure and logic
# 27/06/2017 Read excel and save rdata file
# 01/08/2017 add all variables to the rdata file; moved to read_excel
# 11/08/2017 adapted for R 3.4.1 and tidyverse
# 04/10/2017 ported to github site
# ===========================================================================

# rm(list=ls())

library(readxl)
library(tidyverse)
library(stringr)
library(pander)

# Source the utils file
source("../mptools/R/my_utils.r")

# load gis data
load("../prf/rdata/world.europe.df.RData")
load("../prf/rdata/fao.df.RData")
load("../prf/rdata/fao27.df.RData")
load("../prf/rdata/eez.europe.df.RData")

# Set working directory
dropboxdir <- paste(gsub("\\","/",get_dropbox(), fixed=TRUE), "/ICES Advice database", sep="")



# ===========================================================================
# Read the data
# ===========================================================================

iAdvice <- 
  read_excel(paste0(dropboxdir, "/ICES Scientific Advice database.xlsx",sep=""), 
             sheet     = "DATA",
             col_names = TRUE,
             col_types = "text",
             skip      = 0)  %>% 
  lowcase() %>% 
  mutate_at( c("year", "assessyear", "ncpueseries","nsurveyseries"), 
             funs(as.integer)) %>% 
  mutate_at( c("advisedlandings", "advisedcatch","advisedlandingsmax","advisedcatchmax","tal","tac",
               "officiallandings","iceslandings","icesindustrialbycatch","icesdiscards","icescatch",
               "fsqymin1","ssbymin1","fadvmax","fmax","f01","fmed","f35spr","flim","fpa","fmsy",
               "blim","bpa","msybtrig"), 
            funs(as.numeric)) %>% 
  mutate_at( c("stockices","speciestype", "assessmodel"), 
             funs(tolower)) %>% 
  rename(fishstock = stockices, 
         assessmentyear = assessyear) %>% 
  mutate(faocode    = substr(fishstock,1,3),
         stockarea_ = strsplit(stockarea, "/"),
         tacarea_   = strsplit(tacarea,   "/"),
         intersect  = stockarea_,
         stockonly  = stockarea_,
         taconly    = stockarea_ ) %>% 
  arrange(fishstock, tacarea, year) 

save(iAdvice, file="rdata/iAdvice.RData")
# load(file="rdata/iAdvice.RData")

# expand combined stock areas (more to be done)
for (i in 1:length(iad$stock_area_)) {
  # only do the expansion of non zero listst
  if (length(iad$stock_area_[[i]]) > 0 ) {
    for (j in 1:length(iad$stock_area_[[i]])) {
      if        ( iad$stock_area_[[i]][j] == "7b-k") {
        iad$stock_area_[[i]][j]  <- 
          I(list(c("7b","7c","7d","7e","7f","7g","7h","7j","7k")))
      } else if (iad$stock_area_[[i]][j] == "8ab") {
        iad$stock_area_[[i]][j]  <- 
          I(list(c("8a","8b")))
      } else if (iad$stock_area_[[i]][j] == "8de") {
        iad$stock_area_[[i]][j]  <- 
          I(list(c("8d","8e")))
      } # end of if statement
    }  # end of j loop
    iad$stock_area_[i] <- list(unlist(iad$stock_area_[[i]]))
  } # end of first if statement 
} # end of i loop

# expand combined tac areas (more to be done)
for (i in 1:length(iad$tac_area_)) {
  # only do the expansion of non zero listst
  if (length(iad$tac_area_[[i]]) > 0 ) {
    for (j in 1:length(iad$tac_area_[[i]])) {
      if (iad$tac_area_[[i]][j] == "7b-k") {
        iad$tac_area_[[i]][j]  <-
          I(list(c("7b", "7c", "7d", "7e", "7f", "7g", "7h", "7j", "7k")))
      } else if (iad$tac_area_[[i]][j] == "8ab") {
        iad$tac_area_[[i]][j]  <-
          I(list(c("8a", "8b")))
      } else if (iad$tac_area_[[i]][j] == "8de") {
        iad$tac_area_[[i]][j]  <-
          I(list(c("8d", "8e")))
      } # end of if statement
    } # end of j loop    
    iad$tac_area_[i] <- list(unlist(iad$tac_area_[[i]]))
  } # end of first if statement 
} # end of i loop

# loop to calculate intersection and setdiff's  
for (i in 1:length(iad$stock_area_)){
  # print(paste(iad$stock_area_[[i]],iad$tac_area_[[i]],sep="#"))
  iad$intersect[[i]]  <- NA
  iad$intersect[[i]]  <- I(list(intersect(unlist(iad$stock_area_[[i]]),
                                           unlist(iad$tac_area_[[i]]))))
  
  iad$stock_only[[i]] <- NA
  iad$stock_only[[i]] <- I(list(  setdiff(unlist(iad$stock_area_[[i]]),
                                           unlist(iad$tac_area_[[i]]))))
  
  iad$tac_only[[i]]   <- NA
  iad$tac_only[[i]]   <- I(list(  setdiff(unlist(iad$tac_area_[[i]])  ,
                                           unlist(iad$stock_area_[[i]]))))
} # end of i loop

# ===========================================================================
# Dealing with some stock aggregation examples (anglerfish)
# ===========================================================================

ang <-
  iad %>% 
  filter(substr(stock_ices,1,3) %in% c("ang","anb","anp"))

# save(ang, file="Anglerfish.RData")

i <- 1
j <- 2

for (i in 1:length(ang$stock_area_)){
}  # end of i loop

for (i in 1:length(ang$tac_area_)){
}  # end of i loop


unique(ang$stock_area_)


# Western 
xmin  <- -17; 
xmax  <- 10; 
ymin  <- 38; 
ymax  <- 65

library(rgeos)
gUnion()
i <- 1

my.stock <-
  fao27.df %>%
  mutate(F_CODE = str_replace(F_CODE,"27.",""),
         F_CODE = gsub("[^[:alnum:] ]", "", F_CODE) ) %>% 
  filter(F_CODE %in% ang$stock_area_[[i]])

my.tac <-
  fao27.df %>%
  mutate(F_CODE = str_replace(F_CODE,"27.",""),
         F_CODE = gsub("[^[:alnum:] ]", "", F_CODE) ) %>% 
  filter(F_CODE %in% ang$tac_area_[[i]])

ggplot() + 
  theme_publication() +
  theme(#aspect.ratio     = plot.aspect ,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  coord_quickmap(xlim=c(xmin,xmax) , ylim=c(ymin,ymax)) +
  # geom_polygon(data=eez.europe.df,   aes(long, lat, group=group),
  #              fill = NA, size=0.1, colour="gray60", linetype=3) +
  geom_polygon(data=my.stock,aes(long, lat, group=group),
               fill="blue", size=0.2, colour="black", alpha=0.3) +
  # geom_polygon(data=my.tac,aes(long, lat, group=group),
  #              fill="red", size=0.2, colour="black", alpha=0.3) +
  geom_polygon(data=world.europe.df, aes(long, lat, group=group),
               fill = "gray80", size=0.1, colour="gray60") +
  geom_polygon(data=fao27.df, aes(long, lat, group=group),
               fill=NA, size=0.1, colour="gray60")
  
filter(ang, row()==1)
ang[i,]$tac_area_

xmin  <- -20; 
xmax  <- 10; 
ymin  <- 45; 
ymax  <- 65

cnames <- aggregate(cbind(long, lat) ~ id, data=subset(fao27.df, F_LEVEL="SUBDIVISION"), 
                    FUN=function(x)mean(range(x)))

cnames$F_DIVISION

cnames <-
  fao27.df %>% 
  filter(F_LEVEL == "DIVISION") %>% 
  group_by(id, F_DIVISION) %>% 
  summarize(long = mean(long, na.rm=TRUE),
            lat  = mean(lat, na.rm=TRUE)) %>% 
  mutate(F_DIVISION = str_replace(F_DIVISION,"27.",""))

fao27.df %>%
  filter(F_LEVEL == "DIVISION") %>% 
  ggplot() + 
  theme_publication() +
  theme(#aspect.ratio     = plot.aspect ,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  coord_quickmap(xlim=c(xmin,xmax) , ylim=c(ymin,ymax)) +
  geom_polygon(aes(long, lat, group=group, fill=factor(F_DIVISION)),
               size=0.1, colour="gray60") +
  geom_text(data=cnames, aes(long, lat, label = F_DIVISION), size=2)


fao27df.tac <-
  fao27.df %>%
  filter(F_LEVEL == "DIVISION") %>% 
  filter(F_DIVISION %in% c("27.6.a","27.7.c","27.7.b","27.5.b") ) 
  
fao27.df %>%
  filter(F_LEVEL == "DIVISION") %>% 
  filter(F_DIVISION %in% c("27.6.a","27.6.b","27.7.c","27.7.b") ) %>% 
  ggplot() + 
  theme_publication() +
  theme(#aspect.ratio     = plot.aspect ,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  coord_quickmap(xlim=c(xmin,xmax) , ylim=c(ymin,ymax)) +
  geom_polygon(aes(long, lat, group=group),
               fill="pink", size=0.1, colour="gray60", alpha=0.5) +
  geom_polygon(data=fao27df.tac, aes(long, lat, group=group),
               fill="blue", size=0.1, colour="gray60", alpha=0.5) +
  geom_polygon(data=fao27.df, aes(long, lat, group=group),
               fill=NA, size=0.1, colour="gray60")


# ===========================================================================
# Pseudocode for plotting multiple stock or tac areas
# ===========================================================================

# Simplify FAO df
my.fao <-
  fao27.df %>%
  mutate(F_CODE = str_replace(F_CODE,"27.",""),
         F_CODE = gsub("[^[:alnum:] ]", "", F_CODE) )
  
# Assign unique identifier to each row
t <- 
  iad %>% 
  mutate(my_id = row_number()) %>% 

# Filter the IAD for required stock or tac area
  filter(stock_ices == "ang-ivvi", stock_management == "Y") %>% 
  select(my_id, stock_ices, year, stock_area_, tac_area_)
  
# Split into two separate datasets
  
# Expand area or tac lists into additional rows in the data set
obs.stock <- sapply(t$stock_area_,length)
obs.tac   <- sapply(t$tac_area_, length)

exp.stock <- 
  data.frame(my_id=rep(t$my_id,obs.stock),area=unlist(t$stock_area_)) %>% 
  left_join(t, by="my_id") %>% 
  select(my_id, year, stock_ices, stock_area=area)

exp.tac <- 
  data.frame(my_id=rep(t$my_id,obs.tac),area=unlist(t$tac_area_)) %>% 
  left_join(t, by="my_id") %>% 
  select(my_id, year, stock_ices, F_CODE=area) %>% 
  left_join(my.fao, by=c("F_CODE")) %>% 
  

# apply gSimplify and gUnaryUnion to create simple overviews. 
# now do I need to go back to spatial objects first?
  
# plot
exp.tac %>%
  ggplot() + 
  theme_publication() +
  theme(#aspect.ratio     = plot.aspect ,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin     = unit(c(0,0,0,0), "lines"),
    axis.text.x      = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
    panel.border     = element_rect(colour="black" , size=0.2),
    legend.position  = "none") +
  
  labs(x = NULL, y = NULL) +    
  coord_quickmap(xlim=c(xmin,xmax) , ylim=c(ymin,ymax)) +
  geom_polygon(aes(long, lat, group=group),
               fill="pink", size=0.1, colour="gray60", alpha=0.5) +
  geom_polygon(data=fao27.df, aes(long, lat, group=group),
               fill=NA, size=0.1, colour="gray60") +
  facet_wrap(~ year)



#======== Description of IAD ========#

# ====  Counts  =====##
nSpecies        <- length(unique(iad$species))
nStocks         <- length(unique(iad$stock))
naggregateat    <- length(unique(iad$aggregateat))
nManagementarea <- length(unique(iad$managementarea))
nEcoregion      <- length(unique(iad$ecoregion))

# complete STFs
STF<-iad[complete.cases(iad[,c(30,31,32)]),]
dim(STF)
with(STF, length(unique(stock)))


# ======= AGGREGATES =========

###number of stocks per Aggregate
a <-aggregate(stock ~ aggregateat, data = iad,
              FUN = function(x)length(unique(x))) 
with (a, length(aggregateat[stock> 1])) # Number of Aggregates != stock
with (a, aggregateat[stock> 1])
with (a, aggregateat[stock==1])

# SELECTION OF AGGREGATES WHICH INCLUDE 2 OR MORE STOCKS 
b <- a%>% filter (stock !=1)  

with (b, aggregateat[stock == 2])
with (b, which.max(stock))

aggregates.data <- iad[iad$aggregateat %in% b$aggregateat, ]
# +50% of the rows in IAD belong to Aggregates!
aggregates.data %>%
  summarize (agg = length(unique(aggregateat)),
             Sp = length(unique(Species)),
             St=length(unique(stock)))


# difference in number of advice & TAC~#

no.advice <- c ("", "-", "Not assessed", "Not dealt with", "No advice", "No advice given", 
                "No advice given, apparently stable stock", "No basis for advice",
                "No specific advice","None", "No advice 3", "No advice for sprat",
                "No advice for sprat; Zero TAC for \"mixed clupeoid\" fishery",
                "No advice due to uncertain catches", "No advice because of misreporting",
                "No assessment")

aggregates <- aggregates.data %>% 
  select (stock, aggregateat, Year,AdviceBasis, Ecoregion,
          PredictedLandingsMax, AllowedLandings)%>%
  mutate(adv = ifelse (aggregates$AdviceBasis %in% no.advice,  NaN, 1) )


c <- aggregate (AllowedLandings ~ aggregateat + Year, aggregates, length)
d <- aggregate (adv ~ aggregateat + Year, aggregates, length)       
table <- merge (c, d, all = TRUE)

colnames(table)[3]<- "TAC"
colnames(table)[4]<- "Advice"
melt.table<- melt(table, id.vars = c( "Year","aggregateat")) 

ggplot(melt.table, aes(Year, value, fill = variable))+
  geom_bar(stat="identity", position = "dodge" )+
  scale_y_continuous(breaks= seq(0, 10, 2))+
  facet_wrap(~aggregateat)+ 
  theme_bw()+ ylab ("Number of observations")+
  theme(legend.title     = element_blank(),
        panel.grid.major = element_blank(),
        text             = element_text(size=11),
        strip.text       = element_text(size=8), 
        axis.title.x     = element_blank(),
        axis.text.x      = element_text(angle = 90, size = rel(0.7), vjust=0.3))

# spr 2232 is an aggregate by landings & STF, not by TAC/Adv
# whb nea TAC is set after merging 3 stocks (2adv)

# ===========================================================================
# plotting reference points
# ===========================================================================

data %>% 
  filter(stock %in% c("whb-comb", "her-47d3", "mac-nea"), 
         year >=2010) %>% 
  select(year, stock, flim, fpa, fmsy, blim, bpa, msybtrig) %>% 
  gather(key=refpoint, value=value, flim:msybtrig) %>% 
  filter(refpoint %in% c("flim","fpa","fmsy")) %>% 
  # filter(refpoint %in% c("blim","bpa","msybtrig")) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(panel.border     = element_rect(colour="gray" , size=0.2)) +
  geom_line(colour="blue") +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL) +
  facet_grid(stock~refpoint)

data %>% 
  filter(stock %in% c("whb-comb", "her-47d3", "her-noss", "mac-nea","ple-nsea", "cod-347d"), 
         year >=2010,
         stockmanagement == "Y") %>% 
  select(year, stock, flim, fpa, fmsy, blim, bpa, msybtrig) %>% 
  gather(key=refpoint, value=value, flim:msybtrig) %>% 
  filter(refpoint %in% c("fmsy")) %>% 
  # filter(refpoint %in% c("blim","bpa","msybtrig")) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(panel.border     = element_rect(colour="gray" , size=0.2)) +
  geom_line(colour="blue") +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~stock)

# ===========================================================================
# overview of advice this year compared to previous year
# ===========================================================================

data %>% 
  filter(stock ==   "boc-nea", 
         year  %in% c(2015,2016, 2017)) %>% 
  select(year, stock, predictedlandingsmax,  
         fsqymin1, ssbymin1, fadvmax) %>% 
  gather(key=variable, value=value, predictedlandingsmax:fadvmax) %>% 
  
  ggplot(aes(x=year, y=value)) +
  theme_publication() +
  theme(panel.border     = element_rect(colour="gray" , size=0.2)) +
  geom_bar(aes(fill=year), stat = "identity") +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL) +
  facet_wrap( ~ variable, scales="free_y")



