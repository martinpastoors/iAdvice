# =================================================================
# R-script for analyzing and plotting the ICES quality database and
# the ICES advice database
#
# Martin Pastoors
#
# Original: 07/05/2014
# Updated : 01/06/2015
# =================================================================

# Reset lists
rm(list=ls())

# Add relevant (graphic) libraries
library(graphics)
library(lattice)
library(reshape2)
library(dplyr)

source ("multiplot.r")

# Set working directory
setwd("C://data//data//ICES")

# Read ICES quality database in csv format ------------------------------------
so             <- read.csv("ICES quality database 20150429.csv", stringsAsFactors=F)

so$F           <- as.numeric(so$F)
so$Recruitment <- as.numeric(so$Recruitment)
so$SSB         <- as.numeric(so$SSB)
so$Landings    <- as.numeric(so$Landings)
names(so)

# Read ICES advice database in csv format
to             <- read.csv("20150428 History of scientific advice and management.csv", stringsAsFactors=F)
ta             <- select(to, Year, Stock, Stocktype, Area, Management.authority, 
                         Assess.year)
names(ta)      <- c("y","FishStock","stkt", "area", "ma", "AssYear")

# merge quality and advice datasets
ua <- merge (sa,ta,by=c("AssYear", "FishStock"),all.x= TRUE)
# names(ua)

# select only relevant columns
# using select and names(); to be done

# =========================================================================

# Calculate Mohn's rho on the dataset

# =========================================================================

# Plot for a certain species
stk   <- "her-47d3"
minay <- 2000
maxay <- 2015
miny  <- 1990
maxy  <- 2015
g   <- ua[ua$FishStock == stk & 
          ua$Year    >= miny  & ua$Year <= maxy & 
          ua$AssYear >= minay & ua$AssYear <= maxay ,]
lng <- length(unique(g$AssYear))

# generate plot of retro's
p1 <- ggplot(g ,aes(x=Year,y=SSB,group=AssYear, colour=factor(AssYear), size=factor(AssYear))) +
  geom_line()+
  scale_colour_manual(values=c(rep("black",lng-1),"red")) +
  scale_size_manual(values=c(rep(0.5,lng-1),1.1)) +
  theme(legend.position="none") +
  ggtitle(paste(stk,"\nSSB")) +
  theme(plot.title = element_text(lineheight=.8, size=10))
  
p2 <- ggplot(g ,aes(x=Year,y=F,group=AssYear, colour=factor(AssYear), size=factor(AssYear))) +
  geom_line()+
  scale_colour_manual(values=c(rep("black",lng-1),"red")) +
  scale_size_manual(values=c(rep(0.5,lng-1),1.1)) +
  theme(legend.position="none") +
  ggtitle(paste(stk,"\nF")) +
  theme(plot.title = element_text(lineheight=.8, size=10))

p3 <- ggplot(g ,aes(x=Year,y=Recruitment,group=AssYear, colour=factor(AssYear), size=factor(AssYear))) +
  geom_line()+
  scale_colour_manual(values=c(rep("black",lng-1),"red")) +
  scale_size_manual(values=c(rep(0.5,lng-1),1.1)) +
  theme(legend.position="none") +
  ggtitle(paste(stk, "\nRecruit")) +
  theme(plot.title = element_text(lineheight=.8, size=10))

multiplot(p1,p2,p3,cols=3)

# =========================================================================

# Subset for certain species ----------------------------------------------------------

bya <- so[
                so$FishStock %in% c("her-47d3") &
                so$AssYear==2015 ,]

# Plot the trends in F (with smoother)
xyplot(F ~Year|FishStock ,data=bya, xlim=c(1990,2020), ylim=c(0,0.8), scales=list(0.3, 0.6, 0.9), pch="+")


# Subset data to certain region ---------------------------------------------------

byns <- so[so$AssYear==2013 & 
                   so$Ecoregion== "north sea" & 
                   so$Species %in% c("her","ple","cod","sol", "had", "whg"),]



# add decades and calculate averages
decade <- floor(byns$Year / 10) * 10
byns <- cbind(byns,decade)

# str(byns)
# summary(byns)
# head(byns)

# Calculate averages
bynsm <- aggregate(byns$F, 
                           list(byns$decade,
                                byns$FishStock), 
                           mean, na.rm=TRUE)
# mean.so.tapply <- tapply(byns$F, list(byns$decade,byns$FishStock), mean, na.rm=TRUE)
# head(mean.so)

# Plot the trends in F (with smoother)
xyplot( F~Year|FishStock ,data=byns, xlim=c(1960,2020), ylim=c(0,1.2), scales=list(0.3, 0.6, 0.9), pch="+")

# Plot decade averages
xyplot( x~Group.1|Group.2 ,data=mean.so, xlim=c(1950,2020), ylim=c(0,1.2), scales=list(0.3, 0.6, 0.9))

# Plot North Sea plaice data over different years
Col <- c("red", "green", "blue", "purple", "black", "yellow") 
xyplot( F~Year|AssYear ,
        data=so[so$FishStock=="ple-nsea" & so$AssYear > 2000,], 
        panel = function(x, y, col, ...) {
          Col <- Col[panel.number()]
          panel.xyplot(x, y, col = "blue", ...)
          panel.loess(x, y, col = Col)
        },
        xlim=c(1950,2020), 
        ylim=c(0,1.2), 
        type = c('p','smooth'),
        scales=list(0.3, 0.6, 0.9), 
        pch=".",
        as.table=TRUE,
        strip = strip.custom(strip.names = FALSE, strip.levels=TRUE)
      )

# Plot all years in one plot (not finished yet; needs to add colours)
xyplot( F~Year,
        data=so[so$FishStock=="ple-nsea" & so$AssYear > 2000,], 
        panel = function(x, y, col, ...) {
          Col <- Col[panel.number()]
          panel.xyplot(x, y, col = Col, ...)
          panel.loess(x, y, col = Col)
        },
        group=so$AssYear,
        xlim=c(1950,2020), 
        ylim=c(0,1.2), 
        type = c('p','smooth'),
        scales=list(0.3, 0.6, 0.9), 
        pch="."
)


# --------------------------------------
# mypanel annotates the points
myPanel <- function(x, y, subscripts, ...) {
  panel.xyplot(x, y, ...)
  ltext(x - 1, y + 1, letters[subscripts], cex=0.5)
}

# mystrip alternates the strip texts
myStrip <- function(which.panel, ...) {
  font <- rep(1, 2)
  font[which.panel] <- 2
  col=rep("grey", 2)
  col[which.panel] <- "black"
  llines(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
  ltext(c(0.33, 0.66), rep(0.5, 2), 1:2, font=font, col=col)
}

#orepanel deals with the axis
myPrePanel <- function(x, y, ...) {
  list(xlim=c(min(x) - 1, max(x) + 1),
       ylim=c(min(y) - 1, max(y) + 1))
}

X <- 1:20
Y <- 1:20
G <- factor(rep(1:2, 10))
xyplot(X ~ Y | G, 
       aspect=1, 
       layout=c(1, 2),
       panel=myPanel,
       strip=myStrip,
       prepanel=myPrePanel
       )



