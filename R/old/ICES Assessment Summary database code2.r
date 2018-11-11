# R-script for ICES fishdata base 2013
# Martin Pastoors
# 07/05/2014

# Reset lists
rm(list=ls())

# Add graphics library
library(graphics)
library(lattice)

# Set working directory
# setwd("C://Users//Martin Pastoors//Documents//ZZZ//ICES fishdata")
setwd("C://data//data//ICES")

fishdata <- read.csv("ICES quality database 20150429.csv", stringsAsFactors=F)
str(fishdata)
# summary(fishdata)

# subset data to north sea
fishdataNS2013 <- fishdata[
                      fishdata$AssYear==2013 & 
                      fishdata$Ecoregion== "north sea" & 
                      fishdata$Species %in% c("her","ple","cod","sol", "had", "tur", "whg"),]
# add the decades
decade <- floor(fishdataNS2013$Year / 10) * 10
fishdataNS2013 <- cbind(fishdataNS2013,decade)

# str(fishdataNS2013)
# summary(fishdataNS2013)

# Plot the trends in F (with smoother)
xyplot( F~Year|FishStock ,data=fishdataNS2013, xlim=c(1950,2020), ylim=c(0,1.2), scales=list(0.3, 0.6, 0.9), pch="+")

# Calculate averages
mean.fishdata <- aggregate(fishdataNS2013$MeanF, list(fishdataNS2013$decade,fishdataNS2013$FishStock), mean, na.rm=TRUE)
# mean.fishdata.tapply <- tapply(fishdataNS2013$MeanF, list(fishdataNS2013$decade,fishdataNS2013$FishStock), mean, na.rm=TRUE)
# head(mean.fishdata)

# Plot decade averages
xyplot( x~Group.1|Group.2 ,data=mean.fishdata, xlim=c(1950,2020), ylim=c(0,1.2), scales=list(0.3, 0.6, 0.9))



