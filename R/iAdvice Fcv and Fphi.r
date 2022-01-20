# ===========================================================================
# iAdvice Fcv and Fphi.r
#
# 09/06/2020 initial coding
# ===========================================================================

# rm(list=ls())

library(readxl)
library(tidyverse)
library(stringr)
library(pander)
library(scales)

# Source the utils file
source("../mptools/R/my utils.r")

# set dropboxdirs
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# load forecast data
fc <-
  get(load(file=paste(dropboxdir, "/rdata/iAdvice.RData", sep=""))) %>% 
  filter(purpose=="advice") %>% 
  filter(!is.na(fset)) %>% 
  filter(stockkeylabelnew %in% c("whb.27.1-91214","hom.27.2a4a5b6a7a-ce-k8","mac.27.nea")) %>% 
  rename(year = tacyear) %>% 
  dplyr::select(stockkey, stockkeylabelnew, stockkeylabelold, year, 
                fsqimy, ssbimy, fadvmax, catchrealized, catch1fcy, catch2fcy, f1fcy, f2fcy, ssb1fcy, ssb2fcy, 
                fset, ssbset)

# load assessment data
ass <-
  get(load(file=paste(dropboxdir, "/rdata/iAssess.RData", sep=""))) %>% 
  filter(stockkeylabelnew %in% unique(fc$stockkeylabelnew)) %>% 
  # filter(assessmentyear == 2019, purpose=="advice") %>% 
  filter(assessmentyear == 2018, purpose=="replaced") %>% 
  dplyr::select(stockkey, stockkeylabelnew, stockkeylabelold, year, 
                ssb=stocksize, f=fishingpressure)

# basic calculations and lags
df <-
  fc %>% 
  left_join(ass, by=c("stockkey","stockkeylabelnew","stockkeylabelold", "year")) %>% 
  filter(year > max(year, na.rm=TRUE)-10) %>% 
  mutate(DevLnF = log(fset / f)) %>% 
  mutate(DevLnSSB = log(ssbset/ssb)) %>% 
  group_by(stockkey, stockkeylabelnew, stockkeylabelold) %>%
  mutate(lagDevLnF = lag(DevLnF, n=1)) %>% 
  mutate(lagDevLnSSB = lag(DevLnSSB, n=1))

# write.csv(df, file="temp2019.csv")
# write.csv(df, file="temp2018.csv")

# calculate Fphi and Fcv
df2 <-
  df %>% 
  mutate(sdDevLnF = sd(DevLnF, na.rm=TRUE)) %>% 
  filter(!is.na(DevLnF), !is.na(lagDevLnF)) %>% 
  summarize(
    sdDevLnF = mean(sdDevLnF, na.rm=TRUE),
    Fphi     = cor(DevLnF, lagDevLnF)) %>% 
  mutate(
    Fcv = sdDevLnF * sqrt(1-Fphi^2)
  )

# calculate SSBphi and SSBcv
df3 <-
  df %>% 
  mutate(sdDevLnSSB = sd(DevLnSSB, na.rm=TRUE)) %>% 
  filter(!is.na(DevLnSSB), !is.na(lagDevLnSSB)) %>% 
  summarize(
    sdDevLnSSB = mean(sdDevLnSSB, na.rm=TRUE),
    SSBphi     = cor(DevLnSSB, lagDevLnSSB)) %>% 
  mutate(
    SSBcv = sdDevLnSSB * sqrt(1-SSBphi^2)
  )

# plot by year
df %>% 
  ggplot(aes(x=year, y=DevLnF)) +
  theme_publication() +
  geom_line() +
  facet_wrap(~stockkeylabelnew)


  
#deviation of logs
dfComp$DevLnF <- log(dfComp$Forecast.F/dfComp$Ass.F)

plot(dfComp$Year[!is.na(dfComp$DevLnF)],dfComp$DevLnF[!is.na(dfComp$DevLnF)],
     type="l",ylim=c(-0.5,0.5), xlab="Year", ylab="Ln(ForecastF) - Ln(AssessmentF)")
abline(h=0)

#sd of the log deviations gives the marginal distribution
sigm.F <- sd(dfComp$DevLnF,na.rm = TRUE)
#autocorrelation at lag 1
phi.F <- acf(dfComp$DevLnF[!is.na(dfComp$DevLnF)])[[1]][2,,]
#conditional SD
sigc.F <- sigm.F * sqrt(1-phi.F^2)

dfComp$DevLnSSB <- log(dfComp$Forecast.SSB/dfComp$Ass.SSB)
plot(dfComp$Year[!is.na(dfComp$DevLnSSB)],dfComp$DevLnSSB[!is.na(dfComp$DevLnSSB)],
     type="l",ylim=c(-0.5,0.5), xlab="Year", ylab="Ln(ForecastSSB) - Ln(AssessmentSSB)")
abline(h=0)

#sd of the log deviations gives the marginal distribution
sigm.SSB <- sd(dfComp$DevLnSSB,na.rm = TRUE)
#autocorrelation at lag 1
phi.SSB <- acf(dfComp$DevLnSSB[!is.na(dfComp$DevLnSSB)])[[1]][2,,]
#conditional SD
sigc.SSB <- sigm.SSB * sqrt(1-phi.SSB^2)

cat("F parameters", sigc.F, phi.F,"\n")
cat("SSB parameters", sigm.SSB, phi.SSB, "\n")

