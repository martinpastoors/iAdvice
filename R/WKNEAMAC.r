# -----------------------------------------------------------------------------------------------
# ICES Stock Assessment Graph plotting
#
# 30/03/2017 first coding during HAWG
# 14/07/2017 adapted during HERAS
# 11/08/2017 adapter for R 3.4.1 and tidyverse
# 14/08/2017 added plot for assessment methods
# 04/10/2017 only plot for retro. Updated for iAssess
# 07/03/2019 adapted for new database layout
# -----------------------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Load utils code
source("../mptools/r/my utils.r")

df <- readxl::read_excel("D:/temp/test.xlsx")

df %>%
  pivot_longer(names_to="var", values_to="value",Catch:IAV) %>% 
  
  ggplot(aes(x=Ftarget, y=value, group=Btrigger)) +
  theme_bw() +
  geom_line(aes(colour=factor(Btrigger))) +
  expand_limits(y=0) +
  facet_wrap(~var, scales="free_y")

df %>%
  pivot_longer(names_to="var", values_to="value",Catch:IAV) %>% 
  filter(var=="Catch") %>% 
  
  ggplot(aes(x=Ftarget, y=value, group=Btrigger)) +
  theme_bw() +
  geom_line(aes(colour=factor(Btrigger))) +
  expand_limits(y=0) +
  facet_wrap(~Btrigger)


