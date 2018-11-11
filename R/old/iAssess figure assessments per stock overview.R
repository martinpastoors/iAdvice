# -----------------------------------------------------------------------------------------------
# iAssess figure all assessments per stock
#
# 25/09/2017 taken from ices SAG data plot
# 16/10/2017 now includes datapublished
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
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))

# ---------------------------------------------------------------------------------------------
# plots of assessments by assessment year and stock and fisheries guild (for a single variable)
# ---------------------------------------------------------------------------------------------

# To do: convert this into a function that can plot the different variables. I have struggled with this a lot but 
# without success. 1/8/2017

x <-
  iAssess %>% 
  # filter(assessmentyear >= 1986) %>% 
  # filter(fisheriesguild != "", !is.na(fisheriesguild)) %>% 
  # filter(fisheriesguild %in% c("crustacean","elasmobranch","pelagic","demersal","benthic")) %>%
  group_by(stockkeylabelold, stockkeylabelnew, assessmentyear, assessmenttype2, source) %>% 
  filter(row_number() ==1) %>% 
  select(-year) %>% 
  ungroup() %>% 
  arrange(stockkeylabelold) %>% 
  mutate(id = group_indices(., stockkeylabelold)) %>% 
  data.frame() %>% 
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  mutate(stockkeylabelold = factor(stockkeylabelold), 
         stockkeylabelold = factor(stockkeylabelold, levels = rev(levels(stockkeylabelold))),
         source           = factor(source, levels=c("qcs","excel","sag")),
         assessmenttype2  = factor(assessmenttype2),
         datepublished    = ifelse(is.na(datepublished), 
                                   make_date(year = assessmentyear, month = 6L, day = 30L),
                                   datepublished),
         datepublished    = as.Date(datepublished, origin="1970-01-01"),
         col              = ifelse(id <=  90            , 1, NA),
         col              = ifelse(id  >  90 & id <= 180, 2, col),
         col              = ifelse(id  > 180            , 3, col)) 


# define colour scale
myColors        <- brewer.pal(length(levels(x$source)),"Set1")
names(myColors) <- levels(x$source)

myAssessmenttypeColors        <- brewer.pal(length(levels(x$assessmenttype2)),"Set1")
names(myAssessmenttypeColors) <- levels(x$assessmenttype2)

# define headers for columns
y <-
  x %>% 
  group_by(col) %>% 
  filter(row_number()==1| row_number() == n()) %>% 
  ungroup() %>% 
  mutate(id = group_indices(., col)) %>% 
  select(col, id, stockkeylabelold) %>% 
  group_by(col) %>% 
  summarise(code = paste(stockkeylabelold, collapse=" : "))


# plot by stock and data source

x %>% 
  left_join(y, by="col") %>% 
  ggplot(aes(x=datepublished, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)

# plot by stock and assessment type

x %>% 
  left_join(y, by="col") %>% 
  ggplot(aes(x=datepublished, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = assessmenttype2)) +
  scale_colour_manual(name = "assessmenttype2", values = myAssessmenttypeColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)

# Alternative plot below: by fisheries guild

p1 <-
  filter(x, fisheriesguild %in% c("crustacean","elasmobranch")) %>% 
  ggplot(aes(x=datepublished, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(x = " ", y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2a <-
  filter(x, fisheriesguild %in% c("pelagic")) %>% 
  ggplot(aes(x=datepublished, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(x = NULL, y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2b <-
  filter(x, fisheriesguild %in% c("benthic")) %>% 
  ggplot(aes(x=datepublished, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE, ncol=1)

p2 <- plot_grid(p2a + 
                  theme(legend.position="none", 
                        axis.title.y=element_blank(), 
                        axis.text.x=element_blank(), 
                        plot.margin = unit(c(10, 5, 1, 5), "mm")), 
                p2b + 
                  theme(legend.position="none", 
                        axis.title.y=element_blank(), 
                        plot.margin = unit(c(1, 5, 5, 5), "mm")),
                ncol=1,align="v",
                rel_heights = c(4.7, 2))

p3 <-
  filter(x, fisheriesguild %in% c("demersal")) %>% 
  ggplot(aes(x=datepublished, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        legend.position  = "right",
        legend.direction = "vertical",
        legend.title     = element_blank()) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(y = "fishstock", x=" " ) +
  facet_wrap(~fisheriesguild, scales="free_y", shrink=TRUE)

plot_grid(p1 + theme(legend.position="none") + theme(axis.title.y=element_blank()), 
          p2 ,
          p3 ,
          ncol=3,
          align = 'v', rel_widths = c(1,1,1.2))

# unique(sagdb$fisheriesguild)
# unique(x$stockpublishnote)
# unique(x$assessmentcat)
# unique(x$status)



