# -----------------------------------------------------------------------------------------------
# Plot Exceldata and QCS data
#
# 02/02/2018 first coding
# 08/02/2018 fixing lot's of small things in the excel spreadsheet
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

d <-
  readxl::read_excel(path= paste(dropboxdir, "/data/QCS and EXCEL Assessment Database combined.xlsx", sep=""), 
                     sheet = "data",
                     col_names = TRUE, 
                     col_types = "text", 
                     trim_ws   = FALSE) %>%
  lowcase() %>% 
  mutate_at(c("assessmentyear","year", "recruitment","ssb","f", "catches"),   funs(as.numeric)) %>% 
  mutate_at(c("stockkey"),    funs(as.integer)) %>% 
  mutate_at(c("unitofrecruitment", "recruitmentdescription"),    funs(tolower)) %>% 
  mutate(recruitment       = ifelse(unitofrecruitment == "thousands", recruitment/1000, recruitment),
         unitofrecruitment = ifelse(unitofrecruitment == "thousands", "millions", unitofrecruitment),
         recruitment       = ifelse(unitofrecruitment == "billions", recruitment*1000, recruitment),
         unitofrecruitment = ifelse(unitofrecruitment == "billions", "millions", unitofrecruitment),
         tyear             = substr(as.character(assessmentyear),3,4),
         decade            = 10*floor(assessmentyear/10) ) 


d %>% 
  filter(!is.na(ssb)) %>%  
  filter(year >= 1980) %>% 
  # filter(grepl("her-47d3", stockkeylabelold)) %>% 
  filter(grepl("cod-347d", stockkeylabelold)) %>% 
  
  ggplot(aes(year,ssb, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = factor(tyear)) ) +
  geom_dl(aes(label  = tyear, colour = factor(tyear)), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "ssb")  +
  facet_wrap( ~ decade , scales="free_y")

# plot recent years only
d %>% 
  filter(!is.na(ssb)) %>%  
  filter(year >= 2000) %>%
  filter(assessmentyear >= 2010) %>% 
  filter(assessmenttype == "assess") %>% 
  filter(grepl("her-47d", stockkeylabelold)) %>% 
  select(stockkeylabelold, assessmentyear, year, tyear, ssb, f, recruitment) %>% 
  gather(key=variable, value=value, ssb:recruitment) %>% 
  filter(variable %in% c("ssb","f")) %>% 
  
  # View()

  ggplot(aes(year, value, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = tyear) ) +
  geom_dl(aes(label  = tyear, colour = tyear), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  # expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "ssb") +
  facet_wrap(~variable, scales="free_y")


######


# plotting the end points of each assessment
d %>% 
  filter(!is.na(ssb)) %>%  
  filter(year >= 1990) %>%
  filter(assessmentyear >= 1991) %>% 
  filter(assessmenttype == "assess") %>% 
  filter(grepl("her-47d", stockkeylabelold)) %>% 
  filter(year == assessmentyear-1) %>% 
  select(stockkeylabelold, assessmentyear, year, tyear, ssb, f, recruitment) %>% 
  gather(key=variable, value=value, ssb:recruitment) %>% 
  filter(variable %in% c("ssb","f")) %>% 
  
  # View()
  
  ggplot(aes(year, value)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line() +
  geom_dl(aes(label  = tyear, colour = tyear), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  # expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "ssb and F endpoints of assessments") +
  facet_wrap(~variable, scales="free_y")



d %>% 
  filter(!is.na(ssb)) %>%  
  filter(year %in% 2000:2016) %>%
  filter(assessmentyear %in% c(2016, 2018)) %>%
  # filter(assessmenttype == "assess") %>% 
  filter(grepl("her-47d", stockkeylabelold)) %>% 
  select(stockkeylabelold, assessmentyear, year, tyear, ssb, f, recruitment) %>% 
  gather(key=variable, value=value, ssb:recruitment) %>% 
  filter(variable %in% c("ssb","f")) %>% 
  group_by(assessmentyear, variable) %>% 
  summarize(value = mean(value, na.rm=TRUE)) 

d %>% 
  filter(!is.na(recruitment)) %>%  
  filter(recruitmentdescription == "recruitment") %>% 
  filter(year >= 1985) %>% 
  filter(stockkeylabelold == "her-47d3") %>% 
  
  ggplot(aes(year,recruitment, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = source) ) +
  geom_dl(aes(label  = tyear, colour = source), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "recruitment")  +
  facet_grid(unitofrecruitment ~ recruitmentage , scales="free_y")


# plot recruitment
# p1 <-
d %>% 
  filter(!is.na(recruitment)) %>%  
  filter(recruitmentdescription == "recruitment") %>% 
  filter(year >= 1985) %>% 
  filter(unitofrecruitment == "millions") %>% 
  # filter(stockkeylabelold == "ane-bisc") %>% 
  
  # filter(substr(stockkeylabelold,1,1) %in% letters[seq( from = 1, to = 7 )] ) %>%
  filter(substr(stockkeylabelold,1,1) %in% letters[seq( from = 8, to = 14 )] ) %>%
  # filter(substr(stockkeylabelold,1,1) %in% letters[seq( from = 15, to = 26 )] ) %>% 

  ggplot(aes(year,recruitment, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = source) ) +
  
  geom_dl(aes(label  = tyear, colour = source), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "recruitment")  +
  facet_wrap(~ stockkeylabelold, scales="free_y")
  # facet_grid(unitofrecruitment ~ recruitmentage , scales="free_y")




# d %>%  
#   filter(!is.na(recruitment)) %>% 
#   distinct(stockkeylabelold) %>% 
#   mutate(code=substr(stockkeylabelold,1,1)) %>% 
#   group_by(code) %>% 
#   summarize(count=n()) %>% 
#   mutate(cum = cumsum(count))
# 
#   ungroup() %>% 
#   summarize(count=sum(count))



# plot f
# p2 <-
d %>% 
  filter(!is.na(f)) %>%  
  filter(year >= 1990) %>% 
  filter(substr(stockkeylabelold,1,1) %in% letters[seq( from = 1, to = 4 )] ) %>% 
  mutate(tyear             = substr(as.character(assessmentyear),3,4)) %>% 
  
  ggplot(aes(year,f, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = source) ) +
  
  geom_dl(aes(label  = tyear, colour = source), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "F")  +
  facet_wrap(~ stockkeylabelold, scales="free_y")


# plot ssb
# p2 <-
d %>% 
  filter(!is.na(ssb)) %>%  
  filter(year >= 1990) %>% 
  filter(substr(stockkeylabelold,1,1) %in% letters[seq( from = 1, to = 4 )] ) %>% 
  
  ggplot(aes(year,ssb, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = source) ) +
  
  geom_dl(aes(label  = tyear, colour = source), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  +
  facet_wrap(~ stockkeylabelold, scales="free_y")


# plot catch
# p4 <-
d %>% 
  filter(!is.na(catches)) %>%  
  filter(year >= 1985) %>% 
  # filter(substr(stockkeylabelold,1,1) %in% letters[seq( from = 1, to = 4 )] ) %>% 
  filter(substr(stockkeylabelold,1,1) %in% letters[seq( from = 8, to = 14 )] ) %>%
  
  ggplot(aes(year,catches, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = source) ) +
  
  geom_dl(aes(label  = tyear, colour = source), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "catch")  +
  facet_wrap(~ stockkeylabelold, scales="free_y")



d %>% 
  filter(!is.na(recruitment)) %>%  
  filter(recruitmentdescription == "recruitment") %>% 
  filter(year >= 1985) %>% 
  filter(stockkeylabelold == "her-47d3") %>% 
  
  ggplot(aes(year,recruitment, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = source) ) +
  geom_dl(aes(label  = tyear, colour = source), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "recruitment")  +
  facet_grid(unitofrecruitment ~ recruitmentage , scales="free_y")



# summary plot for four variables of one 'stock'
d %>% 
  filter(year >= 1985) %>% 
  filter(grepl("her-4", stockkeylabelold)) %>% 
  select(stockkeylabelold, assessmentyear, tyear, year, source,
         recruitment, ssb, f, landings) %>% 
  gather(key=variable, value=value, recruitment:landings) %>% 
  filter(!is.na(value)) %>% 
  mutate(value = as.numeric(value)) %>% 
  data.frame() %>% 
  
  ggplot(aes(year,value, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = source) ) +
  
  geom_dl(aes(label  = tyear, colour = source), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL)  +
  facet_wrap(~variable, scales="free_y") 

# plot stock-recruitment relationship
d %>% 
  filter(!is.na(recruitment)) %>%  
  filter(recruitmentdescription == "recruitment") %>% 
  filter(assessmentyear == 2015) %>% 
  filter(unitofrecruitment == "millions") %>% 
  filter(stockkeylabelold == "cod-347d") %>% 
  mutate(recruitment2 = lead(recruitment, n=1)) %>%
  mutate(decade       = 10*floor(year/10) ) %>% 

  ggplot(aes(ssb,recruitment2, group=decade)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9)
        # strip.background = element_blank(),
        # legend.position = "null"
  ) +
  
  geom_point(aes(colour = factor(decade)), size=2 ) +
  geom_line(aes(colour = factor(decade))) +
  
  expand_limits(y = 0, x=0) +
  labs(x = NULL, y = NULL , title = "stock and recruitment")  +
  facet_wrap(~ stockkeylabelold, scales="free_y")
# facet_grid(unitofrecruitment ~ recruitmentage , scales="free_y")
