# -----------------------------------------------------------------------------------------------
# SAG DLS categories
#
# 02/09/2018 first coding
# -----------------------------------------------------------------------------------------------

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
assessdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# load datasets: sag, sagful, qcsexcel, istock, iadvice
sagfull <- 
  get(load(file=file.path(assessdir, "rdata/icesSAGfull 20180812.RData")))

sagfull %>% 
  group_by(assessmentyear, )
  filter(row_number() == 1) %>% 
  t() %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 80, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

sagfull %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>%  
  setNames(c("var","tmp")) %>%
  mutate(tmp = substr(tmp, 1, 50)) %>% 
  arrange(var) %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 100, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

qcsexcel %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 80, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

istock %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 80, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

advice %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 80, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )


# --------------------------------------------------------------------------------
# exploring
# --------------------------------------------------------------------------------

sort(unique(istock$assessmentstatus))
sort(unique(qcsexcel$purpose))
sort(unique(sagfull$purpose))
sort(unique(sag$stockpublishnote))
sort(unique(sag$stocksizedescription))
sag %>% filter(stocksizedescription == "Stock Size: TSB") %>% View()
sag %>% filter(assessmentyear==2018) %>% distinct(fishstock) %>% View()
sag %>% filter(assessmentyear==2016, fishstock %in% c("cod-iceg","cod-5a")) %>%  View()

sagfull %>% filter(stockkeylabel == "her.27.3a47d", assessmentyear == "2017") %>% View()
sagfull %>% filter(stockkeylabel == "her-47d3", assessmentyear == "2016") %>% View()
sagfull %>% filter(purpose == "Historical") %>% View()

count_not_na(qcsexcel) %>% data.frame() %>% rownames_to_column() %>%   setNames(c("var","count")) %>% arrange(-count) %>% View()

sagfull %>% select(-one_of(grepl("custom",names(.)))) %>% head()
names(sagfull)


# --------------------------------------------------------------------------------
# plot historical retro SSB
# --------------------------------------------------------------------------------
d <-
  sagfull %>% 
  filter(grepl("her-47d3|cod-347d", stockkeylabelold) ) %>% 
  ungroup() %>% 
  filter(year             >   1985, 
         assessmentyear   >   2000,
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew,
         stocksize, lowstocksize, highstocksize, purpose, published) %>%
  
  mutate(stocksize = as.numeric(stocksize),
         year      = as.numeric(year),
         key       = paste(assessmentyear,purpose,published)) %>%
  
  data.frame()

d %>% 
  filter(assessmentyear   >   2013) %>% 
  
  ggplot(aes(year,stocksize, group=key)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9)) +
  
  geom_path(aes(colour=as.factor(published) )) +
  
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  +
  facet_grid(stockkeylabelold ~ ., scales="free_y")


# historical
sagfull %>% 
  filter(tolower(purpose) ==  "advice") %>% 
  filter(!is.na(stocksize)) %>% 
  ungroup() %>% 
  filter(year             >   1985,
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew,
         stocksize, lowstocksize, highstocksize, purpose, published) %>%
  mutate(stocksize = as.numeric(stocksize),
         year      = as.numeric(year),
         key       = paste(assessmentyear,purpose,published)) %>%
  data.frame() %>% 

  ggplot(aes(year,stocksize, group=key)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9)) +
  geom_path() +
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  +
  facet_wrap(stockkeylabelold ~ ., scales="free_y")

sort(unique(sagfull$stocksizeunits))



