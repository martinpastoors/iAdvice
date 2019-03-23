# -----------------------------------------------------------------------------------------------
# compare databases istock advice assessment
#
# 12/08/2018 first coding
# 07/11/2018 updated coding after revisions to iAdvice and QCSExcel files
# -----------------------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files
library(stringr)   # string manipulation

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
assessdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")
advicedir <- paste(get_dropbox(), "/ICES Advice database", sep="")

# load datasets: sag, sagful, qcsexcel, istock, iadvice
sag <- 
  get(load(file=file.path(assessdir, "rdata/icesSAG 20180812.RData")))

sagfull <- 
  get(load(file=file.path(assessdir, "rdata/icesSAGfull 20181023.RData")))

sd <-
  get(load(file=file.path(assessdir, "rdata/icesSD 20181023.RData")))

qcsexcel <-
  readxl::read_excel(
    path= paste(assessdir, "/excel/QCS and EXCEL Assessment Database combined.xlsx", sep=""), 
    sheet = "data",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() 


istock <-
  readxl::read_excel(
    path= paste(assessdir, "/excel/iStock.xlsx", sep=""), 
    sheet     = "istock",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() 

advice <-
  readxl::read_excel(
    path= paste(advicedir, "/excel/ICES Scientific Advice database 20181101.xlsx", sep=""), 
    sheet     = "DATA",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() 

# --------------------------------------------------------------------------------
# summarize datasets (variables)
# --------------------------------------------------------------------------------

sag_ <-
  sag %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>%  
  setNames(c("var","tmp")) %>%
  mutate(tmp = substr(tmp, 1, 50),
         db  = "SAG") %>% 
  arrange(var) 

  sag_ %>% 
    pandoc.table(., 
               style        = "simple",
               split.tables = 100, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

sagfull_ <-
  sagfull %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>%  
  setNames(c("var","tmp")) %>%
  mutate(tmp = substr(tmp, 1, 50),
         db  = "SAGfull") %>% 
  arrange(var) 
  
sagfull_ %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 100, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

sd_ <-
  sd %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>%  
  setNames(c("var","tmp")) %>%
  mutate(tmp = substr(tmp, 1, 50),
         db  = "SD") %>% 
  arrange(var) 

qcsexcel_ <-
  qcsexcel %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>%  
  setNames(c("var","tmp")) %>%
  mutate(tmp = substr(tmp, 1, 50),
         db  = "qcsexcel")

qcsexcel_ %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 100, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

istock_ <-
  istock %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>%  
  setNames(c("var","tmp")) %>%
  mutate(tmp = substr(tmp, 1, 50),
         db  = "istock")

istock_ %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 100, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )

advice_ <-
  advice %>% 
  filter(row_number() == 1) %>% 
  t() %>% 
  data.frame() %>% 
  rownames_to_column() %>%  
  setNames(c("var","tmp")) %>%
  mutate(tmp = substr(tmp, 1, 50),
         db  = "iadvice")

advice_ %>% 
  pandoc.table(., 
               style        = "simple",
               split.tables = 100, 
               split.cells  = c(rep(7,10)),
               justify      = "left",
               missing      =".",
               big.mark     = ',', 
               round        = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) )


# --------------------------------------------------------------------------------
# join
# --------------------------------------------------------------------------------

comp <-
  bind_rows(sag_, sagfull_, sd_, qcsexcel_, istock_, advice_) %>% 
  mutate(var = str_trim(tolower(as.character(var)), side="both")) %>% 
  dplyr::select(-tmp) %>% 
  group_by(var) %>% 
  mutate(n="x") %>% 
  # mutate(n=n(), all=n) %>% 
  spread(key=db, value=n) %>% 
  filter(!grepl("custom", var)) 
# %>% arrange(-all, var)

write.csv(comp, file="comp 20181107.csv", row.names = FALSE)

# unique(sagfull$typestock)
# istock %>% filter(stockkeylabel == "cod-22_24") %>% View()


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



# --------------------------------------------------------------------------------
# plot historical retro SSB
# --------------------------------------------------------------------------------
d <-
  sagfull %>% 
  filter(grepl("her-47d3|cod-347d", stockkeylabelold) ) %>% 
  ungroup() %>% 
  filter(year             >   1985, 
         year             <= assessmentyear) %>% 
  select(assessmentyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew,
         stocksize, lowstocksize, highstocksize, purpose, published) %>%
  
  mutate(stocksize = as.numeric(stocksize),
         year      = as.numeric(year),
         key       = paste(assessmentyear,purpose,published)) %>%
  
  data.frame()

d %>% 
  # filter(assessmentyear   >   2013) %>% 
  
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



