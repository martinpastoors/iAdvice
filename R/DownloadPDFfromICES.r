# ============================================================================
# Download PDFs from ICES
#
# 16/11/2022 First coding
# ============================================================================

library(tidyverse)
library(stringr)       # for string manipulation
library(httr)

# Load utils code
source("../prf/r/my utils.r")

fn <- "C:/TEMP/collection_5796935_v72_citations.ris"

t <- 
  readLines(fn) %>% 
  as.data.frame() %>% 
  setNames("string") %>% 
  filter(grepl("T1 - |L4 - |ER -", string)) %>%
  separate(string, into=c("code","data"), sep=" - ") %>% 
  mutate(id = ifelse(row_number()==1,1,NA)) %>% 
  
  # How to deal with records with one T1 and multiple L4?
  # Can I set an index to each record, e.g. based on ER -?
  
  mutate(id = ifelse(is.na(data),lag(id)+1,lag(id))) %>% 
  bind_cols(id = sort(rep(seq(1,230,1),2))) %>% 
  tidyr::pivot_wider(names_from = code, values_from = data) %>% 
  mutate(string = gsub("L4 - ","", string))

class(t)
glimpse(t)

download.file(t[1], "C:/TEMP/test.pdf")
httr::GET(t[[1,1]], write_disk("C:/TEMP/test.pdf", overwrite=TRUE))
httr::GET(t[[2,1]], write_disk("C:/TEMP/test2.pdf", overwrite=TRUE))

# assessmentkeys <- sort(findAssessmentKey(year=myyear))
# assessmentkeys <- findAssessmentKey(year=1983, full=TRUE)
# assessmentkeys <- sort(findAssessmentKey())
# assessmentkeys[assessmentkeys == 10362]
# assessmentkeys <- findAssessmentKey(year=2019, full=TRUE)
# getStockDownloadData(7306)

