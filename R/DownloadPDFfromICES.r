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
  filter(grepl("T1 - |L4 - |KW - ", string)) %>%
  separate(string, into=c("code","data"), sep=" - ") %>% 

  # filter(KW = her.27.etc.)
  
  bind_cols(id = sort(rep(seq(1,230,1),2))) %>% 
  tidyr::pivot_wider(names_from = code, values_from = data) %>% 
  drop_na(T1) %>% 
  mutate(string = gsub("L4 - ","", string))

i <- 1
for (i in 1:nrow(t)) {
  print(paste(t$id[[i]],t$T1[[i]]))
  httr::GET(t$L4[[i]], write_disk(paste0("C:/TEMP/",t$T1[[i]],".pdf"), overwrite=TRUE))
}

httr::GET(t[[2,1]], write_disk("C:/TEMP/test2.pdf", overwrite=TRUE))

# assessmentkeys <- sort(findAssessmentKey(year=myyear))
# assessmentkeys <- findAssessmentKey(year=1983, full=TRUE)
# assessmentkeys <- sort(findAssessmentKey())
# assessmentkeys[assessmentkeys == 10362]
# assessmentkeys <- findAssessmentKey(year=2019, full=TRUE)
# getStockDownloadData(7306)

