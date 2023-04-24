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

fn <- "C:/TEMP/collection_5796935_v93_citations.ris"

# read RIS file
ris <-
  readLines(fn) %>%
  as.data.frame() %>%
  setNames("string") %>%
  filter(grepl("T1 - |L4 - |KW - ", string)) %>%
  separate(string, into=c("code","data"), sep=" - ") %>%
  mutate(helper = ifelse(grepl("T1",code),1,0)) %>% 
  mutate(id = cumsum(helper)) %>% 
  dplyr::select(-helper) 

tmp <-
  ris %>% 
  filter(code=="KW") %>% 
  ungroup() %>% 
  mutate(data=tolower(data)) %>% 
  distinct(data) %>% 
  arrange(data) %>% 
  filter(grepl("^[[:alpha:]]{3}\\.", data))

# keywords
kw <-
  ris %>% 
  filter(code=="KW") %>% 
  mutate(data=tolower(data)) %>% 
  filter(grepl("^[[:alpha:]]{3}\\.", data)) %>% 
  # filter(grepl("\\.27\\.", data)) %>% 
  group_by(id) %>% 
  mutate(n     = row_number()) %>% 
  mutate(nrows = max(n)) %>% 
  filter(nrows==1) %>% 
  rename(name=data) %>% 
  dplyr::select(-code, -n, -nrows)

# link
l4 <-
  ris %>% 
  filter(code=="L4") %>% 
  filter(grepl("downloader", data)) %>% 
  dplyr::select(-code) %>% 
  left_join(kw) %>% 
  drop_na(name) %>% 
  arrange(name, id) %>% 
  group_by(name) %>% 
  mutate(n=row_number()) %>% 
  mutate(name = ifelse(n>1, paste(name, n, sep="_"), name)) %>% 
  mutate(name = gsub("\\\\|/"," ", name))

i <- 1
# for (i in 1:nrow(l4)) {
for (i in 1:10) {
  print(paste(i, l4$data[[i]]))
  
  invisible(
    httr::GET(l4$data[[i]], write_disk(paste0("C:/TEMP/ACOM 2022 ",l4$name[[i]],".pdf"), overwrite=TRUE))
  )
}


# assessmentkeys <- sort(findAssessmentKey(year=myyear))
# assessmentkeys <- findAssessmentKey(year=1983, full=TRUE)
# assessmentkeys <- sort(findAssessmentKey())
# assessmentkeys[assessmentkeys == 10362]
# assessmentkeys <- findAssessmentKey(year=2019, full=TRUE)
# getStockDownloadData(7306)

# OVERVIEWS 
kw <-
  ris %>% 
  filter(code=="T1") %>% 
  mutate(data=tolower(data)) %>% 
  filter(grepl("overview", data)) %>%
  group_by(id) %>% 
  mutate(n     = row_number()) %>% 
  mutate(nrows = max(n)) %>% 
  filter(nrows==1) %>% 
  rename(name=data) %>% 
  dplyr::select(-code, -n, -nrows)

# link
l4 <-
  ris %>% 
  filter(code=="L4") %>% 
  filter(grepl("downloader", data)) %>% 
  dplyr::select(-code) %>% 
  left_join(kw) %>% 
  drop_na(name) %>% 
  arrange(name, id) %>% 
  group_by(name) %>% 
  mutate(n=row_number()) %>% 
  mutate(name = ifelse(n>1, paste(name, n, sep="_"), name)) %>% 
  mutate(name = gsub("\\\\|/"," ", name))

for (i in 1:nrow(l4)) {
  print(paste(i, l4$data[[i]]))
  invisible(
    httr::GET(l4$data[[i]], write_disk(paste0("C:/TEMP/ACOM 2022 ",l4$name[[i]],".pdf"), overwrite=TRUE))
  )
}


# SPECIAL REQUESTS 
kw <-
  ris %>% 
  filter(code=="T1") %>% 
  mutate(data=tolower(data)) %>% 
  filter(grepl("request", data)) %>%
  group_by(id) %>% 
  mutate(n     = row_number()) %>% 
  mutate(nrows = max(n)) %>% 
  filter(nrows==1) %>% 
  rename(name=data) %>% 
  dplyr::select(-code, -n, -nrows)

# link
l4 <-
  ris %>% 
  filter(code=="L4") %>% 
  filter(grepl("downloader", data)) %>% 
  dplyr::select(-code) %>% 
  left_join(kw) %>% 
  drop_na(name) %>% 
  arrange(name, id) %>% 
  group_by(name) %>% 
  mutate(n=row_number()) %>% 
  mutate(name = ifelse(n>1, paste(name, n, sep="_"), name)) %>% 
  mutate(name = gsub("\\\\|/"," ", name))


# HOW TO CAPTURE ERRORS in httr::GET??
i <-2
# for (i in c(1,3:7,9:16,18:25)) {
for (i in 1:nrow(l4)) {
  print(paste(i, l4$data[[i]]))
  ff      <- 
    try(
      httr::GET(l4$data[[i]])
    )

  if (class(ff) != "try-error") {
    invisible(
      httr::GET(l4$data[[i]], write_disk(paste0("C:/TEMP/ACOM 2022 ",
                                                substr(l4$name[[i]], 1,50),".pdf"), overwrite=TRUE))
    )
  }
}

# TECHNICAL SERVICES 
kw <-
  ris %>% 
  filter(code=="T1"|code=="KW") %>% 
  mutate(data=tolower(data)) %>% 
  filter(grepl("technical", data)) 
  