# Search through pdf documents

# install.packages("devtools")
# devtools::install_github('lebebr01/pdfsearch')
library(pdftools)  # generic pdf reader
library(pdfsearch) # wrapper around pdftools for searching
library(tidyverse)

# Load utils code
source("../mptools/r/my_utils.r")

# end of text pages 
# y <- 2010
# f <- files[92]

pages <- NULL
for (y in 1997:2019) {
# for (y in 2010:2010) {
  files <- list.files(path = file.path("D:/temp/advice",y),
                      pattern = ".pdf", 
                      recursive = FALSE,
                      full.names = TRUE)  
  
  for(f in files) {
    gc()
    print(f)
    txt <- paste0(pdf_text(f), collapse=" ")
    txt <- str_replace_all(txt,"\r\n"," ")
    txt <- str_replace_all(txt, "\\,", " ")
    txt <- str_replace_all(txt, "\\s+", " ")
    
    tst1 <- c("history of the advice", 
              "single stock exploitation boundaries (advice) management and catch",
              "advice management and",
              "source of information", 
              "comparison with previous assessment",
              "elaboration and special comments",
              "reference points")
  
    t1 <- unlist(stringr::str_locate_all(pattern =paste0(tst1, collapse="|"), tolower(txt)))
    t1 <- ifelse(length(t1)>0, max(t1), NA)
    
    tst2 <- c('rebuilding plan', "recovery plan")
    t2 <- (unlist(stringr::str_locate_all(pattern =paste0(tst2, collapse="|"), tolower(txt))))
    t2 <- ifelse(length(t2)>0, min(t2), NA)
    
    pages <- bind_rows(pages, data.frame(year=y, file=f, t1 = t1, t2=t2))
  } # for f
} # for year

# save(pages, file="rebuilding1999-2019.RData")
# load(file="rebuilding1999-2019.RData")
pages_all <-
  pages %>% 
  mutate(filename = tolower(basename(file))) %>%  
  filter(grepl("^([a-z]{3})[-\\.]", tolower(filename))) %>% 
  filter(!grepl("replaced", filename)) %>% 
  group_by(year) %>% 
  summarize(n=n()) %>% 
  ungroup() 
  
# plot totals
pages %>% 
  mutate(filename = tolower(basename(file))) %>%  
  filter(t2 < t1) %>% 
  filter(grepl("^([a-z]{3})[-\\.]", tolower(filename))) %>% 
  filter(!grepl("replaced", filename)) %>% 
  group_by(year) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x=year, y=n)) +
  theme_publication() +
  geom_bar(data=pages_all, stat="identity", fill="red", alpha=0.5) +
  geom_bar(stat="identity", fill="blue", alpha=0.6) 

# plot totals for rebuilding
pages %>% 
  mutate(filename = tolower(basename(file))) %>%  
  filter(t2 < t1) %>% 
  filter(grepl("^([a-z]{3})[-\\.]", tolower(filename))) %>% 
  filter(!grepl("replaced", filename)) %>% 
  group_by(year) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x=year, y=n)) +
  theme_publication() +
  geom_bar(stat="identity", fill="blue", alpha=0.6) 

# plot by species
pages %>% 
  mutate(filename = tolower(basename(file))) %>%  
  filter(t2 < t1) %>% 
  filter(grepl("^([a-z]{3})[-\\.]", tolower(filename))) %>% 
  filter(!grepl("replaced", filename)) %>% 
  mutate(species = substr(filename,1,3)) %>%  
  mutate(species = ifelse(species %in% c("anb","anp"), "ang", species)) %>% 
  filter(!(species %in% c("eel", "sal", "rjb", "hom","dgs", "pil")) ) %>% 
  group_by(year, species) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  
  ggplot(aes(x=year, y=n)) +
  theme_publication() +
  theme(legend.position = "none") +
  geom_bar(aes(fill=species), stat="identity") +
  scale_y_continuous(breaks=seq(0,10,2)) +
  scale_x_continuous(breaks=seq(1998,2020,4)) +
  facet_wrap(~species)


pages %>% 
  mutate(filename = basename(file)) %>%  
  filter(t2 < t1) %>% 
  filter(grepl("^([a-z]{3})[-\\.]", tolower(filename))) %>% 
  filter(!grepl("replaced", filename)) %>% 
  filter(year == 2003) %>% 
  View()

