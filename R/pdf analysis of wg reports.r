# Search through pdf documents

# install.packages("devtools")
# devtools::install_github('lebebr01/pdfsearch')
library(pdftools)  # generic pdf reader
library(pdfsearch) # wrapper around pdftools for searching
library(tidyverse)

# Load utils code
source("../mptools/r/my utils.r")

# end of text pages 
# y <- 2010
# f <- files[92]

comb <- NULL

# for (y in 1997:2019) {
for (y in 2010:2020) {
  files <- list.files(path = file.path("D:/ZZZ"),
                      pattern = paste0("^WGWIDE"),
                      # pattern = paste0("^WG"),
                      # pattern = paste0("WGNSSK[*]{.}",y,"*"), 
                      recursive = FALSE,
                      full.names = TRUE)  %>% 
    grep(pattern=paste0(y,"\\.pdf"), inv=F, value=T) 
  
  for(f in files) {
    
    gc()
    print(f)
    
    t <- 
      pdf_text(f) %>% 
      read_lines() %>% 
      tolower() %>% 
      stringr::str_trim(side="both") %>% 
      as.data.frame() %>% 
      setNames(c("text")) 
    
    tst <- c("^[ ]{0,1}table[ ]{0,1}[0-9]{1}|^[ ]{0,1}figure[ ]{0,1}[0-9]{1}")
    # tst <- c("table[ ]{0,1}[0-9]{1}|figure[ ]{0,1}[0-9]{1}")
    
    tmp <- 
      t %>% 
      filter(grepl(tst, text)) %>%
      mutate(type = ifelse(grepl("^[ ]{0,1}table",text), "table","figure")) %>% 
      mutate(year = y, file = f)
    
    comb <- bind_rows(comb, tmp)
    
  } # for f
} # for year

tmp <-
  comb %>% 
  filter(nchar(basename(file)) < 25) %>% 
  mutate(number = stringr::str_trim(stringr::word(text, 2))) %>% 
  mutate(number = stringr::str_remove(number, "\\.$|\\:$|\\;$")) %>% 
  mutate(section = as.integer(stringr::str_extract(number, "^[0-9]{1,2}"))) %>% 
  filter( section < 15) %>% 
  mutate(section=as.factor(section)) %>% 
  group_by(file, year, section, type) %>% 
  summarise(n=n())

tmp %>% 
  ggplot(aes(year, n)) +
  geom_bar(aes(fill=section, group=section), stat="identity") +
  facet_wrap(~type)



glimpse(comb)
pdf_text(f)
x <- (pdf_toc(f))
x[2]
list2DF(x) %>% View()

# UNFINISHED


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

