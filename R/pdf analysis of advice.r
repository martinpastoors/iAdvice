# Search through pdf documents

# install.packages("devtools")
# devtools::install_github('lebebr01/pdfsearch')

library(pdfsearch)
library(tidyverse)

# loop over years
for (y in 1981:2018) {
  assign(paste("results",y,sep=""), {
    
    # print(y)
    print(file.path("E:/ices/advice",y))
    
    keyword_directory(file.path("E:/ices/advice",y),
                      keyword = c('rebuilding plan'),
                      file_pattern   = ".pdf",
                      ignore_case    = TRUE, recursive = TRUE, 
                      surround_lines = 1, full_names     = TRUE) %>% 
      # keyword_directory(file.path("E:/PA",y),
      #                 keyword = c('precautionary approach', 
      #                             'biological reference point',
      #                             "minimum biologically acceptable", "mbal", 
      #                             "safe biological limit",
      #                             "maximum sustainable yield", "msy",
      #                             "btrigger", "bpa", "blim", "fpa", "fmsy"),
      #                 # file_pattern   = ".pdf"
      #                 ignore_case    = TRUE, recursive = FALSE, surround_lines = 1, full_names     = TRUE) %>% 
      mutate(year = y) 
    
  })
}
  
for (y in 1981:2018) {
  if (y==1981) {
    results <- get(eval(paste("results",y,sep="")))
  } else {
    results <- bind_rows(results, get(eval(paste("results",y,sep=""))))
  }  
}

results2 <-
  results %>% 
  mutate(keyword = ifelse(keyword == "minimum biologically acceptable" & !grepl("mbal|MBAL", line_text),
                          "mbal", keyword)) %>% 
  filter(keyword != "minimum biologically acceptable") 

results3 <-
  results2 %>% 
  distinct(year, ID, .keep_all=FALSE) %>%
  group_by(year) %>% 
  summarize(nfiles=n()) 
  

results2 %>% 
  group_by(year, keyword) %>% 
  summarize(n=n()) %>% 
  
  left_join(results3, by="year") %>% 
  mutate(freq = n/nfiles) %>% 
  
  filter(year >= 1999) %>% 
  
  ggplot(aes(x=year, y=freq)) +
  geom_bar(stat="identity") +
  # geom_line(data=results3, aes(y=nfiles)) +
  facet_wrap(~keyword, scale="free_y") 


results %>% 
  filter(keyword == "minimum biologically acceptable") %>% 
  filter(!grepl("mbal|MBAL", line_text)) %>% 
  View()

str(results2$token_text[[1]][1])
unlist(results2$token_text[[1]])
glimpse(results2)

tst <-
  results2 %>% 
  unnest(token_text) %>% 
  unnest(token_text) %>% 
  mutate(token_text = tolower(token_text)) %>% 
  mutate(
    token_text = ifelse(token_text == "stocks", "stock", token_text),
    token_text = ifelse(token_text == "level", "levels", token_text),
    token_text = ifelse(token_text == "catch", "catches", token_text)
    
  ) %>% 
  filter(is.na(as.numeric(token_text))) %>% 
  filter(!(token_text %in% c("a","an", "the", "as","at", "and", "are", "not", "for", "any", "term",
                             "these","this","that","from", "will","with","which", "same", "shall","may",
                             "should","year", "below", "above", "considered","given", "cases")) ) %>% 
  filter(nchar(token_text) > 2) %>%
  
  # select only the top 10 words for each year
  group_by(year, token_text) %>% 
  summarize(n=n()) %>% 
  group_by(year) %>% 
  top_n(n = 10, wt = n) %>% 
  
  # select only words that are mentioned at least in 5 years
  group_by(token_text) %>% 
  filter(n() > 5) %>% 
  
  arrange(year, desc(n)) 

tst %>% 
  ggplot(aes(x=token_text, y=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~year, scales="free_x", nrow=1)


dat <- data.frame(x = factor("A"), y = 1)
mutate(dat,levels(x) = "B")
mutate(dat, x = fct_recode(x, "B" = "A"))
levels(dat$x)



