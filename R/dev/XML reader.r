library(xml2)
library(tidyverse)

data <- read_xml("D:/Dropbox/ICES Assessment database/data/getStockDownloadData.xml")
summary(data)
point <- 
  data %>% 
  xml_find_all("//point")

