# --------------------------------------------------------------------------------------------
# qcsdata.r
# 
# Read Quality Control Sheets word tables
#
# 05/10/2017 coded up the reading in of all data files; datafiles converted to docx format; all superscripts removed prior to reading
# 05/10/2017 converted to iAssess github
# 03/11/2017 renamed file
# --------------------------------------------------------------------------------------------

library(docxtractr)
library(tidyverse)
library(stringr)

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# load rename datafiles
load(file=paste(dropboxdir, "/rdata/iStockkey.RData", sep=""))
load(file=paste(dropboxdir, "/rdata/iRename.RData", sep=""))

# make list of filenames
file.list <- c(list.files(path       = "data/qcs-docs-docx/", 
                          pattern    = "docx",
                          recursive  = TRUE, 
                          full.names = TRUE,   
                          ignore.case= TRUE) )

# read files from filename list
# i <- 1
# j <- 1
for (i in 1:length(file.list)) {
  
  # extract stock name
  stockkeylabel <- gsub(".qcs.docx","", file.list[i], fixed=TRUE)
  stockkeylabel <- gsub("((?:[^/]*/)*)(.*)","\\2", stockkeylabel)
  
  print(paste0(i,stockkeylabel,file.list[i],sep=" - "))
  
  # set docx object
  docx <- read_docx(path=file.list[i])
  # docx_tbl_count(docx)
  # docx_describe_tbls(docx)
  
  # read tables
  tmp <- docx_extract_all_tbls(docx, guess_header = FALSE, trim = TRUE)  
  
  # convert tables to data frames
  for (j in 1:length(tmp)) {
    
    # print(j)
    
    t       <- tmp[j] %>%  as.data.frame()
    variable <- tolower(t[1,1])
    yeartype <- tolower(t[2,2])
    nc      <- ncol(t)
    nr      <- nrow(t)
    head    <- t[3,2:nc]
    rows    <- t[4:nr,1]  %>% data.frame()
    names(rows) <- "assessmentyear"
    t <- 
      t[4:nr,2:nc] %>% 
      setNames(head) %>% 
      cbind(rows) %>% 
      gather(key=year, value=value, 1:(nc-1)) %>% 
      mutate(stockkeylabel          = tolower(stockkeylabel), 
             variable       = variable,
             yeartype       = yeartype, 
             value          = as.numeric(gsub("\\s+","",value)),
             assessmentyear = substr(as.character(assessmentyear), 1,4),
             assessmentyear = as.integer(assessmentyear)) %>% 
      filter(!is.na(value), value != "")
    
    if (j == 1) { data <- t
    } else      { data <- rbind(data,t) }
    
  } # end of j for loop
  
  if (i == 1) { qcsdata <- data
  } else      { qcsdata <- rbind(qcsdata,data) }
  
} #end of i for loop  

# check and convert
t <-
  qcsdata %>% 
  mutate(
    year    = as.integer(year),
    stockkeylabel   = gsub(".qcs.docx","",stockkeylabel), 
    stockkeylabel   = gsub("anb-89","anb-8c9a", stockkeylabel),
    stockkeylabel   = gsub("anp-89","anp-8c9a", stockkeylabel),
    stockkeylabel   = gsub("cod-coast","cod-coas", stockkeylabel),
    stockkeylabel   = gsub("had-icel","had-iceg", stockkeylabel),
    stockkeylabel   = gsub("had-irisde3","had-iris", stockkeylabel),
    stockkeylabel   = gsub("her-2532excgor","her-2532-gor", stockkeylabel),
    stockkeylabel   = gsub("mac-wes","mac-nea", stockkeylabel),
    
    yeartype= gsub("\\s","",yeartype), 
    
    variable = gsub("  "," ", variable), 
    var     = ifelse(grepl("average f"  , variable), "f"  , NA),
    var     = ifelse(grepl("spawning"   , variable), "ssb", var),
    var     = ifelse(grepl("recruitment", variable), "r"  , var),
    var     = ifelse(grepl("fishable"   , variable), "fb" , var),
    
    unit    = ifelse(var=="f"                                  , "year-1"                            , NA),
    
    # extract the bits between brackets
    unit    = ifelse(var=="ssb" & grepl("\\(.+\\)$"  , variable), gsub(".+\\((.+)\\)$","\\1", variable), unit),
    unit    = ifelse(var=="fb"  & grepl("\\(.+\\)$"  , variable), gsub(".+\\((.+)\\)$","\\1", variable), unit),
    unit    = ifelse(             grepl("unit: (.+)$", variable), gsub(".+unit: (.+)$","\\1", variable), unit),
    unit    = ifelse(var=="r" & stockkeylabel=="cod-2532"              , "thousands"                         , unit),  
    unit    = ifelse(var=="r" & stockkeylabel=="cod-iceg"              , "millions"                          , unit),  
    unit    = ifelse(var=="r" & stockkeylabel=="ple-celt"              , "thousands"                         , unit),  
    unit    = ifelse(var=="r" & stockkeylabel=="hke-soth"              , "check"                             , unit),
    
    unit    = gsub("[^[:alnum:]]", "", unit), 
    unit    = gsub("^000s$|^000$","thousands", unit),
    unit    = gsub("^000 000s$|000000s","millions" , unit),
    unit    = gsub("^000t$|^000stonnes$|^000tonnes$","thousand tonnes" , unit),
    unit    = gsub("000million$","billions" , unit),
    unit    = gsub("^t$", "tonnes",unit),
    unit    = ifelse(unit == "year1","year-1",unit),
    
    age     = ifelse(var=="f" & grepl("\\(.+\\)$", variable), gsub(".+\\((.+)\\)$","\\1", variable), NA),
    age     = gsub(" +", "", age), 
    age     = gsub(",,u", "", age), 
    age     = gsub(",u", "", age), 
    age     = gsub(",w", "", age), 
    age     = ifelse(var=="f" & stockkeylabel=="anb-78ab" & assessmentyear <= 2000, "4-8" , age),
    age     = ifelse(var=="f" & stockkeylabel=="anb-78ab" & assessmentyear  > 2000, "6-10", age),
    age     = ifelse(var=="f" & stockkeylabel=="anp-78ab" & assessmentyear <= 2000, "3-7", age),
    age     = ifelse(var=="f" & stockkeylabel=="anp-78ab" & assessmentyear  > 2000, "3-8", age),
    age     = ifelse(var=="f" & stockkeylabel=="cod-347d" & assessmentyear <= 2002, "2-8", age),
    age     = ifelse(var=="f" & stockkeylabel=="cod-347d" & assessmentyear  > 2002, "2-4", age),
    age     = ifelse(var=="f" & stockkeylabel=="her-irls"                          ,"2-7" , age ),
    
    # extract recruitment age information between brackets
    age     = ifelse(var=="r" & grepl(".+\\(age (.)\\).+", variable), gsub(".+\\(age (.)\\).+","\\1", variable), age),
    age     = ifelse(var=="r" & grepl(".+\\(age (.) \\).+", variable), gsub(".+\\(age (.) \\).+","\\1", variable), age),
    age     = ifelse(var=="r" & stockkeylabel=="anb-78ab" & assessmentyear <= 2000, "1", age),
    age     = ifelse(var=="r" & stockkeylabel=="anb-78ab" & assessmentyear  > 2000, "2", age),
    age     = ifelse(var=="r" & stockkeylabel=="anp-78ab" & assessmentyear <= 2000, "0", age),
    age     = ifelse(var=="r" & stockkeylabel=="anp-78ab" & assessmentyear  > 2000, "1", age),
    age     = ifelse(var=="r" & stockkeylabel=="cod-2532", "2"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stockkeylabel=="her-irls", "1"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stockkeylabel=="mgw-78"  , "1"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stockkeylabel=="ple-celt", "1"    , age),  # looked up in ACFM report
    age     = ifelse(var=="r" & stockkeylabel=="whg-47d" , "check", age)
  )

# unique(t$stockkeylabel)
# unique(t$unit)
# unique(t$age)
# 
# filter(t, var=="r"&is.na(unit)) %>% group_by(stockkeylabel, assessmentyear) %>%  filter(row_number()==1) %>%  View()
# filter(t, var=="r"&is.na(age)) %>% group_by(stockkeylabel, assessmentyear) %>%  filter(row_number()==1) %>%  View()
# filter(t, stockkeylabel=="her-3a22") %>% View()
# filter(t, grepl("1990", age)) %>%  View()
# filter(t, grepl("\\(.+\\)$", var)) %>% distinct(var) %>% View()

setf   <- t %>% filter(var=="f")   %>% 
  select(stockkeylabel, assessmentyear, year, fishingpressuredescription=var, f=value, fishingpressureunits=unit, fage=age)

setr   <- t %>% filter(var=="r")   %>% 
  select(stockkeylabel, assessmentyear, year, recruitmentdescription=var, r=value, recruitmentunits=unit, recruitmentage=age, yeartype) %>% 
  # correct for yearclass reporting of recruitment
  mutate(year    = ifelse(yeartype=="yearclass", year + as.integer(recruitmentage), year)) %>% 
  arrange(stockkeylabel, assessmentyear, year)

setssb <- t %>% filter(var=="ssb") %>% 
  select(stockkeylabel, assessmentyear, year, stocksizedescription=var, ssb=value, stocksizeunits=unit) 
# setfb  <- t %>% filter(var=="fb")  %>% 
#   select(stockkeylabel, assessmentyear, year, stocksizedescription=var, fb=value, stocksizeunits=unit, stocksizeage=age)

qcsdata <-
  select(setr, stockkeylabel, assessmentyear, year, recruitment=r, recruitmentunits, recruitmentage, recruitmentdescription) %>% 
  full_join(select(setssb, 
                   stockkeylabel, assessmentyear, year, ssb, stocksizeunits, stocksizedescription), 
            by=c("stockkeylabel","assessmentyear","year")) %>% 
  # full_join(select(setfb, stockkeylabel, assessmentyear, year, fb, stocksizeunits, stocksizedescription),
  #           by=c("stockkeylabel","assessmentyear","year")) %>%
  full_join(select(setf, stockkeylabel, assessmentyear, year, f, fishingpressureunits, fage, fishingpressuredescription) , 
            by=c("stockkeylabel","assessmentyear","year")) %>% 
  arrange(stockkeylabel, assessmentyear, year) %>% 
  mutate(ssb            = ifelse(stocksizeunits == "thousand tonnes", ssb * 1000, ssb),
         stocksizeunits = ifelse(stocksizeunits == "thousand tonnes", "tonnes", stocksizeunits)) %>% 
  left_join(iRename, by=c("stockkeylabel")) %>% 
  mutate(assessmenttype2 = "assess",
         source          = "qcs",
         datepublished   = as.Date(NA)) 
  
  # left_join(iStockkey, by=c("stockkey"))

# save dataset
save(qcsdata, file=paste(dropboxdir, "/rdata/qcsdata.RData",sep=""))
# load(file=paste(dropboxdir, "/rdata/qcsdata.RData",sep=""))

# count_not_na(qcsdata)
# filter(qcsdata, is.na(stockkey)) %>% View()

# plot overview
# qcsdata %>%
#   select(stockkeylabel, assessmentyear, year, r, ssb, f) %>% 
#   gather(key=variable, value=value, r:f) %>%
#   mutate(stockkeylabel = factor(stockkeylabel), 
#          stockkeylabel = factor(stockkeylabel, levels = rev(levels(stockkeylabel)))) %>% 
#            
#   ggplot(aes(x=assessmentyear, y=stockkeylabel)) +
#   theme_publication() +
#   theme(panel.spacing    = unit(0.1, "lines"),
#         text             = element_text(size=8),
#         legend.position  = "none",
#         legend.direction = "vertical",
#         legend.title     = element_blank()) +
#   geom_point(aes(colour = stockkeylabel)) +
#   scale_y_discrete(position="right") +
#   labs(y = "fishstock", x=" " ) +
#   facet_wrap(~variable)


  # see: https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
  # outlierKD <- function(dt, var) {
  #   var_name <- eval(substitute(var),eval(dt))
  #   na1 <- sum(is.na(var_name))
  #   m1 <- mean(var_name, na.rm = T)
  #   par(mfrow=c(2, 2), oma=c(0,0,3,0))
  #   boxplot(var_name, main="With outliers")
  #   hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  #   outlier <- boxplot.stats(var_name)$out
  #   mo <- mean(outlier)
  #   var_name <- ifelse(var_name %in% outlier, NA, var_name)
  #   boxplot(var_name, main="Without outliers")
  #   hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  #   title("Outlier Check", outer=TRUE)
  #   na2 <- sum(is.na(var_name))
  #   cat("Outliers identified:", na2 - na1, "n")
  #   cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  #   cat("Mean of the outliers:", round(mo, 2), "n")
  #   m2 <- mean(var_name, na.rm = T)
  #   cat("Mean without removing outliers:", round(m1, 2), "n")
  #   cat("Mean if we remove outliers:", round(m2, 2), "n")
  #   response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  #   if(response == "y" | response == "yes"){
  #     dt[as.character(substitute(var))] <- invisible(var_name)
  #     assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  #     cat("Outliers successfully removed", "n")
  #     return(invisible(dt))
  #   } else{
  #     cat("Nothing changed", "n")
  #     return(invisible(var_name))
  #   }
  # }
  # 
  # outlierKD(dt=qcsdata, var=f)
  
  
# qcsdata %>% 
#   mutate(ssb = ifelse(year > assessmentyear, NA, ssb),
#          r   = ifelse(year > assessmentyear, NA, r),
#          f   = ifelse(year >= assessmentyear, NA, f)) %>% 
#   
#   # filter(stockkeylabel=="cod-2224", assessmentyear %in% 1998:2000) %>% View()
#   
#   group_by(stockkeylabel, assessmentyear) %>% 
#   summarize(ssb = max(ssb, na.rm=TRUE),
#             f   = max(f, na.rm=TRUE),
#             r   = max(r, na.rm=TRUE)) %>% 
#   
#   
#   ggplot(aes(x=assessmentyear, y=f)) +
#   theme_publication() +
#   theme(panel.grid.major = element_line(colour = "gray"),
#         panel.spacing = unit(1, "lines"),
#         text          = element_text(size=8),
#         legend.title  = element_blank()) +
#   geom_bar(stat="identity") +
#   # geom_boxplot() +
#   facet_wrap(~stockkeylabel, scales="free_y")


# qcsdata %>% 
#   group_by(stockkeylabel, assessmentyear) %>% 
#   summarize(ssb = max(ssb, na.rm=TRUE)) %>% 
#   filter(stockkeylabel =="sol-nsea") %>% 
#   View()


# qcsdata %>% filter(grepl("mac",stockkeylabel)) %>% distinct(stockkeylabel)

