# -----------------------------------------------------------------------------------------
# temporary coding: merging of excel and qcs files
# 05/01/2018
# -----------------------------------------------------------------------------------------

rm(list=ls())

# Libraries
library(rmarkdown)

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours
library(lubridate)
library(docxtractr) # read word documents
library(icesSD)    # ICES Stock database
library(icesSAG)   # ICES Stock Assessment Graphs

# Load utils code
source("../mptools/r/my_utils.r")

# set dropboxdir
dropboxdir <- paste(get_dropbox(), "/ICES assessment database", sep="")

# Generate iStock, iRename and iStockkey
t <- 
  readxl::read_excel(path= paste(dropboxdir, "/data/iStock.xlsx", sep=""), sheet = "istock",
                     col_names = TRUE, col_types = "text", trim_ws   = FALSE) %>%
  lowcase() %>% 
  mutate_at(c("assessmentmodel","assessmenttype","advicetype",
              "expertgroup","useofdiscardsinadvice","pabufferapplied", "assessmentstatus"),
            funs(tolower)) %>% 
  mutate_at(c("fmax","f01","fmed","f35spr","flim","fpa","fmsy","blim","bpa","msybtrigger"), 
            funs(as.numeric)) %>% 
  mutate_at(c("stockkey", "assessmentyear", "firstyearofdata", "ncpueseries","nsurveyseries"),
            funs(as.integer)) %>% 
  
  mutate(assessmenttype   = ifelse(grepl("explo",assessmentmodel), "exploratory",assessmenttype),
         assessmenttype   = ifelse(grepl("trends",assessmentmodel), "trends",assessmenttype) ) %>% 
  
  mutate(assessmentmodel  = ifelse(grepl("(xsa"   , assessmentmodel, fixed=TRUE), "xsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("sxsa"   , assessmentmodel, fixed=TRUE), "sxsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flxsa" , assessmentmodel, fixed=TRUE), "xsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(ica"   , assessmentmodel, fixed=TRUE), "ica"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flica" , assessmentmodel, fixed=TRUE), "ica"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(sam"   , assessmentmodel, fixed=TRUE), "sam"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(flsam" , assessmentmodel, fixed=TRUE), "sam"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(tsa"   , assessmentmodel, fixed=TRUE), "tsa"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(adapt" , assessmentmodel, fixed=TRUE), "adapt" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(ss3"   , assessmentmodel, fixed=TRUE), "ss3"   , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(stock synthesis 3", assessmentmodel, fixed=TRUE), 
                                   "ss3", assessmentmodel),
         assessmentmodel  = ifelse(grepl("(gadget" , assessmentmodel, fixed=TRUE), "gadget", assessmentmodel),
         assessmentmodel  = ifelse(grepl("(asap"  , assessmentmodel, fixed=TRUE), "asap"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(amish"  , assessmentmodel, fixed=TRUE), "amish" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(aspic"  , assessmentmodel, fixed=TRUE), "aspic" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(mycc"   , assessmentmodel, fixed=TRUE), "mycc"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("multi-year catch curve", 
                                         assessmentmodel, fixed=TRUE), "mycc"  , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(aspic"  , assessmentmodel, fixed=TRUE), "aspic" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("aarts"  , assessmentmodel, fixed=TRUE), 
                                   "aarts_poos" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(cbbm"  , assessmentmodel, fixed=TRUE), "cbbm" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(scaa"  , assessmentmodel, fixed=TRUE), "scaa" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(sms"  , assessmentmodel, fixed=TRUE), "sms" , assessmentmodel),
         assessmentmodel  = ifelse(grepl("(tasacs"  , assessmentmodel, fixed=TRUE), "tasacs" , assessmentmodel),
         # NEED TO FINALIZE THIS LIST !!
         
         assessmentdate   = as.Date(as.numeric(assessmentdate), origin="1899-12-30"),
         advicereleasedate= as.Date(as.numeric(advicereleasedate), origin="1899-12-30"),
         modifieddate     = as.Date(as.numeric(modifieddate), origin="1899-12-30")
         
         # assessmenttype2 = ifelse(assessmenttype %in% c("exploratory","trends","trends only"), 
         #                          "trends",assessmenttype)
  ) %>% 
  arrange(stockkey, assessmentyear, stockkeylabel) 

iStock <-
  t %>% 
  select(stockkey, stockkeylabel, 
         assessmentyear, assessmenttype, assessmentmodel, assessmentdate, assessmentcomment,
         stockarea, expertgroup, advicedraftinggroup, 
         firstyearofdata, agerange, ncpueseries, nsurveyseries, 
         datacategory, yearoflastassessment, assessmentfrequency, yearofnextassessment, 
         advicereleasedate, advicetype, useofdiscardsinadvice, pabufferapplied, 
         assessmentstatus, assessmentscale, sectionnumber, assessmentkey, modifieddate,
         fmax, f01, fmed, f35spr, flim, fpa, fmsy, blim, bpa, msybtrigger, checkeddate, checkedby) 

# iStockkey
iStockkey <-
  t %>% 
  group_by(stockkey) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(stockkey, stockkeylabelnew, stockkeylabelold)


# iRename
iRename <-
  t %>% 
  # group_by(stockkey, stockkeylabel, stockkeydescription, speciesfaocode=fao) %>% 
  group_by(stockkey, stockkeylabel) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(stockkeylabel, stockkey, stockkeydescription, speciesfaocode=fao) 

# iSpecies
iSpecies <-
  readxl::read_excel(path= paste(dropboxdir, "/data/species_list.xlsx", sep=""), 
                     col_names = TRUE, col_types = "text", trim_ws   = TRUE) %>%
  mutate_at(c("speciescommonname","trophicguild","fisheriesguild","sizeguild"), 
            funs(tolower)) %>%
  group_by(speciesfaocode, speciesscientificname, speciescommonname) %>%
  arrange(speciesfaocode) 


 
  
### 2.3 Quality Control Sheets (QCS)

# make list of filenames
file.list <- c(list.files(path       = paste(dropboxdir, "/data/qcs-docs-docx/", sep=""), 
                          pattern    = "docx",
                          recursive  = TRUE, 
                          full.names = TRUE,   
                          ignore.case= TRUE) )

# read files from filename list
for (i in 1:length(file.list)) {
  
  # extract stock name
  stockkeylabel <- gsub(".qcs.docx","", file.list[i], fixed=TRUE)
  stockkeylabel <- gsub("((?:[^/]*/)*)(.*)","\\2", stockkeylabel)
  
  # print(paste0(i,stockkeylabel,file.list[i],sep=" - "))
  
  # set docx object
  docx <- read_docx(path=file.list[i])
  
  # read tables
  tmp <- docx_extract_all_tbls(docx, guess_header = FALSE, trim = TRUE)  
  
  # convert tables to data frames
  for (j in 1:length(tmp)) {
    
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

setf   <- t %>% filter(var=="f")   %>% 
  select(stockkeylabel, assessmentyear, year, fishingpressuredescription=var, f=value, fishingpressureunits=unit, fage=age)

setr   <- t %>% filter(var=="r")   %>% 
  select(stockkeylabel, assessmentyear, year, recruitmentdescription=var, r=value, recruitmentunits=unit, 
         recruitmentage=age, yeartype) %>% 
  # correct for yearclass reporting of recruitment
  mutate(year    = ifelse(yeartype=="yearclass", year + as.integer(recruitmentage), year)) %>% 
  arrange(stockkeylabel, assessmentyear, year)

setssb <- t %>% filter(var=="ssb") %>% 
  select(stockkeylabel, assessmentyear, year, stocksizedescription=var, ssb=value, stocksizeunits=unit) 
# setfb  <- t %>% filter(var=="fb")  %>% 
#   select(stockkeylabel, assessmentyear, year, stocksizedescription=var, fb=value, stocksizeunits=unit, stocksizeage=age)

qcsdata <-
  
  select(setr, 
         stockkeylabel, assessmentyear, year, recruitment=r, 
         recruitmentunits, recruitmentage, recruitmentdescription) %>% 
  
  full_join(select(setssb, 
                   stockkeylabel, assessmentyear, year, ssb, stocksizeunits, stocksizedescription), 
            by=c("stockkeylabel","assessmentyear","year")) %>% 
  
  full_join(select(setf, 
                   stockkeylabel, assessmentyear, year, 
                   f, fishingpressureunits, fage, fishingpressuredescription) , 
            by=c("stockkeylabel","assessmentyear","year")) %>% 
  
  arrange(stockkeylabel, assessmentyear, year) %>% 
  
  mutate(ssb            = ifelse(stocksizeunits == "thousand tonnes", ssb * 1000, ssb),
         stocksizeunits = ifelse(stocksizeunits == "thousand tonnes", "tonnes", stocksizeunits)) %>% 
  
  left_join(iRename, by=c("stockkeylabel")) %>% 
  
  mutate(assessmenttype  = "assess",
         source          = "qcs")  %>% 
  
  # add the assessment date
  left_join(select(iStock, 
                   stockkeylabel, assessmentyear, assessmentdate), 
            by = c("stockkeylabel","assessmentyear"))

# qcsdata %>%   filter(stockkeylabel == "cod-kat") %>% View()
# iRename %>%   filter(stockkeylabel == "cod-kat") %>% View()
# glimpse(iRename)
# glimpse(qcsdata)



### 2.4 Excel database of 'Standard Graphs'

exceldata <-
  readxl::read_excel(paste(dropboxdir, "/data/ICES Assessment Summary database.xlsx",sep=""),
                     sheet = "DATA", 
                     col_names = TRUE, 
                     col_types = "text", 
                     skip = 0) %>%
  lowcase %>%
  rename(stockkeylabel = fishstock) %>% 
  mutate(stockkeylabel = tolower(stockkeylabel),
         assessmentdate = as.Date(as.numeric(assessmentdate), origin="1899-12-30")) %>%
  mutate_at(vars("year","assessmentyear"), funs(as.integer)) %>% 
  mutate_at(vars("lowrecruitment", "recruitment","highrecruitment",
                 "lowssb","ssb","highssb",
                 "lowf", "f","highf",
                 "landings","catches","discards","ibc"), 
            funs(as.numeric)) %>%
  mutate_at(vars("stocksizedescription","stocksizeunits",
                 "fishingpressuredescription", "fishingpressureunits"), 
            funs(tolower)) %>% 
  # mutate(assessmenttype2 = ifelse(assessmentyear >= 2011 & assessmenttype2 == "assess", 
  #                                "update",assessmenttype2)) %>% 
  ungroup() %>% 
  
  # dealing with old and new stocknames
  left_join(iRename, by = c("stockkeylabel")) %>% 
  filter(year <= assessmentyear) %>% 
  
  arrange(stockkeylabel, assessmentyear, year)


# -----------------------------------------------------------------------------------------
# Combining
# -----------------------------------------------------------------------------------------

# Extract list of fishstock and assessment years from old database
exceldata_unique <-
  exceldata %>% 
  group_by(stockkey, assessmentyear, stockkeylabel, assessmenttype) %>% 
  filter(row_number()==1) %>% 
  select(stockkey, assessmentyear, stockkeylabel, assessmenttype) %>% 
  ungroup()

# Extract list of fishstock and assessment years from old QCS database
qcsdata_unique <-
  qcsdata %>% 
  group_by(stockkey, assessmentyear, stockkeylabel, assessmenttype) %>% 
  filter(row_number()==1) %>% 
  select(stockkey, assessmentyear, stockkeylabel, assessmenttype) %>% 
  ungroup()

# calculate the data from qcsdata to add
toadd <-
  qcsdata_unique %>% 
  
  # first select only unique values that are not already in the excel unique values
  anti_join(exceldata_unique, by=c("stockkey","assessmentyear","stockkeylabel","assessmenttype")) %>% 
  
  # then merge with the qcsdata
  left_join(qcsdata, by=c("stockkey","assessmentyear","stockkeylabel","assessmenttype"))

# -----------------------------------------------------------------------------------------
# Generate iAssess and do cleaning up
# -----------------------------------------------------------------------------------------

iAssess <- 
  bind_rows(exceldata, toadd) %>%  

  # corrections to stock size descriptions
  mutate(
    stocksizedescription = gsub("stock size: "                 ,""                   , stocksizedescription),
    stocksizedescription = gsub("indices"                      ,"index"              , stocksizedescription),
    stocksizedescription = gsub("indicator"                    ,"index"              , stocksizedescription),
    stocksizedescription = gsub("^biomass$"                    ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^biomass index$"              ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^evhoe biomass index$"        ,"total biomass index", stocksizedescription),
    stocksizedescription = gsub("^stock size index: abundance$","abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^abundance$"                  ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^density$"                    ,"density index"      , stocksizedescription),
    stocksizedescription = gsub("^tsb$"                        ,"total biomass"      , stocksizedescription),
    stocksizedescription = gsub("^total abundance index$"      ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^index$"                      ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^stock abundance$"            ,"abundance index"    , stocksizedescription),
    stocksizedescription = gsub("^density index$"              ,"abundance index"    , stocksizedescription),
    
    stocksizedescription = gsub("stock size index: biomass \\(ages 1-8\\)"   ,"total biomass index"      , stocksizedescription),
    stocksizedescription = gsub("stock size index: german survey"            ,"total biomass index"      , stocksizedescription),
    stocksizedescription = gsub("stock size index: smoothed greenland index" ,"total biomass index"      , stocksizedescription),
    
    stocksizedescription = ifelse(grepl("tv",stocksizedescription), "abundance index", stocksizedescription),
    stocksizedescription = ifelse(grepl("ssb & b",stocksizedescription) , "ssb", stocksizedescription),
    stocksizedescription = ifelse(grepl("total biomass/bmsy",stocksizedescription) , "b/bmsy", stocksizedescription),
    stocksizedescription = ifelse(stocksizedescription == "stock size" & stocksizeunits == "tonnes", "ssb", stocksizedescription),
    stocksizedescription = ifelse(stocksizedescription == "stock size" & grepl("kg/",stocksizeunits), "total biomass index", stocksizedescription),
    stocksizedescription = ifelse(grepl("relative", stocksizeunits, fixed=TRUE) & is.na(stocksizedescription), "total biomass index", stocksizedescription)
  ) %>% 
  
  # corrections to stock units
  mutate(
    stocksizeunits = gsub("stock size: "                 ,""                   , stocksizeunits),
    stocksizeunits = gsub(" ", "", stocksizeunits),
    stocksizeunits = gsub("density(burrows/m2)", "burrows/m2", stocksizeunits, fixed=TRUE),
    stocksizeunits = gsub("cpue(kg/1000hooks)", "kg/1000hooks", stocksizeunits, fixed=TRUE),
    stocksizeunits = gsub("^abundance$", "millions", stocksizeunits),
    stocksizeunits = gsub("na(ratio)", "relative", stocksizeunits, fixed=TRUE),
    stocksizeunits = ifelse(grepl("kg/h", stocksizeunits, fixed=TRUE), "kg/hour", stocksizeunits),
    stocksizeunits = ifelse(grepl("n/h", stocksizeunits, fixed=TRUE), "n/hour", stocksizeunits)
  ) %>% 
  
  # corrections to fishing pressure descriptions
  mutate(
    fishingpressuredescription = gsub("fishing pressure: ",""  , fishingpressuredescription),
    fishingpressuredescription = gsub(" ",""  , fishingpressuredescription),
    fishingpressuredescription = gsub("f&hr","f"  , fishingpressuredescription, fixed=TRUE),
    fishingpressuredescription = gsub("fishingpressure","f"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("finwinterrings","f"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("weightedf","f"  , fishingpressuredescription), 
    
    fishingpressuredescription = gsub("harvestrate","hr"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("relativehr","hr/index"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("hrindex","hr/index"  , fishingpressuredescription), 
    fishingpressuredescription = gsub("relativeexploitationrate","hr/index"  , fishingpressuredescription), 
    
    fishingpressuredescription = ifelse(grepl("ages",fishingpressuredescription), "f", fishingpressuredescription),
    fishingpressuredescription = ifelse(grepl("null",fishingpressuredescription), NA, fishingpressuredescription),
    
    fishingpressureunits       = ifelse(grepl("relative",fishingpressuredescription) & is.na(fishingpressureunits), "relative", fishingpressureunits ),
    fishingpressuredescription = ifelse(grepl("relative",fishingpressuredescription) , "fproxy", fishingpressuredescription )
  ) %>% 
  
  
  # corrections to fishing pressure units
  mutate(
    fishingpressureunits = gsub(" ",""  , fishingpressureunits),
    fishingpressureunits = gsub("peryear","year-1"  , fishingpressureunits),
    fishingpressureunits = gsub("%","percent"  , fishingpressureunits),
    fishingpressureunits = gsub("^f$","year-1"  , fishingpressureunits),
    fishingpressureunits = gsub("^catch/biomass$","relative"  , fishingpressureunits),
    
    fishingpressureunits = ifelse(grepl("cm",fishingpressureunits), "year-1",fishingpressureunits) ,
    fishingpressureunits = ifelse(grepl("null",fishingpressureunits), NA,fishingpressureunits) ,
    fishingpressureunits = ifelse(grepl("ratio",fishingpressureunits), "relative",fishingpressureunits) 
  ) %>% 
  
  # correction due to missing units
  mutate(
    stocksizeunits       = ifelse(stocksizedescription=="b/bmsy" & is.na(stocksizeunits),"relative",stocksizeunits),  
    fishingpressureunits = ifelse(fishingpressuredescription=="f/fmsy" & is.na(fishingpressureunits),"relative",fishingpressureunits)
  ) %>% 
  
  # corrections to the assignments of specific stocks and years
  mutate(
    stocksizedescription       = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"b/bmsy"  ,stocksizedescription),
    stocksizeunits             = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"relative",stocksizeunits),
    fishingpressuredescription = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"f/fmsy"  ,fishingpressuredescription),
    fishingpressureunits       = ifelse(stockkeylabel=="anb-8c9a" & assessmentyear==2013,"relative",fishingpressureunits)
  ) %>% 
  
  # remove double series (e.g. mac-nea 2013 is twice in the sag download)
  group_by(stockkeylabel, assessmentyear, year, assessmenttype) %>% 
  filter(row_number() == 1) %>% 
  
  # add old and new names
  left_join(iStockkey, by="stockkey") %>% 
  
  # add species info
  left_join(iSpecies, by="speciesfaocode") %>% 
  
  # add stock info
  # left_join(iStock, by=c("stockkey","stockkeylabel","assessmentyear","assessmenttype","assessmentdate")) %>% 
  
  # change types
  mutate_at(c("officiallandings","unallocatedremovals"), funs(as.numeric)) %>% 
  
  # remove descriptions for empty variables
  mutate(recruitmentdescription = ifelse(is.na(recruitment), NA, recruitmentdescription),
         unitofrecruitment      = ifelse(is.na(recruitment), NA, unitofrecruitment),
         
         stocksizedescription   = ifelse(is.na(ssb), NA, stocksizedescription),
         stocksizeunits         = ifelse(is.na(ssb), NA, stocksizeunits),
         
         catcheslandingsunits   = ifelse(is.na(sum(landings, officiallandings, catches, 
                                                   discards, ibc, unallocatedremovals, na.rm=TRUE)), NA, catcheslandingsunits),
         
         fishingpressuredescription = ifelse(is.na(f), NA, fishingpressuredescription),
         fishingpressureunits       = ifelse(is.na(f), NA, fishingpressureunits) ) %>% 
         
  # convert to lowercase
  ungroup() %>% 
  as.data.frame()

write.csv(iAssess, file="qcsandexcelcombined.csv", na="", row.names=FALSE)

# generate set of unique values
iAssess_unique <-
  iAssess %>% 
  group_by(stockkey, assessmentyear, stockkeylabel, assessmenttype, assessmentdate,
           recruitmentunits, stocksizeunits, fishingpressureunits) %>% 
  filter(row_number()==1) %>% 
  select  (stockkey, assessmentyear, stockkeylabel, assessmenttype, assessmentdate,
           recruitmentunits, stocksizeunits, fishingpressureunits) %>% 
  ungroup()


x <-
  iAssess %>% 
  
  # add old and new names
  # left_join(iStockkey, by="stockkey") %>% 
  
  filter(assessmentyear >= 1980) %>% 
  filter(!substr(stockkeylabel,1,2) %in% c("ne","rj","ra") ) %>% 
  filter(!is.na(stockkey)) %>% 
  group_by(stockkeylabelold, stockkeylabelnew, assessmentyear, assessmenttype, source) %>% 
  filter(row_number() ==1) %>% 
  select(-year) %>% 
  ungroup() %>% 
  arrange(stockkeylabelold) %>% 
  mutate(id = group_indices(., stockkeylabelold)) %>% 
  data.frame() %>% 
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  mutate(stockkeylabelold = factor(stockkeylabelold), 
         stockkeylabelold = factor(stockkeylabelold, levels = rev(levels(stockkeylabelold))),
         source           = factor(source, levels=c("qcs","excel","sag")),
         assessmenttype  = factor(assessmenttype),
         assessmentdate    = ifelse(is.na(assessmentdate), 
                                    make_date(year = assessmentyear, month = 6L, day = 30L),
                                    assessmentdate),
         assessmentdate    = as.Date(assessmentdate, origin="1970-01-01"),
         col              = ifelse(id <=  90            , 1, NA),
         col              = ifelse(id  >  90 & id <= 180, 2, col),
         col              = ifelse(id  > 180            , 3, col)) 

# define colour scale
myColors        <- brewer.pal(length(levels(x$source)),"Set1")
names(myColors) <- levels(x$source)

# define headers for columns
y <-
  x %>% 
  group_by(col) %>% 
  filter(row_number()==1| row_number() == n()) %>% 
  ungroup() %>% 
  mutate(id = group_indices(., col)) %>% 
  select(col, id, stockkeylabelold) %>% 
  group_by(col) %>% 
  summarise(code = paste(stockkeylabelold, collapse=" : "))


# plot by stock and data source
x %>% 
  left_join(y, by="col") %>% 
  ggplot(aes(x=assessmentdate, y=stockkeylabelold)) +
  theme_publication() +
  theme(panel.spacing = unit(1, "lines"),
        text          = element_text(size=8),
        legend.title  = element_blank(),
        axis.text.x   = element_text(angle = 90, vjust = 1)) +
  geom_point(aes(colour = source)) +
  scale_colour_manual(name = "source", values = myColors, na.value="lightgray") +
  scale_y_discrete(position="right") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(x = "assessmentyear", y = NULL ) +
  facet_wrap(~code, scales="free_y", shrink=TRUE, ncol=3)
