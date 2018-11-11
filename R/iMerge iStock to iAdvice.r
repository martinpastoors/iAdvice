# -----------------------------------------------------------------------------------------------
# iMerge iStock to iAdviced.r
#
# Getting rid of the iStock database by merging unique records into iAdvice 
# 
# 07/11/2018 No longer need iStock
# -----------------------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse) # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)  # reshaping data; e.g. dcast
library(pander)    # for print tables
library(readxl)    # read excel files

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
assessdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")
advicedir  <- paste(get_dropbox(), "/ICES Advice database", sep="")

# -----------------------------------------------------------------------------------------
# load the Advice database
# -----------------------------------------------------------------------------------------

iAdvice <-
  readxl::read_excel(
    path= paste(advicedir, "/Excel/ICES Scientific Advice database 20181108.xlsx", sep=""), 
    sheet     = "DATA",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() %>% 
  
  # split up advisedlandings
  
  # mutate(advisedlandings = gsub("\\[|\\]","",advisedlandings)) %>% 
  # separate(advisedlandings, into=c("advisedlandingsmin","advisedlandingsmax"), sep="-") %>% 
  # mutate(advisedlandingsmax = ifelse(is.na(advisedlandingsmax), advisedlandingsmin, advisedlandingsmax)) %>% 
  # mutate(advisedlandingsmin = ifelse(advisedlandingsmin == advisedlandingsmax, NA, advisedlandingsmin)) %>% 
  
  # split up advisedcatch
  
  # mutate(advisedcatch = gsub("\\[|\\]","",advisedcatch)) %>% 
  # separate(advisedcatch, into=c("advisedcatchmin","advisedcatchmax"), sep="-") %>% 
  # mutate(advisedcatchmax = ifelse(is.na(advisedcatchmax), advisedcatchmin, advisedcatchmax)) %>% 
  # mutate(advisedcatchmin = ifelse(advisedcatchmin == advisedcatchmax, NA, advisedcatchmin)) %>% 
  
  mutate_at(c("advisedlandingsmin", "advisedcatchmin", 
              "advisedlandingsmax", "advisedcatchmax",
              "tal", "tac",
              "officiallandings", "landings", "ibc", "discards", "catches",
              "fsqay", "ssbay", "fadvmax",
              "fmax", "f01", "fmed", "f35spr", "flim", "fpa", "fmsy",
              "blim", "bpa", "msybtrigger",
              "m1", "m5"),
            funs(as.numeric)) %>%
  mutate_at(c("tacyear", "assessmentyear", "stockkey", "firstyearofdata", "ncpueseries","nsurveyseries",
              "assessmentdate","checkeddate"),
            funs(as.integer)) %>% 
  mutate_at(c("assessmenttype", "assessmentmodel"), 
            funs(tolower)) %>% 
  arrange(sort1)  
  # rowid_to_column()

save(iAdvice, file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))
# glimpse(iAdvice)

# -----------------------------------------------------------------------------------------
# load the iStock database
# -----------------------------------------------------------------------------------------

iStock <-
  readxl::read_excel(
    path= paste(assessdir, "/excel/istock 20181107.xlsx", sep=""), 
    sheet = "istock",
    col_names = TRUE, 
    col_types = "text", 
    trim_ws   = FALSE) %>%
  lowcase() %>% 
  mutate_at(c("fmax", "f01", "fmed", "f35spr", 
              "fpa", "flim", "fmsy", 
              "bpa", "blim", "msybtrigger", "mbal"), 
            funs(as.numeric)) %>% 
  mutate_at(c("stockkey", "assessmentyear","firstyearofdata", "ncpueseries", "nsurveyseries",
              "assessmentdate","checkeddate"), 
            funs(as.integer)) %>% 
  mutate_at(c("assessmenttype", "assessmentmodel"), 
            funs(tolower)) %>% 
  mutate(
    assessmenttype = ifelse(assessmenttype == "update", "assess", assessmenttype)
  )


# -----------------------------------------------------------------------------------------
# Combine in 3 steps
# -----------------------------------------------------------------------------------------

# 1. Add data from columns that are shared

t1 <-
  iAdvice %>% 
  left_join(select(iStock, intersect(names(iAdvice), names(iStock))),
            by = c("stockkey", "stockkeylabel","stockkeylabelnew","stockkeylabelold",
                   "assessmentyear", "assessmentdate","assessmenttype")) %>% 
  mutate(
    stockarea         = ifelse(!is.na(stockarea.x), stockarea.x, stockarea.y),
    wg                = ifelse(!is.na(wg.x), wg.x, wg.y),
    fmax              = ifelse(!is.na(fmax.x)     , fmax.x     , fmax.y     ),
    f01               = ifelse(!is.na(f01.x)      , f01.x      , f01.y      ),
    fmed              = ifelse(!is.na(fmed.x), fmed.x, fmed.y),
    f35spr            = ifelse(!is.na(f35spr.x), f35spr.x, f35spr.y),
    flim              = ifelse(!is.na(flim.x), flim.x, flim.y),
    fpa               = ifelse(!is.na(fpa.x), fpa.x, fpa.y),
    fmsy              = ifelse(!is.na(fmsy.x), fmsy.x, fmsy.y),
    blim              = ifelse(!is.na(blim.x), blim.x, blim.y),
    bpa               = ifelse(!is.na(bpa.x), bpa.x, bpa.y),
    msybtrigger       = ifelse(!is.na(msybtrigger.x), msybtrigger.x, msybtrigger.y),
    mbal              = ifelse(!is.na(mbal.x), mbal.x, mbal.y),
    assessmentmodel   = ifelse(!is.na(assessmentmodel.y), assessmentmodel.y, assessmentmodel.x),
    firstyearofdata   = ifelse(!is.na(firstyearofdata.x), firstyearofdata.x, firstyearofdata.y),
    ncpueseries       = ifelse(!is.na(ncpueseries.x), ncpueseries.x, ncpueseries.y),
    nsurveyseries     = ifelse(!is.na(nsurveyseries.x), nsurveyseries.x, nsurveyseries.y),
    assessmentcomment = ifelse(!is.na(assessmentcomment.x), assessmentcomment.x, assessmentcomment.y),
    dlscategory       = ifelse(!is.na(dlscategory.x), dlscategory.x, dlscategory.y),
    checkedby         = ifelse(!is.na(checkedby.x), checkedby.x, checkedby.y),
    checkeddate       = ifelse(!is.na(checkeddate.x), checkeddate.x, checkeddate.y)
  ) %>% 
  select(names(iAdvice))

# Checks



# iAdvice %>% 
#   left_join(select(iStock, intersect(names(iAdvice), names(iStock))),
#             by = c("stockkey", "stockkeylabel","stockkeylabelnew","stockkeylabelold",
#                    "assessmentyear", "assessmentdate","assessmenttype")) %>%
#   filter(firstyearofdata.x != firstyearofdata.y) %>% 
#   select(stockkey, stockkeylabel, assessmentyear, assessmentdate, assessmenttype, 
#          firstyearofdata.x, firstyearofdata.y) %>% 
#   View()


# 2. Add new columns

t2 <-
  t1 %>% 
  left_join(select(iStock, 
                   stockkey, stockkeylabel, assessmentyear, assessmentdate, assessmenttype, 
                   agerange, fage, assessmentstatus, assessmentscale),
            by = c("stockkey", "stockkeylabel", "assessmentyear", "assessmentdate","assessmenttype")) 

# 3. Add new rows

# Generate unique records
iStock_ <-
  iStock %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, assessmentdate, assessmenttype) 

iAdvice_ <-
  iAdvice %>% 
  distinct(stockkey, stockkeylabel, assessmentyear, assessmentdate, assessmenttype)

# Generate records to add
toadd <-
  setdiff((iStock_), (iAdvice_)) %>% 
  left_join(iStock, by=c("stockkey","stockkeylabel","assessmentyear", "assessmentdate", "assessmenttype")) %>% 
  mutate(speciesfaocode = substr(stockkeylabel,1,3)) %>% 
  left_join(iSpecies, by=c("speciesfaocode")) %>% 
  select(one_of(names(t2)))

# Create final dataset
t3 <-
  bind_rows(t2, toadd)


write.csv(t3, file="t3.csv", row.names=FALSE)


# as.data.frame(count_not_na(iAdvice), stringsasfactors=FALSE) %>%
#   rownames_to_column() %>%
#   left_join(as.data.frame(count_not_na(t2), stringsasfactors=FALSE) %>% rownames_to_column(),
#             by=c("rowname"))

