# -----------------------------------------------------------------------------------------------
# Presentation for pelagic forum
#
# 03/11/2017 first coding
# 15/10/2018 updated for 2018 version of the talk
# 28/09/2019 updated for 2019 version of the talk
# 18/04/2023 using Officer package (while saving files as png and jpg)
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
# library(directlabels)  # for printing labels at end of geom lines
library(scales)
library(stringr)
library(readxl)
library(officer)
library(flextable)

# Load utils code
source("../prf/r/my utils.r")
source("R/iAssess functions.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

figuresdir <- file.path(dropboxdir, "figures")
dir.create(figuresdir, showWarnings = FALSE)

figuresdir <- file.path(dropboxdir, "figures", "pelagicforum2023")
dir.create(figuresdir, showWarnings = FALSE)
# file.remove(dir(figuresdir, full.names = TRUE))

# load the data
iSpecies <-
  loadRData(file=paste(dropboxdir,"/rdata/iSpecies.RData", sep="")) %>% 
  group_by(speciesfaocode, trophicguild, fisheriesguild) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  dplyr::select(speciesfaocode, trophicguild, fisheriesguild, sizeguild) 

iAssess <-
  loadRData(file=paste(dropboxdir,"/rdata/iAssess.RData", sep="")) %>% 
  distinct() %>% 
  left_join(iSpecies, by="speciesfaocode")

load(file=paste(dropboxdir,"/rdata/iAdvice.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iForecast.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iRename.RData", sep=""))
load(file=paste(dropboxdir,"/rdata/iStockkey.RData", sep=""))

lay               <- 2022
my.lastyear       <- 2022
my.firstyear      <- 1990

# top colours
# colourCount             <- nrow(top)+1
# getPalette              <- colorRampPalette(brewer.pal(12, "Paired"))
myColors                <- brewer.pal(12, "Paired")[1:9]
names(myColors)         <- c("other", "demersal", "pelagic",
                             "her", "whb", "mac","cap","san","spr")

# scales::show_col(brewer.pal(12, "Paired"))
# scales::show_col(myColors)
species <-
  iAdvice %>% 
  ungroup() %>% 
  distinct(speciesfaocode, speciescommonname)


# ---------------------------------------------------------------------------------------------
# start presentation
# ---------------------------------------------------------------------------------------------

my_pres<-
  read_pptx("MPFF_powerpoint_template.pptx") %>%
  
  # Add a title slide
  add_slide(layout="Title Slide2", 
            master="Office Theme") %>%
  ph_with(value = "Pelagic species in the Northeast Atlantic", 
          location = ph_location_type(type = "ctrTitle")) %>% 
  ph_with(value = "Martin Pastoors", 
          location = ph_location_type(type = "subTitle")) %>% 
  ph_with(value = "Global Pelagic Forum, 24 April 2023, Barcelona", 
          location = ph_location_label(ph_label="Location")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "mackerel2.jpg")), 
          location = ph_location_label(ph_label="Main picture")) 
  
# fileout <- "test.pptx"
# print(my_pres, target = fileout)

# knitr::kable(layout_summary(my_pres))
# layout_properties ( x = my_pres, layout = "Title Slide" )
# layout_properties ( x = my_pres, layout = "Title Slide2" )
# layout_properties ( x = my_pres, layout = "Title and Content")
# layout_properties ( x = my_pres, layout = "Section Header")
# layout_properties ( x = my_pres, layout = "Two Content")
# layout_properties ( x = my_pres, layout = "Blank")

# ---------------------------------------------------------------------------------------------
# Context of scientific advice in NE Atlantic
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Two Content", 
            master="Office Theme") %>%
  ph_with(value = "Context of scientific advice in the NE Atlantic", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = "Scientific advice by International Council for the Exploration of the Sea (ICES)", 
          location = ph_location_label(ph_label="Content left")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "ICES ecoregions with ICES areas.png")), 
          location = ph_location_label(ph_label="Content right")) 

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# Context of fisheries management in NE Atlantic
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Two Content", 
            master="Office Theme") %>%
  ph_with(value = "Fisheries management in the NE Atlantic", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = c("Northeast Atlantic Fisheries Commission (NEAFC)",
                    "Coastal States negotiations"), 
          location = ph_location_label(ph_label="Content left")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "neafc-area.png")), 
          location = ph_location_label(ph_label="Content right")) 

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# plot of catch per fisheries guild
# ---------------------------------------------------------------------------------------------

d <-
  iAssess %>% 
  ungroup() %>% 
  filter(assessmentyear == lay  ) %>% 
  filter(year           >= my.firstyear) %>% 
  filter(year           < my.lastyear) %>% 
  filter(purpose        %in% c("advice")) %>% 

  mutate(catches = ifelse(is.na(catches) | catches == 0, landings, catches))  %>% 
  mutate(fisheriesguild = ifelse(fisheriesguild %in% c("pelagic","demersal"), fisheriesguild, "other"))  %>% 
  mutate(fisheriesguild = factor(fisheriesguild, levels=c("other", "demersal", "pelagic"))) %>% 
  
  group_by(fisheriesguild, speciesfaocode, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  mutate(perc = catches/ sum(catches)) 

t <-
  d %>% 
  group_by(fisheriesguild, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE)/1000000,
            landings = sum(landings, na.rm=TRUE)/1000000) %>% 
  group_by(year) %>% 
  mutate(perc = catches/ sum(catches)) %>% 
  ungroup()
  
tt <-
  t %>% 
  filter(year == max(year))

ttt <-
  t %>% 
  group_by(year) %>% 
  summarise(catches = sum(catches, na.rm=TRUE))

# plot stacked catches: pelagic, demersal and other
p <-
  t %>% 
  group_by(fisheriesguild, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE),
            landings = sum(landings, na.rm=TRUE)) %>% 
  group_by(year) %>% 
  
  mutate(perc = round(100 * catches/ sum(catches), digits=2)) %>% 
  
  ggplot(aes(year,catches, group=fisheriesguild)) +
  theme_publication() +
  theme(legend.position="right", legend.direction = "vertical") +
  theme(plot.background = element_blank()) + 
  theme(panel.background = element_blank()) + 
  geom_col(aes(fill=(fisheriesguild))) +
  
  # percentage in final year
  geom_text(data=tt,
            aes(x=year, 
                label=scales::percent(perc)),
            position=position_stack(vjust=0.5), 
            hjust=0.5,
            vjust=0.5,
            # angle=90,
            size=3, 
            fontface="bold") +
    
  # 2021 catch
  geom_text(data=filter(ttt, year==2021),
            aes(x=year,
                y=catches,
                label=format(catches, digits=2)),
            position=position_stack(vjust=1.05),
            hjust=0.5,
            inherit.aes = FALSE,
            size=4,
            fontface="bold") +
    
  scale_fill_manual(values=myColors) +
  scale_colour_manual(values=myColors) +
  labs(x="",y="Catch (million tonnes)",title="", fill="") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(expand = expansion(add=c(1,1))) 

# save plot
png(filename=file.path(figuresdir, "catch_by_fisheriesguild.png"),
     width=12.5, height=5.5, units="in", res=300, bg="transparent")
print(p)
dev.off()
jpeg(filename=file.path(figuresdir, "catch_by_fisheriesguild.jpg"),
    width=12.5, height=5.5, units="in", res=300)
print(p)
dev.off()

# Add slide to presentation
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", 
            master="Office Theme") %>%
  ph_with(value = "Catches of pelagic, demersal and other stocks", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "catch_by_fisheriesguild.png")), 
          location = ph_location_type(type = "body"),
          use_loc_size = TRUE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)

# layout_properties ( x = my_pres, layout = "Title and Content")



# ---------------------------------------------------------------------------------------------
# plot of pelagic catches
# ---------------------------------------------------------------------------------------------

species  <- data.frame(
  speciesfaocode = c("other","san","spr","cap","mac","whb","her"),
  speciescommonname =c("Other","Sandeel","Sprat","Capelin","Mackerel","Blue whiting","Herring"))

myPelColors                <- brewer.pal(12, "Paired")[1:9]
names(myPelColors)         <- c("Other", "demersal", "pelagic",
                             "Herring", "Blue whiting", "Mackerel","Capelin","Sandeel","Sprat")

# now check the composition of the pelagic catches
t <-
  d %>% 
  filter(fisheriesguild == "pelagic") %>% 
  mutate(speciesfaocode = ifelse(speciesfaocode %in% species$speciesfaocode, 
                                 speciesfaocode, "other")) %>% 
  group_by(fisheriesguild, speciesfaocode, year) %>% 
  summarize(catches  = sum(catches, na.rm=TRUE)/1000000,
            landings = sum(landings, na.rm=TRUE)/1000000) %>% 
  group_by(year) %>% 
  mutate(perc = catches/ sum(catches)) %>% 
  left_join(species, by="speciesfaocode") %>% 
  mutate(speciescommonname=factor(speciescommonname, levels=species$speciescommonname)) %>% 
  ungroup()

tt <-
  t %>% 
  filter(year == max(year)) %>% 
  arrange(desc(catches)) 

ttt <-
  t %>% 
  group_by(year) %>% 
  summarise(catches = sum(catches, na.rm=TRUE))


p <-
  t %>%  
  ggplot(aes(year,catches)) +
  theme_publication() +
  theme(legend.position="right", legend.direct="vertical") +
  geom_bar(aes(fill=speciescommonname), stat="identity", position="stack") +
  
  # percentage in final year
  geom_text(data=tt,
            aes(x=year, 
                label=scales::percent(perc, accuracy = 1)),
            position=position_stack(vjust=0.5), 
            hjust=0.5,
            vjust=0.5,
            # angle=90,
            size=2.5, 
            fontface="bold") +
  
  # 2021 catch
  geom_text(data=filter(ttt, year==2021),
            aes(x=year,
                y=catches,
                label=format(catches, digits=2)),
            position=position_stack(vjust=1.05),
            hjust=0.5,
            inherit.aes = FALSE,
            size=4,
            fontface="bold") +
  
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values=myPelColors) +
  labs(x="",y="catch (million tonnes)",title="", fill="") 
  
# save plot
png(filename=file.path(figuresdir, "pelagic catches.png"),
     width=12.5, height=5.5, units="in", res=300, bg="transparent")
print(p)
dev.off()
jpeg(filename=file.path(figuresdir, "pelagic catches.jpg"),
     width=12.5, height=5.5, units="in", res=300)
print(p)
dev.off()

# Add slide to presentation
my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Catches of pelagic species", location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "pelagic catches.png")), 
          location = ph_location_type(type = "body")) 

fileout <- "test.pptx"
print(my_pres, target = fileout)



# ---------------------------------------------------------------------------------------------
# pelagic species slide 
# ---------------------------------------------------------------------------------------------

mactext <- fpar(ftext("Atlantic mackerel (MAC)", fp_text(color="white", font.size = 24)))
hertext <- fpar(ftext("Atlantic herring (HER)", fp_text(color="white", font.size = 24)))
whbtext <- fpar(ftext("Blue whiting (WHB)", fp_text(color="white", font.size = 24)))

mactext2 <- fpar(ftext("Scomber scombrus", fp_text(color="white", font.size = 22, italic = TRUE)))
hertext2 <- fpar(ftext("Clupea harengus", fp_text(color="white", font.size = 22, italic = TRUE)))
whbtext2 <- fpar(ftext("Micromesistius poutassou", fp_text(color="white", font.size = 22, italic = TRUE)))

my_pres <- my_pres %>% 
  add_slide(layout="Section Header", master="Office Theme") %>%
  ph_with(value = "Stocks", location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "mackerel herring blue whiting.png"), width=5, height=4), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 2, top = 2),
          use_loc_size = FALSE) %>% 
  ph_with(value = mactext, location = ph_location(left = 7, top = 2, height=0.5), use_loc_size = FALSE) %>% 
  ph_with(value = hertext, location = ph_location(left = 7, top = 3.3, height=0.5), use_loc_size = FALSE) %>% 
  ph_with(value = whbtext, location = ph_location(left = 7, top = 4.6, height=0.5), use_loc_size = FALSE) %>% 
  
  ph_with(value = mactext2, location = ph_location(left = 7, top = 2.4, right=12), use_loc_size = FALSE) %>% 
  ph_with(value = hertext2, location = ph_location(left = 7, top = 3.7, right=12), use_loc_size = FALSE) %>% 
  ph_with(value = whbtext2, location = ph_location(left = 7, top = 5.0, right=12), use_loc_size = FALSE) 
  
fileout <- "test.pptx"
print(my_pres, target = fileout)




# =============================================================================================
# ---------------------------------------------------------------------------------------------
# mackerel section slide 
# ---------------------------------------------------------------------------------------------
# =============================================================================================

my_pres <- my_pres %>% 
  add_slide(layout="Section Header", master="Office Theme") %>%
  ph_with(value = "Atlantic mackerel (MAC)", location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "mackerel.jpg"), width=11, height=2.5), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 2),
          use_loc_size = FALSE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# mackerel intro slide 
# ---------------------------------------------------------------------------------------------
ul <- unordered_list(
  level_list = c(1, 1, 1, 1, 1),
  str_list   = c("Catch area has expanded, and is contracting", 
                 "No agreement on sharing. Large overshoot catch over advice", 
                 "High recruitment, but stock coming down", 
                 "Very uncertain assessment", 
                 "Advice for 2023 slightly lower than advice 2022"))

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Key aspects of Northeast Atlantic mackerel", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = ul, 
          location = ph_location_type(type="body") )

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# mackerel spatial distribution slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Mackerel: catch area expanding and contracting", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "mackerel catch by rect.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="ICES WGWIDE 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# mackerel catch overshoot slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Mackerel: overshoot of catches over advice", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img("C:/DATA/CS MAC/Figures/catches and overshoot.png"), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="CS MAC WG 2022, ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# mackerel assessment slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Mackerel: high recruitment, stock coming down", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "mackerel assessment.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# mackerel assessment uncertainty slide 
# ---------------------------------------------------------------------------------------------

p <-
  plot_history(stock="mac-nea",
               firstassessyear=2010,
               lastassessyear=2022, 
               firstyear=my.firstyear, 
               lastyear=my.lastyear, 
               include.benchmark = TRUE,  
               include.replaced = FALSE,
               # plot = c("stocksize"),
               plot = c("stocksize", "fishingpressure"),
               plot.title=FALSE,
               plot.uncertainty=TRUE) 

# save plot
png(filename=file.path(figuresdir, "mackerel history.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
print(p +theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()
jpeg(filename=file.path(figuresdir, "mackerel history.jpg"),
     width=12.5, height=5.5, units="in", res=300)
print(p)
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Mackerel: very uncertain assessment", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "mackerel history.png")), 
          location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# mackerel advice outlook 
# ---------------------------------------------------------------------------------------------

# plot_advice("mac-nea",2018,2022, include.replaced=FALSE)
ft <-
  table_advice("mac-nea",2020,2023, include.replaced=FALSE, output.df=TRUE) %>% 
  dplyr::select(year=tacyear, advice, adv_c, tac, tac_c, uniq=unilateralquota, uni_c) %>% 
  mutate(across(c("advice", "tac", "uniq"), ~ .x /1000)) %>% 
  mutate(across(c("advice", "tac", "uniq"), as.integer)) %>% 
  mutate(across(names(.)[grepl("delta", names(.))], ~scales::percent(., accuracy=1))) %>% 
  mutate(across(c("adv_c", "tac_c", "uni_c"), ~scales::percent(./100, accuracy=1))) %>% 
  flextable::flextable() %>%
  # flextable::set_table_properties(width = 1, layout = "autofit") %>% 
  flextable::fontsize(size = 24, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[1], part = "body") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[2], part = "header") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[3], part = "body") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[4], part = "header") %>% 
  flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[7], part = "body") %>% 
  flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[8], part = "header") %>% 
  flextable::set_header_labels(values = list(year   = "YEAR",
                                             advice = "ADVICE",
                                             adv_c  = paste0("\u394", "adv"),
                                             tac    = "TAC",
                                             tac_c  = paste0("\u394", "tac"),
                                             uniq   = "Uni Quota",
                                             uni_c  = paste0("\u394", "uni"))) %>% 
  flextable::align(align = "center", part = "all") %>% 
  # flextable::height(height = .8) 
  flextable::autofit()

png(filename=file.path(figuresdir, "mackerel outlook.png"),
    width=8.95, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Mackerel: recommended (slightly) lower catch", 
          location = ph_location_type(type = "title")) %>% 
  # ph_with(value = ft, 
  #         location = ph_location_type(type = "body"))
  ph_with(value = external_img(file.path(figuresdir, "mackerel outlook.png"), width=11, height=4), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 1),
          use_loc_size = FALSE) 
# ph_with(value = external_img(file.path(figuresdir, "mackerel outlook.png")), 
#           location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)





# =============================================================================================
# ---------------------------------------------------------------------------------------------
# herring section slide 
# ---------------------------------------------------------------------------------------------
# =============================================================================================

my_pres <- my_pres %>% 
  add_slide(layout="Section Header", master="Office Theme") %>%
  ph_with(value = "Atlantic herring (HER)", location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "herring.jpg"), width=11, height=2.5), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 2),
          use_loc_size = FALSE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)


# =============================================================================================
# ---------------------------------------------------------------------------------------------
# herring stocks slide 
# ---------------------------------------------------------------------------------------------
# =============================================================================================

my_pres <- my_pres %>% 
  add_slide(layout="Blank", master="Office Theme") %>%
  ph_with(value = external_img(file.path(figuresdir, "herring stocks.png"), width=13.4, height=7.5), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 0, top = 0),
          use_loc_size = FALSE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# AS Herring intro slide 
# ---------------------------------------------------------------------------------------------
ul <- unordered_list(
  level_list = c(1, 1, 1, 1, 1),
  str_list   = c("Catch area seems to be contracting",
                 "No agreement on sharing; some overshoot over advice",
                 "Stock declining, despite strong year class",
                 "Relatively consistent assessment",
                 "Advice 2023 15% lower than for 2022"))

my_pres <- my_pres %>%
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Key aspects of Atlanto-scandian (AS) herring",
          location = ph_location_type(type = "title")) %>%
  ph_with(value = ul,
          location = ph_location_type(type="body") ) %>% 
  ph_with(value = external_img(file.path(figuresdir, "asherring area.png"), width=3, height=3), 
          location = ph_location(left = 10, top = 3),
          use_loc_size = FALSE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# AS herring spatial distribution slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "AS herring: contracting catching area", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "AS herring catch by rect.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="ICES WGWIDE 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# AS herring catch overshoot slide 
# ---------------------------------------------------------------------------------------------

t <-
  iAdvice %>% 
  filter(stockkeylabelold=="her-noss") %>% 
  filter(tacyear >= 1998, tacyear <= 2021) %>% 
  filter(purpose=="advice") %>% 
  dplyr::select(stockkeylabel, species = speciesfaocode, year=tacyear, advisedlandingsmax, advisedcatchmax, catches, landings, tac, tal) %>% 
  mutate(overshoot = catches/advisedlandingsmax-1)

tt <-
  t %>% 
  filter(year >= 2015) 

p <-
  t %>% 
  ggplot(aes(x=year, y=catches)) +
  theme_publication() +
  theme(legend.position = "none") +
  geom_bar(aes(fill=species), stat="identity") +
  geom_line(aes(y=advisedlandingsmax), linewidth=2) +
  geom_text(data=tt,
            aes(y=catches+10000, label=scales::percent(overshoot, accuracy=1)),
            vjust=0, size=4, fontface="bold") +
  scale_fill_manual(values=myColors) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x="", y="tonnes")

png(filename=file.path(figuresdir, "asherring overshoot.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
plot(p + theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "AS herring: some overshoot of catches over advice", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "asherring overshoot.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with("ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# AS herring assessment slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "AS herring: stock declining", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "asherring assessment.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# AS herring assessment uncertainty slide 
# ---------------------------------------------------------------------------------------------

p <-
  plot_history(stock="her-noss",
               firstassessyear=2010,
               lastassessyear=2022, 
               firstyear=my.firstyear, 
               lastyear=my.lastyear, 
               include.benchmark = TRUE,  
               include.replaced = FALSE,
               # plot = c("stocksize"),
               plot = c("stocksize", "fishingpressure"),
               plot.title=FALSE,
               plot.uncertainty=TRUE) 

# save plot
png(filename=file.path(figuresdir, "asherring history.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
print(p +theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()
jpeg(filename=file.path(figuresdir, "asherring history.jpg"),
     width=12.5, height=5.5, units="in", res=300)
print(p)
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "AS herring: relatively consistent assessment", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "asherring history.png")), 
          location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# AS herring advice outlook 
# ---------------------------------------------------------------------------------------------

# plot_advice("mac-nea",2018,2022, include.replaced=FALSE)
ft <-
  table_advice("her-noss",2020,2023, include.replaced=FALSE, output.df=TRUE) %>% 
  dplyr::select(year=tacyear, advice, adv_c, tac, tac_c, uniq=unilateralquota, uni_c) %>% 
  mutate(across(c("advice", "tac", "uniq"), ~ .x /1000)) %>% 
  mutate(across(c("advice", "tac", "uniq"), as.integer)) %>% 
  mutate(across(c("adv_c", "tac_c", "uni_c"), ~scales::percent(./100, accuracy=1))) %>% 
  flextable::flextable() %>%
  # flextable::set_table_properties(width = 1, layout = "autofit") %>% 
  flextable::fontsize(size = 24, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[1], part = "body") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[2], part = "header") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[3], part = "body") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[4], part = "header") %>% 
  flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[7], part = "body") %>% 
  flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[8], part = "header") %>% 
  flextable::set_header_labels(values = list(year   = "YEAR",
                                             advice = "ADVICE",
                                             adv_c  = paste0("\u394", "adv"),
                                             tac    = "TAC",
                                             tac_c  = paste0("\u394", "tac"),
                                             uniq   = "Uni Quota",
                                             uni_c  = paste0("\u394", "uni"))) %>% 
  flextable::align(align = "center", part = "all") %>% 
  # flextable::height(height = .8) 
  flextable::autofit()

png(filename=file.path(figuresdir, "asherring outlook.png"),
    width=8.95, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "AS herring: 15% lower advice", 
          location = ph_location_type(type = "title")) %>% 
  # ph_with(value = ft, 
  #         location = ph_location_type(type = "body"))
  ph_with(value = external_img(file.path(figuresdir, "asherring outlook.png"), width=11, height=4), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 1),
          use_loc_size = FALSE) 
# ph_with(value = external_img(file.path(figuresdir, "mackerel outlook.png")), 
#           location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)



# ---------------------------------------------------------------------------------------------
# NS Herring intro slide 
# ---------------------------------------------------------------------------------------------
ul <- unordered_list(
  level_list = c(1, 1, 1, 1, 1),
  str_list   = c("Some overshoots of catch over advice",
                 "Stock coming down (close to trigger biomass)",
                 "Uncertainty in stock level in assessment (scaling)",
                 "Stable spatial patterns in catches",
                 "Advice for 23% reduction (relative to high advice 2022)"))

my_pres <- my_pres %>%
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Key aspects of North Sea herring",
          location = ph_location_type(type = "title")) %>%
  ph_with(value = ul,
          location = ph_location_type(type="body") ) %>% 
  ph_with(value = external_img(file.path(figuresdir, "nsherring area.png"), width=2.5, height=3), 
          location = ph_location(left = 10.5, top = 0.5),
          use_loc_size = FALSE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)

fileout <- "Pastoors 2023 northeast atlantic pelagic stocks.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# NS herring spatial distribution slide - NOT NEEDED
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------
# NS herring catch overshoot slide 
# ---------------------------------------------------------------------------------------------

t <-
  iAdvice %>% 
  filter(stockkeylabelold=="her-47d3") %>% 
  filter(tacyear >= 1998, tacyear <= 2021) %>% 
  filter(purpose=="advice") %>% 
  dplyr::select(stockkeylabel, species = speciesfaocode, year=tacyear, advisedlandingsmax, advisedcatchmax, catches, landings, tac, tal) %>% 
  mutate(overshoot = catches/advisedlandingsmax-1)

tt <-
  t %>% 
  filter(year >= 2015) 

p <-
  t %>% 
  ggplot(aes(x=year, y=catches)) +
  theme_publication() +
  theme(legend.position = "none") +
  geom_bar(aes(fill=species), stat="identity") +
  geom_line(aes(y=advisedlandingsmax), linewidth=2) +
  geom_text(data=tt,
            aes(y=catches+10000, label=scales::percent(overshoot, accuracy=1)),
            vjust=0, size=4, fontface="bold") +
  scale_fill_manual(values=myColors) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x="", y="tonnes")

png(filename=file.path(figuresdir, "nsherring overshoot.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
plot(p + theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "NS herring: some overshoot of catches over advice", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "nsherring overshoot.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with("ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# NS herring assessment slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "NS herring: stock assessment", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "nsherring assessment.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# NS herring assessment uncertainty slide 
# ---------------------------------------------------------------------------------------------

p <-
  plot_history(stock="her-47d3",
               firstassessyear=2010,
               lastassessyear=2022, 
               firstyear=my.firstyear, 
               lastyear=my.lastyear, 
               include.benchmark = TRUE,  
               include.replaced = FALSE,
               # plot = c("stocksize"),
               plot = c("stocksize", "fishingpressure"),
               plot.title=FALSE,
               plot.uncertainty=TRUE) 

# save plot
png(filename=file.path(figuresdir, "nsherring history.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
print(p +theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()
jpeg(filename=file.path(figuresdir, "nsherring history.jpg"),
     width=12.5, height=5.5, units="in", res=300)
print(p)
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "NS herring: uncertainty in stock level", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "nsherring history.png")), 
          location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# NS herring advice outlook 
# ---------------------------------------------------------------------------------------------

# plot_advice("mac-nea",2018,2022, include.replaced=FALSE)
ft <-
  table_advice("her-47d3",2020,2023, include.replaced=FALSE, output.df=TRUE) %>% 
  dplyr::select(year=tacyear, advice, adv_c, tac, tac_c, catch, cat_c) %>% 
  mutate(across(c("advice", "tac","catch"), ~ .x /1000)) %>% 
  mutate(across(c("advice", "tac", "catch"), as.integer)) %>% 
  mutate(across(c("adv_c", "tac_c", "cat_c"), ~scales::percent(./100, accuracy=1))) %>% 
  flextable::flextable() %>%
  # flextable::set_table_properties(width = 1, layout = "autofit") %>% 
  flextable::fontsize(size = 24, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[1], part = "body") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[2], part = "header") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[3], part = "body") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[4], part = "header") %>% 
  flextable::bg(j = c('catch', 'cat_c'), bg = myColors[5], part = "body") %>% 
  flextable::bg(j = c('catch', 'cat_c'), bg = myColors[6], part = "header") %>% 
  flextable::set_header_labels(values = list(year   = "YEAR",
                                             advice = "ADVICE",
                                             adv_c  = paste0("\u394", "adv"),
                                             tac    = "TAC",
                                             tac_c  = paste0("\u394", "tac"),
                                             catch   = "CATCH",
                                             cat_c  = paste0("\u394", "cat"))) %>% 
  flextable::align(align = "center", part = "all") %>% 
  # flextable::height(height = .8) 
  flextable::autofit()

png(filename=file.path(figuresdir, "nsherring outlook.png"),
    width=8.95, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "NS herring: 23% lower advice", 
          location = ph_location_type(type = "title")) %>% 
  # ph_with(value = ft, 
  #         location = ph_location_type(type = "body"))
  ph_with(value = external_img(file.path(figuresdir, "nsherring outlook.png"), width=11, height=4), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 1),
          use_loc_size = FALSE) 
# ph_with(value = external_img(file.path(figuresdir, "mackerel outlook.png")), 
#           location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# =============================================================================================
# ---------------------------------------------------------------------------------------------
# blue whiting section slide 
# ---------------------------------------------------------------------------------------------
# =============================================================================================

my_pres <- my_pres %>% 
  add_slide(layout="Section Header", master="Office Theme") %>%
  ph_with(value = "Blue whiting (WHB)", location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "blue whiting.jpg"), width=11, height=2.5), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 2),
          use_loc_size = FALSE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# Blue whiting intro slide 
# ---------------------------------------------------------------------------------------------
ul <- unordered_list(
  level_list = c(1, 1, 1, 1, 1),
  str_list   = c("Stable catching area",
                 "No agreement on sharing; substantial overshoot over advice",
                 "Stock increasing due to very strong recruitment",
                 "Relatively consistent assessment",
                 "Advice: 81% increase relative to 2022 advice"))

my_pres <- my_pres %>%
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Key aspects of Blue whiting",
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = ul,
          location = ph_location_type(type="body") )

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# Blue whiting spatial distribution slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Blue whiting: stability in catching area", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "Blue whiting catch by rect.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="ICES WGWIDE 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# Blue whiting catch overshoot slide 
# ---------------------------------------------------------------------------------------------

t <-
  iAdvice %>% 
  filter(stockkeylabelold=="whb-comb") %>% 
  filter(tacyear >= 1998, tacyear <= 2021) %>% 
  filter(purpose=="advice") %>% 
  dplyr::select(stockkeylabel, species = speciesfaocode, year=tacyear, advisedlandingsmax, advisedcatchmax, catches, landings, tac, tal) %>% 
  mutate(overshoot = catches/advisedlandingsmax-1)

tt <-
  t %>% 
  filter(year >= 2014) 

p <-
  t %>% 
  ggplot(aes(x=year, y=catches)) +
  theme_publication() +
  theme(legend.position = "none") +
  geom_bar(aes(fill=species), stat="identity") +
  geom_line(aes(y=advisedlandingsmax), linewidth=2) +
  geom_text(data=tt,
            aes(y=catches+10000, label=scales::percent(overshoot, accuracy=1)),
            vjust=0, size=4, fontface="bold") +
  scale_fill_manual(values=myColors) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(x="", y="tonnes")

png(filename=file.path(figuresdir, "whb overshoot.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
plot(p + theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Blue whiting: overshoot of catches over advice", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "whb overshoot.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with("ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# Blue whiting assessment slide 
# ---------------------------------------------------------------------------------------------

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Blue whiting: very strong incoming recruitment", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "whb assessment.png")), 
          location = ph_location_type(type = "body")) %>% 
  ph_with(value="ICES ACOM 2022",
          location = ph_location_type(type = "ftr"))

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# Blue whiting assessment uncertainty slide 
# ---------------------------------------------------------------------------------------------

p <-
  plot_history(stock="whb-comb",
               firstassessyear=2010,
               lastassessyear=2022, 
               firstyear=my.firstyear, 
               lastyear=my.lastyear, 
               include.benchmark = TRUE,  
               include.replaced = FALSE,
               # plot = c("stocksize"),
               plot = c("stocksize", "fishingpressure"),
               plot.title=FALSE,
               plot.uncertainty=TRUE) 

# save plot
png(filename=file.path(figuresdir, "whb history.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
print(p +theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()
jpeg(filename=file.path(figuresdir, "whb history.jpg"),
     width=12.5, height=5.5, units="in", res=300)
print(p)
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Blue whiting: relatively consistent assessment", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "whb history.png")), 
          location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# Blue whiting advice outlook 
# ---------------------------------------------------------------------------------------------

# plot_advice("mac-nea",2018,2022, include.replaced=FALSE)
ft <-
  table_advice("whb-comb",2020,2023, include.replaced=FALSE, output.df=TRUE) %>% 
  dplyr::select(year=tacyear, advice, adv_c, tac, tac_c, uniq=unilateralquota, uni_c) %>% 
  mutate(across(c("advice", "tac","uniq"), ~ .x /1000)) %>% 
  mutate(across(c("advice", "tac", "uniq"), as.integer)) %>% 
  mutate(across(c("adv_c", "tac_c", "uni_c"), ~scales::percent(./100, accuracy=1))) %>% 
  flextable::flextable() %>%
  # flextable::set_table_properties(width = 1, layout = "autofit") %>% 
  flextable::fontsize(size = 24, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[1], part = "body") %>% 
  flextable::bg(j = c('advice', 'adv_c'), bg = myColors[2], part = "header") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[3], part = "body") %>% 
  flextable::bg(j = c('tac', 'tac_c'), bg = myColors[4], part = "header") %>% 
  flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[7], part = "body") %>% 
  flextable::bg(j = c('uniq', 'uni_c'), bg = myColors[8], part = "header") %>% 
  flextable::set_header_labels(values = list(year   = "YEAR",
                                             advice = "ADVICE",
                                             adv_c  = paste0("\u394", "adv"),
                                             tac    = "TAC",
                                             tac_c  = paste0("\u394", "tac"),
                                             uniq   = "Uni Quota",
                                             uni_c  = paste0("\u394", "uni"))) %>% 
  flextable::align(align = "center", part = "all") %>% 
  # flextable::height(height = .8) 
  flextable::autofit()

png(filename=file.path(figuresdir, "whb outlook.png"),
    width=9.5, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Blue whiting: strong increase in advice", 
          location = ph_location_type(type = "title")) %>% 
  # ph_with(value = ft, 
  #         location = ph_location_type(type = "body"))
  ph_with(value = external_img(file.path(figuresdir, "whb outlook.png"), width=11, height=4), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 1),
          use_loc_size = FALSE) 
# ph_with(value = external_img(file.path(figuresdir, "mackerel outlook.png")), 
#           location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)




# ---------------------------------------------------------------------------------------------
# pelagic summary slide 
# ---------------------------------------------------------------------------------------------

# mactext <- fpar(ftext("Atlantic mackerel (MAC)", fp_text(color="white", font.size = 24)))
# hertext <- fpar(ftext("Atlantic herring (HER)", fp_text(color="white", font.size = 24)))
# whbtext <- fpar(ftext("Blue whiting (WHB)", fp_text(color="white", font.size = 24)))
# 
# mactext2 <- fpar(ftext("Scomber scombrus", fp_text(color="white", font.size = 22, italic = TRUE)))
# hertext2 <- fpar(ftext("Clupea harengus", fp_text(color="white", font.size = 22, italic = TRUE)))
# whbtext2 <- fpar(ftext("Micromesistius poutassou", fp_text(color="white", font.size = 22, italic = TRUE)))

my_pres <- my_pres %>% 
  add_slide(layout="Section Header", master="Office Theme") %>%
  ph_with(value = "Summary", location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "mackerel herring blue whiting.png"), width=5, height=4), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 2, top = 2),
          use_loc_size = FALSE) %>% 
  ph_with(value = mactext, location = ph_location(left = 7, top = 2, height=0.5), use_loc_size = FALSE) %>% 
  ph_with(value = hertext, location = ph_location(left = 7, top = 3.3, height=0.5), use_loc_size = FALSE) %>% 
  ph_with(value = whbtext, location = ph_location(left = 7, top = 4.6, height=0.5), use_loc_size = FALSE) %>% 
  
  ph_with(value = mactext2, location = ph_location(left = 7, top = 2.4, right=12), use_loc_size = FALSE) %>% 
  ph_with(value = hertext2, location = ph_location(left = 7, top = 3.7, right=12), use_loc_size = FALSE) %>% 
  ph_with(value = whbtext2, location = ph_location(left = 7, top = 5.0, right=12), use_loc_size = FALSE) 

fileout <- "test.pptx"
print(my_pres, target = fileout)

# ---------------------------------------------------------------------------------------------
# Pelagic stocks assessment uncertainty slide 
# ---------------------------------------------------------------------------------------------

p <-
  plot_history(stock=c("whb-comb","mac-nea", "her-47d3", "her-noss"),
               firstassessyear=2010,
               lastassessyear=2022, 
               firstyear=my.firstyear, 
               lastyear=my.lastyear, 
               include.benchmark = FALSE,  
               include.replaced = FALSE,
               # plot = c("stocksize"),
               plot = c("stocksize"),
               plot.title=FALSE,
               plot.uncertainty=FALSE,
               stockkeylabeltype = "stockkeylabelold") 

# save plot
png(filename=file.path(figuresdir, "pelagic assessment history.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
print(p +theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()
# jpeg(filename=file.path(figuresdir, "whb history.jpg"),
#      width=12.5, height=5.5, units="in", res=300)
# print(p)
# dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Pelagic assessments are uncertain/declining ...", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "pelagic assessment history.png")), 
          location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)



# ---------------------------------------------------------------------------------------------
# plot of catch and forecast for key pelagic species 
# ---------------------------------------------------------------------------------------------

mystocks <- c("whb-comb","mac-nea","her-47d3", "her-noss")

t <-
  iAdvice %>% 
  filter(stockkeylabelold %in% mystocks) %>% 
  # filter(assessmentyear == 2018) %>% 
  filter(tacyear >= 2010, tacyear <= 2023) %>%
  # filter(!is.na(catches)) %>% 
  filter(purpose %in% c("advice")) %>% 
  rename(year=tacyear) %>% 
  rename(species=speciesfaocode) %>% 
  group_by(stockkeylabelold, year) %>% 
  filter(row_number()==1) %>% 
  group_by(year) %>% 
  mutate(catches = catches/1000000) %>% 
  mutate(advisedlandingsmax = advisedlandingsmax/1000000) %>% 
  mutate(perc = round(100 * catches/ sum(catches), digits=0)) %>% 
  mutate(stockkeylabelold=factor(stockkeylabelold, levels=mystocks)) %>% 
  arrange(stockkeylabelold, year) %>% 
  ungroup()

t1 <-
  t %>% 
  dplyr::select(stockkeylabelold, species, year, catches) %>% 
  drop_na(catches) 

t2 <-
  readxl::read_xlsx(file.path(figuresdir, "imy_catch.xlsx")) %>% 
  mutate(stockkeylabelold=factor(stockkeylabelold, levels=mystocks)) %>% 
  mutate(imycatch = imycatch/1000000)
  
t3 <-
  t %>% 
  dplyr::select(stockkeylabelold, species, year, advisedlandingsmax) %>% 
  filter(year == (max(year, na.rm=TRUE))) 


myTempColors                <- brewer.pal(12, "Paired")[1:4]
names(myTempColors)         <- mystocks
                             
# filter(d, stockkeylabelold=="whb-comb", year >= 2015) %>% View()
# filter(d, year == 2018) %>% View()
# d %>% ungroup() %>% filter(year == 2018) %>% summarize(catches=sum(catches, na.rm=TRUE))

# plot stacked catches
p <-
  t1 %>% 
  ggplot(aes(year,catches, group=stockkeylabelold)) +
  theme_publication() +
  theme(legend.position = "right", legend.direction = "vertical") +
  
  geom_col(aes(fill=stockkeylabelold), 
           alpha=0.5) +
  # geom_text(aes(x=year, y=catches, label=format(catches, digits=2)),
  #           position=position_stack(vjust=0.5), hjust=0.5, vjust=0.5, size=4, fontface="bold") +
  
  geom_col(data=t2,
           aes(x=year, y=imycatch, fill=stockkeylabelold, group=stockkeylabelold), 
           alpha=1.0) +
  geom_text(data=t2,
            aes(x=year, y=imycatch, label=format(imycatch, digits=1)),
            position=position_stack(vjust=0.5), hjust=0.5, vjust=0.5, size=4, fontface="bold") +
  geom_text(data=(t2 %>% group_by(year) %>% summarise(imycatch=sum(imycatch))),
            aes(x=year, y=imycatch+0.05, label=format(imycatch, digits=2)),
            vjust=0, size=4, fontface="bold", inherit.aes = FALSE) +
  
  geom_col(data=t3,
           aes(y=advisedlandingsmax, fill=stockkeylabelold, group=stockkeylabelold), 
           position="stack", alpha=1.0) +
  geom_text(data=t3,
            aes(x=year, y=advisedlandingsmax, label=format(advisedlandingsmax, digits=1)),
            position=position_stack(vjust=0.5), hjust=0.5, vjust=0.5, size=4, fontface="bold") +
  geom_text(data=(t3 %>% group_by(year) %>% summarise(advisedlandingsmax=sum(advisedlandingsmax))),
            aes(x=year, y=advisedlandingsmax+0.05, label=format(advisedlandingsmax, digits=2)),
            vjust=0, size=4, fontface="bold", inherit.aes = FALSE) +
  
  geom_rect(aes(xmin=my.lastyear-0.5, xmax=my.lastyear+0.5, ymin=0, ymax=4), colour=myColors[2], fill=NA, linewidth=0.8) +
  geom_text(aes(x = my.lastyear, y=4, label="current"), colour=myColors[2], size=4, vjust=0, hjust=0.5) +
  
  geom_rect(aes(xmin=my.lastyear+0.5, xmax=my.lastyear+1.5, ymin=0, ymax=4), colour=myColors[6], fill=NA, linewidth=0.8) +
  geom_text(aes(x = my.lastyear+1, y=4, label="advice"), colour=myColors[6], size=4, vjust=0, hjust=0.5) +
  
  labs(x="",y="Million tonnes",title="", fill="") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks=seq(min(t1$year), max(t3$year),1)) +
  scale_fill_manual(values=myTempColors) 

png(filename=file.path(figuresdir, "pelagic outlook.png"),
    width=12.5, height=5.5, units="in", res=300, bg="transparent")
plot(p +theme(plot.background = element_blank(), panel.background = element_blank()))
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Increase in blue whiting, decrease others", 
          location = ph_location_type(type = "title")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "pelagic outlook.png"), width=11, height=4), 
          location = ph_location_type(type = "body"))
          # location = ph_location(left = 1, top = 1),
          # use_loc_size = FALSE)

fileout <- "test.pptx"
print(my_pres, target = fileout)


# ---------------------------------------------------------------------------------------------
# Pelagic advice outlook 
# ---------------------------------------------------------------------------------------------

ft <-
  table_advice(c("whb-comb","mac-nea", "her-noss","her-47d3"),2022,2023, include.replaced=FALSE, output.df=TRUE) %>% 
  dplyr::select(stock=stockkeylabelold, year=tacyear, adv=advice, tac, uniq=unilateralquota) %>% 
  mutate(across(c("adv", "tac", "uniq"), ~ .x /1000)) %>% 
  
  pivot_longer(names_to = "variable", values_to = "data", adv:uniq) %>% 
  mutate(variable = factor(variable, levels=c("adv","tac","uniq"))) %>% 
  arrange(stock, variable, year) %>% 
  group_by(stock, variable) %>% 
  mutate(delta = data/lag(data)-1) %>% 
  ungroup() %>% 
  
  pivot_longer(names_to = "var2", values_to = "data2", data:delta) %>% 
  arrange(stock, variable, year) %>% 
  reshape2::dcast(stock ~ variable+var2+year, value.var="data2", sum, margins="stock") %>% 
  
  # remove current year delta
  dplyr::select(-(names(.)[grepl("delta",names(.)) & grepl("2022",names(.))])) %>% 
  mutate(across(names(.)[grepl("data", names(.))], as.integer)) %>% 
  mutate(across(names(.)[grepl("delta", names(.))], ~scales::percent(., accuracy=1))) %>% 
  
  flextable::flextable() %>%
  # flextable::set_table_properties(width = 1, layout = "autofit") %>% 
  flextable::fontsize(size = 20, part = "all") %>% 
  flextable::colformat_num(j=1, big.mark="") %>% 
  flextable::bg(j = c(2:4), bg = myColors[1], part = "body") %>% 
  flextable::bg(j = c(2:4), bg = myColors[2], part = "header") %>% 
  flextable::bg(j = c(5:7), bg = myColors[3], part = "body") %>% 
  flextable::bg(j = c(5:7), bg = myColors[4], part = "header") %>% 
  flextable::bg(j = c(8:10), bg = myColors[7], part = "body") %>% 
  flextable::bg(j = c(8:10), bg = myColors[8], part = "header") %>% 
  flextable::bold(i = 5, j = NULL, bold = TRUE, part = "body") %>% 
  flextable::hline(i = 4, j = NULL, border = NULL, part = "body") %>% 
  flextable::set_header_labels(values = c("stock", "2022", "2023", paste0("\u394"),
                                                   "2022","2023",paste0("\u394"),
                                                   "2022", "2023", paste0("\u394"))) %>% 
  flextable::add_header_row(values = c("", "advice", "TAC", "Unilat. Quota"), colwidths = c(1, 3, 3, 3)) %>% 
  flextable::align(align = "center", part = "all") %>% 
  flextable::autofit()

png(filename=file.path(figuresdir, "pelagic outlook table.png"),
    width=9.5, height=2.85, units="in", res=300, bg="transparent")
plot(ft, fit = "fixed", just = "center")
dev.off()

my_pres <- my_pres %>% 
  add_slide(layout="Title and Content", master="Office Theme") %>%
  ph_with(value = "Pelagic stocks: outlook", 
          location = ph_location_type(type = "title")) %>% 
  # ph_with(value = ft, 
  #         location = ph_location_type(type = "body"))
  ph_with(value = external_img(file.path(figuresdir, "pelagic outlook table.png"), width=11, height=4), 
          # location = ph_location_type(type = "pic"),
          location = ph_location(left = 1, top = 1),
          use_loc_size = FALSE) 
# ph_with(value = external_img(file.path(figuresdir, "mackerel outlook.png")), 
#           location = ph_location_type(type = "body"))

fileout <- "test.pptx"
print(my_pres, target = fileout)

# table_advice(c("whb-comb", "mac-nea","her-47d3", "her-noss"),2020,2023, include.replaced=FALSE, output.df=TRUE) %>% 
#   writexl::write_xlsx(path="pelagic advice and management.xlsx")
  
# final slide
my_pres <- my_pres %>% 
  add_slide(layout="Title Slide2", master="Office Theme") %>%
  ph_with(value = "Thank you", 
          location = ph_location_type(type = "ctrTitle")) %>% 
  ph_with(value = "martinpastoors@mpff.nl", 
          location = ph_location_type(type = "subTitle")) %>% 
  ph_with(value = external_img(file.path(figuresdir, "DSCN0570 sea.jpg")), 
          location = ph_location_label(ph_label="Main picture")) 

fileout <- "test.pptx"
print(my_pres, target = fileout)


fileout <- "Pastoors 2023 Northeast Atlantic pelagic stocks.pptx"
print(my_pres, target = fileout)
