# ------------------------------------------------------------------------------------------------------------------
# iPlot_utils.r
# 
# Plotting routines for iAssess and iAdvice databases
#
# 29/09/2019 code taken from ICES advice and assessment summaries
#
# Functions:
# - plot_history
# - table_advice
# ------------------------------------------------------------------------------------------------------------------


# Function to plot historical assessments
plot_history <- function(stock,firstassessyear,lastassessyear, firstyear, lastyear, include.benchmark = FALSE, 
                         plot=c("stocksize","fishingpressure")) {
  
  d <-
    iAssess %>% 
    ungroup() %>% 
    
    {if (include.benchmark == TRUE) {
      filter(., purpose %in% c("advice","benchmark", "interbenchmark", "bench"))
    } else {
      filter(., purpose %in% c("advice")) 
    }} %>%
    
    filter(stockkeylabelold %in% stock | stockkeylabel %in% stock) %>% 
    
    filter(assessmentyear   %in% c(firstassessyear:lastassessyear), 
           year             %in% firstyear:lastyear) %>% 
    
    mutate(tyear     = substr(as.character(assessmentyear), 3,4)) %>% 
    
    #set type
    mutate(
      type = ifelse(assessmentyear == lastassessyear & purpose == "advice"       , "last"      , NA),
      type = ifelse(assessmentyear == lastassessyear & grepl("bench",purpose),  "last_bench", type),  
      type = ifelse(assessmentyear != lastassessyear & purpose == "advice"       , "advice"    , type),
      type = ifelse(assessmentyear != lastassessyear & grepl("bench",purpose),  "benchmark" , type),
      type = ifelse(assessmentyear != lastassessyear & grepl("replaced",purpose) , "replaced"  , type),
      type = ifelse(is.na(type), "other", type)) %>%
    
    # set group
    mutate(group = paste0(tyear, type)) %>% 
    
    select(assessmentyear, tyear, year, stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew,
           type, group, source,
           lowrecruitment, recruitment, highrecruitment, 
           lowfishingpressure, fishingpressure, highfishingpressure, 
           lowstocksize, stocksize, highstocksize) %>%
    data.frame()
  
  # find out what is the last assessment
  last <- filter(d, assessmentyear == lastassessyear) %>% distinct(type) %>% as.character()
  
  # plot stocksize
  stocksize <-
    d %>% 
    filter(!is.na(stocksize)) %>%  
    
    ggplot(aes(year,stocksize, group=group)) +
    
    theme_publication() +
    
    theme(legend.title=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
          axis.text.y = element_text(size=9),
          plot.title  = element_text(size=10), 
          plot.margin = margin(2, 2, 2, 2, "mm"),
          # strip.background = element_blank(),
          legend.position = "null") +
    
    geom_ribbon(data=filter(d, type==last), 
                aes(x=year, ymin=lowstocksize, ymax=highstocksize, fill=type), 
                alpha=0.2) +
    geom_line(aes(colour = type, size=type, linetype=type) ) +
    
    geom_dl(aes(label  = tyear, colour = type), method = list(dl.combine("last.points"), cex = 0.8)) +
    
    geom_hline(data=filter(iAdvice, stockkeylabelold %in% stock | stockkeylabel %in% stock, assessmentyear == lastassessyear),
               aes(x=assessmentyear, yintercept=blim, label="blim", vjust=-1), 
               colour="red", inherit.aes=FALSE) +
    geom_text (data=filter(iAdvice, stockkeylabelold %in% stock | stockkeylabel %in% stock, assessmentyear == lastassessyear),
               aes( firstyear, blim, label = "blim", vjust = -0.3, hjust = 0),
               colour="red", inherit.aes = FALSE) +
    
    geom_hline(data=filter(iAdvice, stockkeylabelold %in% stock | stockkeylabel %in% stock, assessmentyear == lastassessyear),
               aes(x=assessmentyear, yintercept=msybtrigger), 
               colour="red", linetype="dashed", inherit.aes=FALSE) +
    geom_text (data=filter(iAdvice, stockkeylabelold %in% stock | stockkeylabel %in% stock, assessmentyear == lastassessyear),
               aes( firstyear, msybtrigger, label = "msy btrigger", vjust = -0.3, hjust = 0),
               colour="red", inherit.aes=FALSE) +
    
    scale_colour_manual  (values=c(last ="red",last_bench="blue",advice="black",benchmark="blue",replaced="gray40",other="gray80")) +
    scale_fill_manual    (values=c(last = "red",last_bench="blue",advice="black",benchmark="blue",replaced="gray40",other="gray80")) +
    scale_linetype_manual(values=c(last="solid",last_bench="solid",advice="solid",benchmark="solid",replaced="solid",other="solid")) +
    scale_size_manual    (values=c(last= 1.2,last_bench=1.2,advice=0.8,benchmark=0.8,replaced=0.8,other=0.8)) +
    
    expand_limits(y = 0) +
    xlim(firstyear,lastyear) +
    labs(x = NULL, y = NULL , title = "stocksize")  
  
  # fishingpressure
  fishingpressure <-
    d %>% 
    filter(!is.na(fishingpressure)) %>%  
    
    ggplot(aes(year,fishingpressure, group=group)) +
    
    theme_publication() +
    theme(legend.title=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
          axis.text.y = element_text(size=9),
          plot.title  = element_text(size=10), 
          plot.margin = margin(2, 2, 2, 2, "mm"),
          # strip.background = element_blank(),
          legend.position = "null") +
    
    geom_ribbon(data=filter(d, type==last), 
                aes(x=year, ymin=lowfishingpressure, ymax=highfishingpressure, fill=type), 
                alpha=0.2) +
    geom_line(aes(colour = type, size=type, linetype=type) ) +
    
    geom_dl(aes(label  = tyear, colour = type), method = list(dl.combine("last.points"), cex = 0.8)) +
    
    geom_hline(data=filter(iAdvice, stockkeylabelold %in% stock | stockkeylabel %in% stock, assessmentyear == lastassessyear),
               aes(x=assessmentyear, yintercept=fmsy), 
               colour="red", linetype="dashed", inherit.aes=FALSE) +
    geom_text (data=filter(iAdvice, stockkeylabelold %in% stock | stockkeylabel %in% stock, assessmentyear == lastassessyear),
               aes( firstyear, fmsy, label = "fmsy", vjust = -0.3, hjust = 0),
               colour="red", inherit.aes=FALSE) +
    
    scale_colour_manual  (values=c(last ="red",last_bench="blue",advice="black",benchmark="blue",replaced="gray40",other="gray80")) +
    scale_fill_manual    (values=c(last = "red",last_bench="blue",advice="black",benchmark="blue",replaced="gray40",other="gray80")) +
    scale_linetype_manual(values=c(last="solid",last_bench="solid",advice="solid",benchmark="solid",replaced="solid",other="solid")) +
    scale_size_manual    (values=c(last= 1.2,last_bench=1.2,advice=0.8,benchmark=0.8,replaced=0.8,other=0.8)) +
    
    expand_limits(y = 0) +
    xlim(firstyear,lastyear) +
    labs(x = NULL, y = NULL , title = "fishingpressure")  
  
  # Recruitment
  recruitment <-
    d %>% 
    filter(!is.na(recruitment)) %>%  
    
    ggplot(aes(year,recruitment, group=group)) +
    
    theme_publication() +
    theme(legend.title=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
          axis.text.y = element_text(size=9),
          plot.title  = element_text(size=10), 
          plot.margin = margin(2, 2, 2, 2, "mm"),
          # strip.background = element_blank(),
          legend.position = "null") +
    
    geom_ribbon(data=filter(d, type==last), 
                aes(x=year, ymin=lowrecruitment, ymax=highrecruitment, fill=type), 
                alpha=0.2) +
    geom_line(aes(colour = type, size=type, linetype=type) ) +
    
    geom_dl(aes(label  = tyear, colour = type), method = list(dl.combine("last.points"), cex = 0.8)) +
    
    scale_colour_manual  (values=c(last ="red",last_bench="blue",advice="black",benchmark="blue",replaced="gray40",other="gray80")) +
    scale_fill_manual    (values=c(last = "red",last_bench="blue",advice="black",benchmark="blue",replaced="gray40",other="gray80")) +
    scale_linetype_manual(values=c(last="solid",last_bench="solid",advice="solid",benchmark="solid",replaced="solid",other="solid")) +
    scale_size_manual    (values=c(last= 1.2,last_bench=1.2,advice=0.8,benchmark=0.8,replaced=0.8,other=0.8)) +
    
    expand_limits(y = 0) +
    xlim(firstyear,lastyear) +
    labs(x = NULL, y = NULL , title = "Recruitment")  
  
  if (length(plot) == 1) {
    if ("stocksize" %in% plot ) print(stocksize)
    if ("fishingpressure" %in% plot ) print(fishingpressure)
    if ("recruitment" %in% plot ) print(recruitment)
    
  } else if (length(plot) == 2) {
    
    if ("stocksize" %in% plot & "fishingpressure" %in% plot) plot_grid(stocksize, fishingpressure,  ncol=2, align = 'hv') 
    if ("stocksize" %in% plot & "recruitment" %in% plot) plot_grid(stocksize, recruitment,  ncol=2, align = 'hv') 
    if ("recruitment" %in% plot & "fishingpressure" %in% plot) plot_grid(fishingpressure, recruitment,  ncol=2, align = 'hv') 
    
  } else if (length(plot) == 3) {
    plot_grid(stocksize, fishingpressure,  recruitment, ncol=length(plot), align = 'hv') 
  }  
  
} # End of plot_history function

# plot_history(stock=s,firstassessyear=fay,lastassessyear=lay, firstyear=fy, lastyear=ly, include.benchmark = TRUE,               plot = c("stocksize", "fishingpressure", "recruitment"), include.benchmark = TRUE) 


# Function to plot advice overviews
table_advice <- function(stock,firstadviceyear,lastadviceyear, include.replaced=FALSE) {
  
  d <-
    iAdvice %>% 
    
    filter(stockkeylabelold %in% stock | stockkeylabel %in% stock) %>% 
    
    filter(tacyear %in% firstadviceyear:lastadviceyear) %>% 
    
    {if (include.replaced == TRUE) {
      filter(., purpose %in% c("advice","replaced"))
    } else {
      filter(., purpose %in% c("advice")) 
    }} %>%
    
    mutate(advicebasis= substr(advicebasis,1,25),
           
           advice     = ifelse(!is.na(advisedcatchmax), advisedcatchmax, advisedlandingsmax),
           advicemax  = ifelse(!is.na(advisedcatchmax), advisedcatchmax, advisedlandingsmax),
           advicetype = ifelse(!is.na(advisedcatchmax), "catch", "landings"),
           
           tac        = ifelse(!is.na(tac), tac, tal),
           tactype    = ifelse(!is.na(tac), "catch", "landings"), 
           
           catch      = ifelse(!is.na(catches), catches, NA),
           catchtype  = ifelse(!is.na(catches), "catches", ""),
           
           catchtype  = ifelse(!is.na(landings) & is.na(catch), "landings", catchtype),
           catch      = ifelse(!is.na(landings) & is.na(catch), landings, catch),
           
           catchtype  = ifelse(!is.na(officiallandings) & is.na(catch), "officiallandings", catchtype),
           catch      = ifelse(!is.na(officiallandings) & is.na(catch), officiallandings, catch), 
           
           adv_c      = 100 * (advicemax / lag(advicemax, n=1) - 1),
           tac_c      = 100 * (tac /lag(tac, n=1) - 1),
           cat_c       = 100 * (catch / lag(catch, n=1) -1)
    ) %>% 
    
    select(stockkeylabel, tacyear, advicebasis, purpose, advice, fadvmax, advicemax, tac, catch, 
           adv_c, tac_c, cat_c,
           advicetype, tactype, catchtype) 
  
  d %>% 
    select("stockkeylabel", "tacyear", "advicebasis", "purpose", "fadvmax", "advicemax", "tac", "catch") %>%
    arrange(tacyear, desc(purpose)) %>% 
    pandoc.table(., 
                 style        = "simple",
                 split.tables = 200, 
                 justify      = "right",
                 missing      =" ",
                 big.mark     = '', 
                 round        = c(0,0,0,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
}