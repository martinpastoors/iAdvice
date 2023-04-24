# ===================================================================================================
# iAssess plot and table functions
#
# 30/03/2020 separate file for plot and table functions
# ===================================================================================================

# stock="mac-nea"
# assessyear=2020
# firstyear=2000
# lastyear=2020
# plot = c("stocksize", "fishingpressure", "recruitment")
# my.purpose="advice"

# Function to plot single assessment
plot_assess <- function(stock,assessyear, firstyear, lastyear, 
                        my.purpose = "advice", plot=c("stocksize","fishingpressure")) {
  
  d <-
    iAssess %>% 
    ungroup() %>% 
    
    filter(stockkeylabelold %in% stock | stockkeylabel %in% stock) %>% 
    filter(assessmentyear   == assessyear) %>% 
    filter(year             %in% firstyear:lastyear) %>% 
    filter(purpose %in% my.purpose) %>% 
    
    #set type
    mutate(
      type = ifelse(purpose == "advice"      , "last"    , NA),
      type = ifelse(grepl("bench",purpose)   ,  "benchmark", type),
      type = ifelse(grepl("replaced",purpose), "replaced"  , type),
      type = ifelse(is.na(type), "other", type)) %>%
    
    mutate(tyear     = substr(as.character(assessmentyear), 3,4)) %>% 
    dplyr::select(stockkeylabel, stockkeylabelold, stockdescription, type, 
                  assessmentyear, year,
                  recruitment, lowrecruitment, highrecruitment, unitofrecruitment,
                  stocksize, lowstocksize, highstocksize, stocksizeunits,
                  fishingpressure, lowfishingpressure, highfishingpressure, fishingpressureunits,
                  landings, discards, catches, catcheslandingsunits) %>% 
    
    # convert to long data
    tidyr::pivot_longer(names_to="variable", 
                        values_to="data", 
                        c(recruitment, lowrecruitment, highrecruitment, 
                          stocksize, lowstocksize, highstocksize,
                          fishingpressure, lowfishingpressure, highfishingpressure, 
                          landings, discards, catches)) %>% 
    
    # set appropriate units and remove old units columns
    mutate(
      unit = ifelse(grepl("recruitment", variable), unitofrecruitment, NA),
      unit = ifelse(grepl("stocksize"  , variable), stocksizeunits, unit),
      unit = ifelse(grepl("fishing"    , variable), fishingpressureunits, unit),
      unit = ifelse(grepl("landing|discard|catch", variable), catcheslandingsunits, unit) ) %>% 
    dplyr::select(-unitofrecruitment, - stocksizeunits, -fishingpressureunits, -catcheslandingsunits) %>% 
    
    # separate estimate, low and high
    mutate(
      esttype = ifelse(grepl("low", variable), "low", NA),
      esttype = ifelse(grepl("high", variable), "high", esttype),
      esttype = ifelse(is.na(esttype), "est", esttype) ) %>% 
    mutate(
      variable = gsub("low|high","", variable) ) %>% 
    
    # select variables to plot
    filter(variable %in% plot) %>% 
    mutate(variable = factor(variable, levels=plot, ordered=TRUE)) %>% 
    
    # spread to make separate columns for est, low and high
    tidyr::pivot_wider(names_from="esttype", values_from="data")
  
  # Reference points
  rp <-
    iAdvice %>% 
    ungroup() %>% 
    filter(purpose %in% my.purpose) %>% 
    filter(stockkeylabelold %in% stock | stockkeylabel %in% stock) %>% 
    filter(assessmentyear   == assessyear) %>% 
    dplyr::select(stockkeylabel, stockkeylabelold, assessmentyear,
                  blim, bpa, msybtrigger, flim, fpa, fmsy) %>%
    gather(key=refpoint, value=data, blim:fmsy) %>% 
    
    # generate variables
    mutate(
      variable = ifelse(refpoint %in% c("blim","bpa","msybtrigger"), "stocksize", NA),
      variable = ifelse(refpoint %in% c("flim", "fpa", "fmsy"), "fishingpressure", variable) ) %>% 
    mutate(variable = factor(variable, levels=plot, ordered=TRUE)) %>% 
    
    # generate refpoint types
    mutate(
      reftype = ifelse(refpoint %in% c("blim","flim"), "lim", NA),
      reftype = ifelse(refpoint %in% c("bpa", "fpa") , "pa", reftype),
      reftype = ifelse(refpoint %in% c("msybtrigger", "fmsy"), "msy", reftype))  %>% 
    dplyr::select(-refpoint)
  
  
  # Titles
  tt <-
    d %>% 
    distinct(variable, unit, stockkeylabel, stockdescription) %>%
    filter(!is.na(unit))
  
  # d %>% View()
  
  # plot 
  d %>% 
    
    ggplot(aes(x=year,y=est)) +
    
    theme_publication() +
    
    theme(legend.title=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
          axis.text.y = element_text(size=9),
          plot.title  = element_text(size=10, hjust=0), 
          plot.margin = margin(2, 2, 2, 2, "mm"),
          # strip.background = element_blank(),
          legend.position = "null") +
    
    geom_ribbon(aes(x=year, ymin=low, ymax=high, fill=type), alpha=0.2) +
    
    geom_line(aes(colour = type, size=type, linetype=type) ) +
    
    # geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2, fill="red") +
    # geom_line(colour="red" ) +
    
    # plot limits
    geom_hline(data=filter(rp, reftype %in% "lim"), aes(yintercept=data), 
               colour="red", linetype="solid", inherit.aes=FALSE) +
    geom_text (data=filter(rp, reftype %in% "lim"), aes(x=firstyear, y=data), 
               label = "lim", vjust = -0.3, hjust = 0, colour="red", inherit.aes=FALSE) +
    
    # plot msy
    geom_hline(data=filter(rp, reftype %in% "msy"), aes(yintercept=data), 
               colour="red", linetype="dashed", inherit.aes=FALSE) +
    geom_text (data=filter(rp, reftype %in% "msy"), aes(x=firstyear, y=data), 
               label = "msy", vjust = -0.3, hjust = 0, colour="red", inherit.aes=FALSE) +
    
    scale_colour_manual  (
      values=c(last ="red",last_bench="blue",advice="black",
               benchmark="blue",replaced="green",other="gray80")) +
    scale_fill_manual    (
      values=c(last = "red",last_bench="blue",advice="black",
               benchmark="blue",replaced="green",other="gray80")) +
    scale_linetype_manual(
      values=c(last="solid",last_bench="solid",advice="solid",
               benchmark="solid",replaced="solid",other="solid")) +
    scale_size_manual    (
      values=c(last= 1.2,last_bench=1.2,advice=0.8,
               benchmark=0.8,replaced=0.8,other=0.8)) +
    
    expand_limits(y = 0) +
    xlim(firstyear,lastyear) +
    labs(x = NULL, y = NULL, 
         title = paste0(unique(tt$stockdescription), " (", unique(tt$stockkeylabel),")") ) +
    facet_wrap(~variable, scales="free_y")
  
  
  
} # End of plot_assess function

# s   <- "mac-nea"
# plot_assess(stock=s,assessyear=lay, firstyear=fy, lastyear=ly, 
#             plot = c("stocksize", "fishingpressure", "recruitment"), 
#             my.purpose="advice")
# plot_assess(stock=s,assessyear=lay, firstyear=fy, lastyear=ly, 
#             plot = c("stocksize", "fishingpressure", "recruitment"), 
#             my.purpose="bench") 


# stock="her-nirs"
# firstassessyear=fay; lastassessyear=lay; firstyear=fy;lastyear=ly;
# plot = c("stocksize", "fishingpressure")
# include.benchmark=FALSE; include.replaced=FALSE
# plot.uncertainty=FALSE
# plot.relative=FALSE

# stock="mac-nea"
# firstassessyear=2015
# lastassessyear=2020 
# firstyear=2000
# lastyear=2019 
# include.benchmark = TRUE
# include.replaced=TRUE 
# plot=c("stocksize")
# plot.uncertainty=TRUE
# plot.relative=FALSE
  
# Function to plot historical assessments
plot_history <- function(
     stock,firstassessyear,lastassessyear, firstyear, lastyear, 
     include.benchmark = TRUE, 
     include.replaced=TRUE, 
     plot=c("stocksize","fishingpressure"),
     plot.uncertainty=FALSE,
     plot.relative=FALSE,
     plot.title=TRUE,
     stockkeylabeltype="stockkeylabelold") {
  
  d <-
    iAssess %>% 
    ungroup() %>% 
    
    {if (include.benchmark == TRUE & include.replaced == TRUE) {
      filter(., purpose %in% c("advice","benchmark", "interbenchmark", "bench", "replaced"))
    } else if (include.benchmark == TRUE & include.replaced == FALSE) {
      filter(., purpose %in% c("advice","benchmark", "interbenchmark", "bench"))
    } else if (include.benchmark == FALSE & include.replaced == TRUE) {
      filter(., purpose %in% c("advice","replaced"))
    } else {
      filter(., purpose %in% c("advice")) 
    }} %>%
    
    filter(stockkeylabelold %in% stock | stockkeylabel %in% stock) %>% 
    filter(assessmentyear   %in% c(firstassessyear:lastassessyear)) %>% 
    filter(year             %in% firstyear:lastyear) %>% 
    
    group_by(stockkeylabel,  stockkeylabelnew, stockkeylabelold, assessmentyear) %>% 
    
    #set type
    mutate(
      type = ifelse(assessmentyear == lastassessyear & purpose == "advice"      , "last"      , NA),
      type = ifelse(assessmentyear == lastassessyear & grepl("bench",purpose) & 
                      n_distinct(purpose)==1, "last_bench", type),
      type = ifelse(assessmentyear == lastassessyear & grepl("bench",purpose) & 
                      n_distinct(purpose) > 1, "bench", type),
      type = ifelse(assessmentyear != lastassessyear & purpose == "advice"      , "advice"    , type),
      type = ifelse(assessmentyear != lastassessyear & grepl("bench",purpose)   ,  "benchmark", type),
      type = ifelse(assessmentyear != lastassessyear & grepl("replaced",purpose), "replaced"  , type),
      type = ifelse(is.na(type), "other", type)) %>%
    
    ungroup() %>% 
    
    # set tyear and group
    mutate(tyear = substr(as.character(assessmentyear), 3,4)) %>% 
    mutate(group = paste0(tyear, type)) %>% 
    mutate(group = substr(group, 1,3)) %>% 
    mutate(group = gsub("a|l","", group)) %>% 
    
    dplyr::select(stockkeylabel,  stockkeylabelnew, stockkeylabelold, stockdescription, 
                  assessmentyear, year, tyear, group, type, purpose,
                  recruitment, lowrecruitment, highrecruitment, unitofrecruitment,
                  stocksize, lowstocksize, highstocksize, stocksizeunits,
                  fishingpressure, lowfishingpressure, highfishingpressure, fishingpressureunits,
                  landings, discards, catches, catcheslandingsunits) %>% 
    
    # convert to long data
    tidyr::pivot_longer(names_to="variable", values_to="data", 
           c(recruitment, lowrecruitment, highrecruitment, 
             stocksize, lowstocksize, highstocksize,
             fishingpressure, lowfishingpressure, highfishingpressure, 
             landings, discards, catches)) %>% 
    
    # set appropriate units and remove old units columns
    mutate(
      unit = ifelse(grepl("recruitment", variable), unitofrecruitment, NA),
      unit = ifelse(grepl("stocksize"  , variable), stocksizeunits, unit),
      unit = ifelse(grepl("fishing"    , variable), fishingpressureunits, unit),
      unit = ifelse(grepl("landing|discard|catch", variable), catcheslandingsunits, unit) ) %>% 
    dplyr::select(-unitofrecruitment, - stocksizeunits, -fishingpressureunits, -catcheslandingsunits) %>% 
    
    # separate estimate, low and high
    mutate(
      esttype = ifelse(grepl("low", variable), "low", NA),
      esttype = ifelse(grepl("high", variable), "high", esttype),
      esttype = ifelse(is.na(esttype), "est", esttype) ) %>% 
    mutate(
      variable = gsub("low|high","", variable) ) %>% 
    
    # select variables to plot
    filter(variable %in% plot) %>% 
    mutate(variable = factor(variable, levels=plot, ordered=TRUE)) %>% 
    group_by(stockkeylabel,  stockkeylabelnew, stockkeylabelold, stockdescription, assessmentyear, tyear, group, type, purpose, 
             variable, esttype) %>% 
  
    # make relative if needed
    {if (plot.relative==TRUE) mutate(., data = data / mean(data, na.rm=TRUE)) else . } %>% 
  
    # spread to make separate columns for est, low and high
    tidyr::pivot_wider(names_from="esttype", values_from="data") %>% 
    
    # remove NA
    filter(!is.na(est)) %>% 
    ungroup()
  
  # Check
  # d %>% filter(assessmentyear == 2019) %>% View()
  
  # Reference points
  rp <-
    iAdvice %>% 
    ungroup() %>% 
    filter(purpose %in% "advice") %>% 
    filter(stockkeylabelold %in% stock | stockkeylabel %in% stock) %>% 
    filter(assessmentyear   == lastassessyear) %>% 
    dplyr::select(stockkeylabel,  stockkeylabelnew, stockkeylabelold, assessmentyear,
                  blim, bpa, msybtrigger, flim, fpa, fmsy) %>%
    gather(key=refpoint, value=data, blim:fmsy) %>% 
    
    # generate variables
    mutate(
      variable = ifelse(refpoint %in% c("blim","bpa","msybtrigger"), "stocksize", NA),
      variable = ifelse(refpoint %in% c("flim", "fpa", "fmsy"), "fishingpressure", variable) ) %>% 
    filter(variable %in% plot) %>% 
    mutate(variable = factor(variable, levels=plot, ordered=TRUE)) %>% 
    
    # generate refpoint types
    mutate(
      reftype = ifelse(refpoint %in% c("blim","flim"), "lim", NA),
      reftype = ifelse(refpoint %in% c("bpa", "fpa") , "pa", reftype),
      reftype = ifelse(refpoint %in% c("msybtrigger", "fmsy"), "msy", reftype))  %>% 
    dplyr::select(-refpoint)
  
  # Titles
  tt <-
    d %>% 
    distinct(variable, unit, stockkeylabel,  stockkeylabelnew, stockkeylabelold, stockdescription) %>%
    filter(!is.na(unit))
  
  # plot
  d %>% 
    
    ggplot(aes(year,est, group=group)) +
    
    theme_publication() +
    
    theme(legend.title=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
          axis.text.y = element_text(size=9),
          plot.title  = element_text(size=10, hjust=0), 
          plot.margin = margin(2, 2, 2, 2, "mm"),
          # strip.background = element_blank(),
          legend.position = "null") +
    
    {if (plot.uncertainty)     
      geom_ribbon(data=filter(d, type=="last"),
                  aes(x=year, ymin=low, ymax=high, fill=type), alpha=0.2) } +
    
    geom_line(aes(colour = type, size=type, linetype=type) ) +
    
    geom_dl(aes(label  = group, colour = type, group=group), 
            method = list(dl.combine("last.points"), cex = 0.8, inherit.aes=FALSE)) +
    
    # plot limits
    {if(plot.relative==FALSE) geom_hline(data=filter(rp, reftype %in% "lim"), aes(yintercept=data), 
                                         colour="red", linetype="solid", inherit.aes=FALSE) } +     
    {if(plot.relative==FALSE) geom_text (data=filter(rp, reftype %in% "lim"), aes(x=firstyear, y=data), 
                                         label = "lim", vjust = -0.3, hjust = 0, colour="red", inherit.aes=FALSE) } +     

      # plot msy
    {if(plot.relative==FALSE) geom_hline(data=filter(rp, reftype %in% "msy"), aes(yintercept=data), 
                                         colour="red", linetype="dashed", inherit.aes=FALSE) } +     
    {if(plot.relative==FALSE) geom_text (data=filter(rp, reftype %in% "msy"), aes(x=firstyear, y=data), 
                                         label = "msy", vjust = -0.3, hjust = 0, colour="red", inherit.aes=FALSE) } +     
    scale_colour_manual  (
      values=c(last ="red",last_bench="blue",advice="black",
               benchmark="blue",replaced="green",other="gray80")) +
    scale_fill_manual    (
      values=c(last = "red",last_bench="blue",advice="black",
               benchmark="blue",replaced="green",other="gray80")) +
    scale_linetype_manual(
      values=c(last="solid",last_bench="solid",advice="solid",
               benchmark="solid",replaced="solid",other="solid")) +
    scale_size_manual    (
      values=c(last= 1.2,last_bench=1.2,advice=0.8,
               benchmark=0.8,replaced=0.8,other=0.8)) +
    
    expand_limits(y = 0) +
    xlim(firstyear,lastyear) +
    {if (plot.title==TRUE) {
      labs(x = NULL, y = NULL, title = paste0(unique(tt$stockdescription), " (", unique(tt$stockkeylabel),")") )} 
     else {
       labs(x = NULL, y = NULL, title = "")   
      } } +
    
    {if (length(stock)==1) {
      facet_wrap(~variable, scales="free_y")      
    } else if (length(stock)>1 & length(plot)==1) {
      facet_wrap(~get(stockkeylabeltype), scales="free_y")       
    } else {
      facet_grid(rows=variable, cols= get(stockkeylabeltype), scales="free_y")
    }}

  # return(d)
  
} # End of plot_history function


# firstadviceyear <- 2016; lastadviceyear  <- 2020; stock<-c("mac-nea"); include.replaced <- TRUE

# stock="whb-comb"
# firstadviceyear=2001
# lastadviceyear=2021
# include.replaced = FALSE

# Function to plot advice overviews
plot_advice <- function(stock,firstadviceyear,lastadviceyear, include.replaced=FALSE) {
  
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

    mutate(ToA = tac/advicemax-1,
           CoA = catch/advicemax-1) %>% 
    
    dplyr::select(stockkeylabel, tacyear, advicebasis, purpose, advice=advicemax, tac, catch, 
           adv_c, tac_c, cat_c,
           ToA, CoA) %>% 
    
    tidyr::pivot_longer(names_to = "variable", values_to = "data", advice:CoA)
  
  t1 <-
    d %>% 
    filter(variable %in% c("advice","tac","catch")) %>% 
    filter(!is.na(data)) %>% 
    group_by(variable) %>% 
    filter(tacyear == max(tacyear, na.rm=TRUE))

  p1 <-
    d %>% 
    filter(variable %in% c("advice","tac","catch")) %>% 
    
    ggplot(aes(x=tacyear,y=data, group=variable)) +
    theme_publication() +
    theme(legend.position = "none") +
    ggrepel::geom_label_repel(data=t1, 
                              aes(x=tacyear, y=data, label=variable, colour=variable),
                              inherit.aes = FALSE,
                              nudge_x = 1, 
                              segment.alpha= 0.5,
                              show.legend=FALSE) +
    geom_line(aes(colour=variable), size=1) +
    expand_limits(y = 0) +
    labs(x="year", y="tonnes")

  t2 <-
    d %>% 
    filter(variable %in% c("ToA","CoA")) %>% 
    filter(!is.na(data)) %>% 
    group_by(variable) %>% 
    filter(tacyear == max(tacyear, na.rm=TRUE))
  
  p2 <-
    d %>% 
    filter(variable %in% c("ToA","CoA")) %>% 
    
    ggplot(aes(x=tacyear,y=data, group=variable)) +
    theme_publication() +
    theme(legend.position = "none") +
    ggrepel::geom_label_repel(data=t2, 
                              aes(x=tacyear, y=data, label=variable, colour=variable),
                              inherit.aes = FALSE,
                              nudge_x = 1, 
                              segment.alpha= 0.5,
                              show.legend=FALSE) +
    geom_line(aes(colour=variable), size=1) +
    geom_hline(yintercept=0, colour="black", linetype="dashed") +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::percent) +
    labs(x="year", y="percentage over advice")
  
  plot_grid(p1 + theme(axis.title.x = element_blank()), 
            p2 + theme(axis.title.x = element_blank()),
            ncol=1, align = 'hv', rel_heights = c(3,3))
  
}

# plot_advice(stock=s,firstadviceyear=fay+1,lastadviceyear=lay+1, include.replaced = TRUE)

# stock=s;firstadviceyear=fay+1;lastadviceyear=lay+1;include.replaced = TRUE; output.df=TRUE

# Function to plot advice overviews
table_advice <- function(stock,
                         firstadviceyear,
                         lastadviceyear, 
                         include.replaced=FALSE, 
                         output.df=FALSE) {
  
  d <-
    iAdvice %>% 
    
    filter(stockkeylabelold %in% stock | stockkeylabel %in% stock) %>% 
    
    {if (include.replaced == TRUE) {
      filter(., purpose %in% c("advice","replaced"))
    } else {
      filter(., purpose %in% c("advice")) 
    }} %>%
    
    mutate(advicebasis= substr(advicebasis,1,25),
           
           advice     = ifelse(!is.na(advisedcatchmax), advisedcatchmax, advisedlandingsmax),
           # advicemax  = ifelse(!is.na(advisedcatchmax), advisedcatchmax, advisedlandingsmax),
           advicetype = ifelse(!is.na(advisedcatchmax), "catch", "landings"),
           
           tactype    = ifelse(!is.na(tac), "catch", "landings"), 
           tac        = ifelse(!is.na(tac), tac, tal),
           
           catchtype  = ifelse(!is.na(catches), "catches", ""),
           catch      = ifelse(!is.na(catches), catches, NA),
           
           catchtype  = ifelse(!is.na(landings) & is.na(catch), "landings", catchtype),
           catch      = ifelse(!is.na(landings) & is.na(catch), landings, catch),
           
           catchtype  = ifelse(!is.na(officiallandings) & is.na(landings) & is.na(catch), "officiallandings", catchtype),
           catch      = ifelse(!is.na(officiallandings) & is.na(landings) & is.na(catch), officiallandings, catch), 
           
           adv_c      = 100 * (advice / lag(advice, n=1) - 1),
           tac_c      = 100 * (tac    / lag(tac, n=1) - 1),
           cat_c      = 100 * (catch  / lag(catch, n=1) -1),
           uni_c      = 100 * (unilateralquota  / lag(unilateralquota, n=1) -1)
    ) %>% 
    
    filter(tacyear %in% firstadviceyear:lastadviceyear) %>% 
    
    select(stockkeylabel, stockkeylabelold, tacyear, advicebasis, purpose, advice, fadvmax, tac, unilateralquota, catch, 
           adv_c, tac_c, cat_c, uni_c,
           advicetype, tactype, catchtype) 
  
  if (output.df == FALSE) {
    d %>% 
      select("stockkeylabel", "tacyear", "advicebasis", "purpose", "fadvmax", "advice", "tac", "catch") %>%
      arrange(tacyear, desc(purpose)) %>% 
      pandoc.table(., 
                   style        = "simple",
                   split.tables = 200, 
                   justify      = "right",
                   missing      =" ",
                   big.mark     = '', 
                   round        = c(0,0,0,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
    
  } else {
    return(d)
  }
}

# table_advice(stock=s,firstadviceyear=fay+1,lastadviceyear=lay+1, include.replaced = TRUE)
