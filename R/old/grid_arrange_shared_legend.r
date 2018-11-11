# ----------------------------------------------------------------------------------------------
# grid_arrange_shared_legend.r 
# 
# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# 24/3/2017
# ----------------------------------------------------------------------------------------------

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

  require(ggplot2)
  require(gridExtra)
  require(grid)
  
  plots    <- list(...)
  position <- match.arg(position)
  g        <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend   <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight  <- sum(legend$height)
  lwidth   <- sum(legend$width)
  gl       <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl       <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


# dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
# 
# p1 <- qplot(carat, price, data = dsamp, colour = clarity)
# p2 <- qplot(cut, price, data = dsamp, colour = clarity)
# p3 <- qplot(color, price, data = dsamp, colour = clarity)
# p4 <- qplot(depth, price, data = dsamp, colour = clarity)
# 
# grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
# grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)
