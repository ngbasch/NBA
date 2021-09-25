
TransparentImage<-function(img, amount){

#img <- image_read(url)
bitmap <- img[[1]]
bitmap[4,,] <- as.raw(as.integer(bitmap[4,,]) * amount)
img <- image_read(bitmap)

return(img)
}

FormatTodaysInfo<-function(df){
  df%>%
    group_by(idGame)%>%
    mutate_at(vars(contains("ratio")), funs(round(.,2)))%>%
    mutate_at(vars(contains("consensus")), funs(paste0(100*., "%")))%>%
    mutate(Team = paste0(sbrTeam, "<br/>(", sbrTeam[home == "Away"],")"),
           ratio_ml = paste0(ratio_ml, "<br/>(", ratio_ml[home == "Away"],")"),
           ratio_rl = paste0(ratio_rl, "<br/>(", ratio_rl[home == "Away"],")"),
           ratio_to = paste0("U:",ratio_to, "<br/>(O:", ratio_to[home == "Away"],")"),
           consensus_ml = paste0(consensus_ml, "<br/>(", consensus_ml[home == "Away"],")"),
           consensus_rl = paste0(consensus_rl, "<br/>(", consensus_rl[home == "Away"],")"),
           consensus_to = paste0("U:",consensus_to, "<br/>(O:", consensus_to[home == "Away"],")")
    )%>%
    filter(home == "Home")%>%
    ungroup()%>%
    select(Team, ratio_ml, consensus_ml, "Spread" = sbr_spread,ratio_rl, consensus_rl, "Total" = sbr_total, ratio_to, consensus_to)
  
}

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, 
                      scale = scale, 
                      draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...)
  )
}
GeomSplitViolin <- ggproto(
  "GeomSplitViolin", 
  GeomViolin, 
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, 
                      xminv = x - violinwidth * (x - xmin), 
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv), 
      if(grp%%2==1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", 
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_pannel(newdata, ...))
    }
  }
)

CleanTotals<-function(df,include_push = FALSE){
    df%>%
      distinct(idGame,dateGame, yearSeason, sbr_total, ptsTotal,dif_total)%>%
      mutate(result = ifelse(ptsTotal < sbr_total, "Under", 
                           ifelse(ptsTotal>sbr_total, "Over", 
                                  ifelse(include_push, "Push", NA))))%>%
      mutate(result_count = ifelse(result == "Over", 1,
                                 ifelse(result == "Under",-1,NA)))
    
}
  
  
  
#CleanSpread<-
  
  
#CleanML<-
  

# url_input<-"C:/Users/nated/Documents/Documents_NB/Projects/Betting/SBR Workflow/Input/Logos/heat.png"
# z<-TransparentImage(image_read(url_input), 0.2)
# 
# x<-image_read(url_input)
# 
# grid.newpage()
# grid.draw(rasterGrob(width=1, height=1, image=z%>%mutate(colorspace = "red")))

