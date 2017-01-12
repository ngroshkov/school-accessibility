freqhist.choise  <- function(buildings, field, show.titles = TRUE, maxval = 7) {
  buildings$STUDENTS <- buildings$STUDENTS/1000;
  buildings[[field]][buildings[[field]] >= maxval] <- maxval
#  colors <- data.frame(ID=c(0:19), COLOR = c("#c2523c", "#cc5e33", "#d97529", "#e68e1c", "#eda813", 
#                                             "#f2c00c", "#f7d707", "#fcf403", "#c6f700", "#7bed00",
#                                             "#35e300", "#02d609", "#0ec441", "#17b36d", "#1e9e84", 
#                                             "#1e9094", "#166d8a", "#114d82", "#0c2f7a", "#081e4e"))
#  colors <- data.frame(ID=c(0:14), COLOR = c("#c2523c", "#cc5e33", "#d97529", "#e68e1c", 
#                                             "#f2c00c", "#f7d707", "#fcf403", "#c6f700",
#                                             "#35e300", "#02d609", "#0ec441", "#17b36d", 
#                                             "#1e9094", "#166d8a", "#114d82"))
#  colors <- data.frame(ID=c(0:14), COLOR = c("#c2523c",  
#                                                        "#f7d707", "#fcf403", "#c6f700", "#7bed00",
#                                             "#35e300", "#02d609", "#0ec441", "#17b36d", "#1e9e84", 
#                                             "#1e9094", "#166d8a", "#114d82", "#0c2f7a", "#081e4e"))
  colors <- data.frame(ID=c(0:7), COLOR = c( "#e60000", "#b4f7b0", "#9ef296", "#87eb7c", 
                                             "#6ce362", "#55db48", "#37d42f", "#0084a8"))
  colors <- colors[colors$ID %in% unique(buildings[[field]]),]
  
  x.axis.name <- "\nКоличество доступных школ"
  y.axis.name <- "\nКоличество школьников\n(тыс. чел.)"
  title <- comment(buildings[[field]])
  
  if (!show.titles) {
    x.axis.name <- ''
    y.axis.name <- ''
    title <- ''
  }
  
  freqhist.choise <- ggplot(buildings, aes_string(x=field, weight = "STUDENTS")) +
    geom_bar(fill=colors$COLOR, width = 0.8) +
    scale_x_continuous(name=x.axis.name, limits = c(-0.4, maxval + 0.4),breaks = c(0:maxval), labels=c("0","1","2","3","4","5","6",">7")) +
    scale_y_continuous(name=y.axis.name, expand = c(0:10), breaks=seq(0, 900, by = 200), limits=c(0, 1000)) +
    geom_text(stat="count", aes(label=round(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
    theme(legend.position="none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_blank(), axis.ticks = element_blank(), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
          plot.margin=unit(c(0,0,0,0),"cm"))
  # + ggtitle(title);
  
  freqhist.choise;  
}