freqhist.choise  <- function(buildings, field, show.titles = TRUE) {
  buildings$STUDENTS <- buildings$STUDENTS/1000;
  
#  colors <- data.frame(ID=c(0:19), COLOR = c("#c2523c", "#cc5e33", "#d97529", "#e68e1c", "#eda813", 
#                                             "#f2c00c", "#f7d707", "#fcf403", "#c6f700", "#7bed00",
#                                             "#35e300", "#02d609", "#0ec441", "#17b36d", "#1e9e84", 
#                                             "#1e9094", "#166d8a", "#114d82", "#0c2f7a", "#081e4e"))
#  colors <- data.frame(ID=c(0:14), COLOR = c("#c2523c", "#cc5e33", "#d97529", "#e68e1c", 
#                                             "#f2c00c", "#f7d707", "#fcf403", "#c6f700",
#                                             "#35e300", "#02d609", "#0ec441", "#17b36d", 
#                                             "#1e9094", "#166d8a", "#114d82"))
  colors <- data.frame(ID=c(0:14), COLOR = c("#c2523c",  
                                                        "#f7d707", "#fcf403", "#c6f700", "#7bed00",
                                             "#35e300", "#02d609", "#0ec441", "#17b36d", "#1e9e84", 
                                             "#1e9094", "#166d8a", "#114d82", "#0c2f7a", "#081e4e"))  
  colors <- colors[colors$ID %in% unique(buildings[[field]]),]
  
  x.axis.name <- "\nКоличество доступных школ"
  y.axis.name <- "\nКоличество школьников\n(тыс. чел.)"
  title <- comment(buildings[[field]])
  
  if (!show.titles) {
    x.axis.name <- ""
    y.axis.name <- ""
    title <- ""
  }
  
  freqhist.choise <- ggplot(buildings, aes_string(x=field, weight = "STUDENTS")) +
  #  geom_histogram(fill=colors$COLOR, binwidth = 1) +
    geom_bar(fill=colors$COLOR, width = 1) +
  #  scale_x_discrete(name=x.axis.name, expand = c(0,2), limits=c(0:14), labels=c("","","","","","","","","","","","","","","")) +
  #  scale_y_continuous(name=y.axis.name, breaks=seq(0, 325, by = 100), limits=c(0, 340), labels=c("","","","")) +
  #  stat_bin(binwidth=1, geom="text", aes(label=round(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
    scale_x_discrete(name=x.axis.name, expand = c(0,2), limits=c(0:14)) +
    scale_y_continuous(name=y.axis.name, breaks=seq(0, 325, by = 100), limits=c(0, 340)) +
   # stat_bin(binwidth=1, geom="text", aes(label=round(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
  #  geom_text(aes(y=..count.., label=..count..), stat="count", color="white", hjust=1.0, size=3) +
  #  geom_text(binwidth=1, geom="text", aes(label=round(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
    geom_text(stat="count", aes(label=round(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
    theme(legend.position="none") +
    theme_minimal() +
    ggtitle(title);
  
  freqhist.choise;  
}