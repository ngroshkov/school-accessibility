freqhist.choise  <- function(buildings, field) {
  buildings$STUDENTS <- buildings$STUDENTS/1000;
  
  colors <- data.frame(ID=c(0:19), COLOR = c("#c2523c", "#cc5e33", "#d97529", "#e68e1c", "#eda813", 
                                             "#f2c00c", "#f7d707", "#fcf403", "#c6f700", "#7bed00",
                                             "#35e300", "#02d609", "#0ec441", "#17b36d", "#1e9e84", 
                                             "#1e9094", "#166d8a", "#114d82", "#0c2f7a", "#081e4e"))
  colors <- colors[colors$ID %in% unique(buildings[[field]]),]
  
  freqhist.choise <- ggplot(buildings, aes_string(x=field, weight = "STUDENTS")) +
    geom_histogram(fill=colors$COLOR, binwidth = 1) +
    scale_x_discrete(name="Количество школ", expand = c(0,2), limits=c(0:19)) +
<<<<<<< HEAD
<<<<<<< HEAD
    scale_y_continuous(name="Количество школьников\n(тыс. чел.)", breaks=seq(0, 325, by = 25), limits=c(0, 325)) +
=======
    scale_y_continuous(name="Количество школьников\n(тыс. чел.)", breaks=seq(0, 325, by = 25), limits=c(0, 340)) +
>>>>>>> f717f8aa8e9447e084c08f080993494f5cc15060
=======
    scale_y_continuous(name="Количество школьников\n(тыс. чел.)", breaks=seq(0, 325, by = 25), limits=c(0, 340)) +
>>>>>>> f717f8aa8e9447e084c08f080993494f5cc15060
    stat_bin(binwidth=1, geom="text", aes(label=round(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
    theme(legend.position="none") +
    ggtitle(comment(buildings[[field]]));
  
  freqhist.choise;  
}