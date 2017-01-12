intended.students  <- function(schoolId, school2building, field) {
  
  bIds <- school2building[school2building$SCHOOL_ID == schoolId,]$BUILDING_ID;
  blds <- buildings[buildings$ID %in% bIds,]
  
  intended.students <- sum(blds$STUDENTS/blds[[field]]);
  intended.students;
}

freqhist.economic  <- function(schools, field, include.zero = TRUE, show.titles = TRUE) {
  
  colors <- c("#730000", "#e60000", "#55ff00", "#38a800", "#1b5000");
  labels <- c("0", "0–500", "500–800", "800–1000", ">1000");
  colors <- data.frame(ID=labels, COLOR = colors);
  colors$ID <- as.character(colors$ID);
  colors$COLOR <- as.character(colors$COLOR);
  
  schools$STUDENTS_FACTOR <- cut(schools[[field]], 
                                 breaks=c(-1, 0, 500, 800, 1000, 10000), 
                                 labels=labels);
  schools.to.graph <- schools[!is.na(schools[[field]]),];
  if (!include.zero) {
    schools.to.graph <- schools.to.graph[schools.to.graph[[field]] != 0,]
  }
  colors <- colors[colors$ID %in% unique(as.character(schools.to.graph$STUDENTS_FACTOR)),]
  
  x.axis.name <- "\nСреднее число обучающихся в школе"
  y.axis.name <- "Количество школ\n"
  title <- comment(schools[[field]])
  
  if (!show.titles) {
    x.axis.name <- ""
    y.axis.name <- ""
    title <- ""
  }
  
  freqhist.economic <- ggplot(schools.to.graph, aes(x = STUDENTS_FACTOR)) +
    geom_bar(fill=colors$COLOR, width = 0.8) +
    scale_x_discrete(name=x.axis.name, labels=labels, breaks = labels, limits= labels) +
    scale_y_continuous(name=y.axis.name, breaks=seq(0, 800, by = 100), limits=c(0, 860)) +
    scale_fill_manual("", breaks=levels(schools.to.graph$STUDENTS_FACTOR), values=colors$COLOR) +
    geom_text(stat="count", aes(label=round(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
    theme(legend.position="none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
        strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
        plot.margin=unit(c(0,0,0,0),"cm"))  
  #  + ggtitle(title);
  
  freqhist.economic;
}

