intended.students  <- function(schoolId, school2building, field) {
  
  bIds <- school2building[school2building$SCHOOL_ID == schoolId,]$BUILDING_ID;
  blds <- buildings[buildings$ID %in% bIds,]
  
  intended.students <- sum(blds$STUDENTS/blds[[field]]);
  intended.students;
}

freqhist.economic  <- function(schools, field, include.zero = TRUE) {
  
  colors <- c("#730000", "#e60000", "#55ff00", "#38a800", "#1b5000");
  labels <- c("0", "0-500", "500-800", "800-1000", ">1000");
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
  
  freqhist.economic <- ggplot(schools.to.graph, aes(x = STUDENTS_FACTOR)) +
    geom_histogram(aes(fill=STUDENTS_FACTOR), binwidth = 1) +
    scale_x_discrete(name="\nСреднее число обучающихся в школе", labels=labels, breaks = labels, limits= labels) +
    scale_y_continuous(name="Количество школ\n", breaks=seq(0, 800, by = 100), limits=c(0, 810)) +
    scale_fill_manual("", breaks=levels(schools.to.graph$STUDENTS_FACTOR), values=colors$COLOR) +
    stat_bin(binwidth=1, geom="text", aes(label=(..count..)), size=3.5, colour="#4C4646", fontface="bold", vjust=-0.5) +
    theme(legend.position="none") +
    ggtitle(comment(schools[[field]]));
  
  freqhist.economic;
}