dist.to.center  <- function(schools, school.blocks) {
  
  center <- c(37.61981, 55.75371);
  
  dist.to.center <- data.frame();
  for (sIdx in 1:nrow(schools)) {
    sId <- schools$ID[sIdx];
    
    sb <- school.blocks[school.blocks$SCHOOL_ID == sId,];
    dists <- integer();
    for(sbIdx in 1:nrow(sb)) {
      bp <- c(sb$POINT_X[sbIdx],sb$POINT_Y[sbIdx]);
      
      dist <- distVincentyEllipsoid(center, bp);
      dists <- c(dists, dist);
    }
    dist <- min(dists);
    
    dist.to.center <- rbind(dist.to.center, data.frame(ID=sId, DIST_TO_CENTER=dist));
  }
  
  dist.to.center;
}

scat.dist.to.rating  <- function(schools, dist.field, rating.field, show.titles = TRUE) {
  not.in.rat <- max(schools[!is.na(schools[[rating.field]]),][[rating.field]]) + 10;
  schools$rat <- sapply(schools[[rating.field]], {function(x) ifelse(is.na(x), not.in.rat, x)});
  schools$dst <- schools[[dist.field]]/1000
  colors = c("#264500", "#267300", "#38A800", "#4CE600", "#55ff00", "#FF7F7F", "#E60000", "#A80000", "#730000");

  x.axis.name <- "\nРасстояние от центра Москвы (км)"
  y.axis.name <- "\nРейтинг школы"
  
  if (!show.titles) {
    x.axis.name <- ""
    y.axis.name <- ""
  }
  
  scat.dist.to.rating <- ggplot(schools, aes_string(x="dst", y="rat")) +
    geom_point(shape = 20, aes(color=cut(rat, breaks=seq(0, 450, by = 50)))) +
    scale_colour_manual("", values=colors) +
    scale_x_continuous(name=x.axis.name, breaks=seq(0, 40, by = 5), limits=c(0,40)) +
    scale_y_continuous(name=y.axis.name, breaks=c(seq(0, 410, by = 50), 410), 
                       limits=c(0, 410), labels=c("1","50","100","150","200","250","300","350","400","Вне рейтинга")) +
    theme_minimal() +
    theme(legend.position="none")
  scat.dist.to.rating;
}

scat.catch.to.rating  <- function(schools, students.field, rating.field, show.titles = TRUE) {
  not.in.rat <- max(schools[!is.na(schools[[rating.field]]),][[rating.field]]) + 10;
  schools$rat <- sapply(schools[[rating.field]], {function(x) ifelse(is.na(x), not.in.rat, x)});
  
  colors = c("#264500", "#267300", "#38A800", "#4CE600", "#55ff00", "#FF7F7F", "#E60000", "#A80000", "#730000");

  x.axis.name <- "\nКоличество обслуживаемых школьников"
  y.axis.name <- "\nРейтинг школы"
  
  if (!show.titles) {
    x.axis.name <- ""
    y.axis.name <- ""
  }
  scat.catch.to.rating <- ggplot(schools, aes_string(x=students.field, y="rat")) +
    geom_point(shape = 20, aes(color=cut(rat, breaks=seq(0, 450, by = 50)))) +
    scale_colour_manual("", values=colors) +
    scale_x_continuous(name=x.axis.name, breaks=seq(0, 2500, by = 500), limits=c(0,2500)) +
    scale_y_continuous(name=y.axis.name, breaks= c(seq(0, 410, by = 50), 410), 
                       limits=c(0, 410), labels=c("1","50","100","150","200","250","300","350","400","Вне рейтинга")) +
    #geom_smooth(method=lm) +
    theme_minimal() +
    theme(legend.position="none")
  scat.catch.to.rating;
}