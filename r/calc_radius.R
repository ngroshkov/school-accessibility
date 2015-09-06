colors_schools_radius = c("#730000", "#A80000", "#e60000", "#ff7f7f", "#a3ff73", "#55ff00", "#4ce600", "#38a800", "#267300", "#264500");


distances = function(school_point, buildings){
  distances <- numeric();
  for (bIdx in 1:nrow(buildings)) {
    building_point <- c(buildings$POINT_X[bIdx], buildings$POINT_Y[bIdx]);
    distance <- distVincentyEllipsoid(school_point, building_point);
    distances <- c(distances, distance);
  }
  distances;
}


radiuses = function(schools, buildings){
  radiuses500 <- numeric();
  radiuses800 <- numeric();
  radiuses1000 <- numeric();
  for (sIdx in 1:nrow(schools)) {
    s <- c(schools$POINT_X[sIdx], schools$POINT_Y[sIdx]);
  
    build_distances <- distances(s, buildings);
  
    builds <- data.frame(buildings$OBJECTID_12, buildings$STUDENTS, build_distances);
    builds <- builds[order(build_distances), ];
  
    students <- 0;
    r500 <- numeric();
    r800 <- numeric();
    r1000 <- numeric();
    for (bIdx in 1:nrow(builds)) {
      students <- students + builds$buildings.STUDENTS[bIdx];
      if (students >= round(80*(500/100)) && length(r500) == 0) {
        r500 <- tail(head(builds, bIdx - 1), 1)$build_distances;
      }
      if (students >= round(80*(800/100)) && length(r800) == 0) {
        r800 <- tail(head(builds, bIdx - 1), 1)$build_distances;
      }
      if (students >= round(80*(1000/100)) && length(r1000) == 0) {
        r1000 <- tail(head(builds, bIdx - 1), 1)$build_distances;
        break;
      }
    }
    
    radiuses500 <- c(radiuses500, r500);
    radiuses800 <- c(radiuses800, r800);
    radiuses1000 <- c(radiuses1000, r1000);
    
    
    print(paste(round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
  }
  
  radiuses = data.frame(radiuses500, radiuses800, radiuses1000);
  radiuses;
}

freqhist_school_rad_for_500_students <- ggplot(schools.old, aes(x=schools.old$r500)) +
  geom_histogram(aes(fill=cut(schools.old$r500, breaks=seq(49, 999, by = 50))), colour="black", fill="white", binwidth = 25) +
  #geom_density(alpha=.2) +
  scale_x_continuous(name="Радиус доступности, школы охватывающий 500 школьников", breaks=seq(0, 1000, by = 50), limits=c(0, 1000)) +
  scale_y_continuous(name="Частота распределения", breaks=seq(0, 350, 50)) +
  theme(legend.position="none")

freqhist_school_rad_for_800_students <- ggplot(schools.old, aes(x=schools.old$r800)) +
  geom_histogram(aes(fill=cut(schools.old$r800, breaks=seq(49, 999, by = 50))), colour="black", fill="white", binwidth = 25) +
  #geom_density(alpha=.2) +
  scale_x_continuous(name="Радиус доступности, школы охватывающий 800 школьников", breaks=seq(0, 1000, by = 50), limits=c(0, 1000)) +
  scale_y_continuous(name="Частота распределения", breaks=seq(0, 350, 50)) +
  theme(legend.position="none")

freqhist_school_rad_for_1000_students <- ggplot(schools.old, aes(x=schools.old$r1000)) +
  geom_histogram(aes(fill=cut(schools.old$r1000, breaks=seq(49, 999, by = 50))), colour="black", fill="white", binwidth = 25) +
  #geom_density(alpha=.2) +
  scale_x_continuous(name="Радиус доступности, школы охватывающий 1000 школьников", breaks=seq(0, 1000, by = 50), limits=c(0, 1000)) +
  scale_y_continuous(name="Частота распределения", breaks=seq(0, 350, 50)) +
  theme(legend.position="none")

freqhist_asarea_students <- ggplot(schools, aes(x=schools$ASSIGNMENT_AREA_STUDENTS)) +
  geom_histogram(aes(fill=cut(schools$ASSIGNMENT_AREA_STUDENTS, breaks=seq(999, 49999, by = 1000))), colour="black", fill="white", binwidth = 25) +
  #geom_density(alpha=.2) +
  scale_x_continuous(name="Школьники в закрепленной территории", breaks=seq(0, 50000, by = 1000), limits=c(0, 50000)) +
  scale_y_continuous(name="Частота распределения", breaks=seq(0, 350, 50)) +
  theme(legend.position="none")