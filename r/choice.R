school.choice.radius <- function(buildings, school.blocks) {
  
  ids <- integer();
  s500count <- integer();
  s800count <- integer();
  s1000count <- integer();
  
  for (bIdx in 1:nrow(buildings)) {
    id <- buildings$ID[bIdx];
    
    p <- c(buildings$POINT_X[bIdx], buildings$POINT_Y[bIdx]);
    
    s500 <- numeric();
    s800 <- numeric();
    s1000 <- numeric();
    for (sbIdx in 1:nrow(school.blocks)) {
      sId <- school.blocks$SCHOOL_ID[sbIdx];
      sp <- c(school.blocks$POINT_X[sbIdx], school.blocks$POINT_Y[sbIdx]);
      
      dist <- distVincentyEllipsoid(p, sp);
      
      if (dist <= 500) {
        s500 <- c(s500, sId);
        s800 <- c(s800, sId);
        s1000 <- c(s1000, sId);
      } else if (dist <= 800) {
        s800 <- c(s800, sId);
        s1000 <- c(s1000, sId);
      } else if (dist <= 1000) {
        s1000 <- c(s1000, sId);
      }
    };
    
    ids <- c(ids, id);
    s500count <- c(s500count, length(unique(s500)));
    s800count <- c(s800count, length(unique(s800)));
    s1000count <- c(s1000count, length(unique(s1000))); 
    
    print(paste(bIdx, " ", round((100*bIdx)/nrow(buildings), digits = 2), "%", sep = ""));
  };
  school.choice.radius <- data.frame(ID = ids, S500 = s500count, S800 = s800count, S1000 = s1000count);
}

school.choice.complex <- function(buildings, school2building, complexes) {
  school.choice <- integer();
  
  for (bIdx in 1:nrow(buildings)) {
    id <- buildings$ID[bIdx];
    complexIds <- unique(schools[schools$ID %in% school2building[school2building$BUILDING_ID == id,]$SCHOOL_ID,]$COMPLEX_ID)
    schools.count <- sum(complexes[complexes$ID %in% complexIds,]$SCHOOLS_COUNT)
    school.choice <- c(school.choice, schools.count);
    print(paste(bIdx, " ", round((100*bIdx)/nrow(buildings), digits = 2), "%", sep = ""));
  }
  school.choice.complex <- data.frame(BUILDING_ID = buildings$ID, ACCESSIBLE_SCHOOLS_COMPLEX = school.choice);
  school.choice.complex;
}