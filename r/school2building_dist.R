calc.school2building.dist <- function(buildings, schools, school.blocks) {
  
  calc.school2building.dist <- data.frame();

  for (sIdx in 1:nrow(schools)) {
    sId <- schools$ID[sIdx];
    sblocks <- school.blocks[school.blocks$SCHOOL_ID == sId,];
    
    dists <- integer();
    for (bIdx in 1:nrow(buildings)) {
      bId <- buildings$ID[bIdx];
      bp <- c(buildings$POINT_X[bIdx], buildings$POINT_Y[bIdx]);
      
      sbdists <- numeric();
      for (sbIdx in 1:nrow(sblocks)) {
        sbp <- c(sblocks$POINT_X[sbIdx], sblocks$POINT_Y[sbIdx]);
        sbdists <- c(sbdists, distVincentyEllipsoid(bp, sbp));
      }
      
      dist <- as.integer(round(min(sbdists)));
      dists <- c(dists, dist);
    }
    df <- data.frame(BUILDING_ID = buildings$ID)
    df$SCHOOL_ID <- sId;
    df$DIST <- dists;
    
    calc.school2building.dist <- rbind(calc.school2building.dist, df);
    print(paste(sIdx, " ", round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
  };
  calc.school2building.dist;  
}

calc.school2building.economic <- function(buildings, schools, school2building.dist, 
                                          schools.current.fill, school2building.economic, minSchoolStudents = 500) {
  if (is.null(schools.current.fill)) {
    schools.current.fill <- data.frame(ID = schools$ID, CURRENT_FILL = 0);
  }

  result.school.ids <- integer();
  result.building.ids <- integer();
  if (!is.null(school2building.economic)) {
    result.school.ids <- school2building.economic$SCHOOL_ID;
    result.building.ids <- school2building.economic$BUILDING_ID;
  }
 
  
  for (s2bIdx in 1:nrow(school2building.dist)) {
    if (s2bIdx %% 100000 == 0) {
      write.csv(data.frame(SCHOOL_ID = result.school.ids, BUILDING_ID = result.building.ids), "../csv/school2building-ecomomic.csv", row.names = FALSE);
      write.csv(schools.current.fill, "../csv/schools-current-fill.csv", row.names = FALSE);
    }
    
    sId <- school2building.dist$SCHOOL_ID[s2bIdx];
    bId <- school2building.dist$BUILDING_ID[s2bIdx];

    if (bId %in% result.building.ids) {
      next;
    }

    currentFill <- schools.current.fill[schools.current.fill$ID==sId,]$CURRENT_FILL;
    if (currentFill >= minSchoolStudents) {
      next;
    }
    result.school.ids <- c(result.school.ids, sId);
    result.building.ids <- c(result.building.ids, bId);
    
    students <- buildings[buildings$ID == bId,]$STUDENTS;
    schools.current.fill[schools.current.fill$ID==sId,]$CURRENT_FILL <- currentFill + students;

    print(paste(s2bIdx, " ", round((100*s2bIdx)/nrow(school2building.dist), digits = 2), "%", sep = ""));
    
  }
  
  school2building.economic <- data.frame(SCHOOL_ID = result.school.ids, BUILDING_ID = result.building.ids);
  calc.school2building.economic <- list(schools.current.fill, school2building.economic);
  calc.school2building.economic;
}