calc.school2building.dist <- function(buildings, schools, school.blocks) {
  
  calc.school2building.dist <- data.frame();

  for (sIdx in 1:nrow(schools)) {
    sId <- schools$ID[sIdx];
    sblocks <- school.blocks[school.blocks$SCHOOL_ID == sId,];
    
    dists <- numeric();
    for (bIdx in 1:nrow(buildings)) {
      bId <- buildings$ID[bIdx];
      bp <- c(buildings$POINT_X[bIdx], buildings$POINT_Y[bIdx]);
      
      sbdists <- numeric();
      for (sbIdx in 1:nrow(sblocks)) {
        sbp <- c(sblocks$POINT_X[sbIdx], sblocks$POINT_Y[sbIdx]);
        sbdists <- c(sbdists, distVincentyEllipsoid(bp, sbp));
      }
      
      dist <- min(sbdists);
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