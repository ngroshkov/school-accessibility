generate.connection.features <- function(buildings, schools, school.blocks, school2building) {
  generate.connection.features <- data.frame();
  
  featureId <- 0;
  for (sIdx in 1:nrow(schools)) {
    sId <- schools$ID[sIdx];
    
    s2b <- school2building[school2building$SCHOOL_ID == sId,];
    
    if (nrow(s2b) == 0) {
      next;
    }
    
    s2b <- merge(s2b, buildings[buildings$ID %in% s2b$BUILDING_ID,][c(2, 10, 11)], by.x = "BUILDING_ID", by.y = "ID");
    s2b$BUILDING_POINT_X <- s2b$POINT_X;
    s2b$BUILDING_POINT_Y <- s2b$POINT_Y;
    
    s2b$POINT_X <- NULL;
    s2b$POINT_Y <- NULL;
    
    sbs <- school.blocks[school.blocks$SCHOOL_ID == sId,];
      
    school.blocks.ids <- integer();
    school.blocks.p_x <- numeric();
    school.blocks.p_y <- numeric();
    bearings <- numeric();  
    for(s2bIdx in 1:nrow(s2b)) {
      bp <- c(s2b$BUILDING_POINT_X[s2bIdx],s2b$BUILDING_POINT_Y[s2bIdx]);
      
      dists <- integer();
      for (sbIdx in 1:nrow(sbs)) {
        dist <- distVincentyEllipsoid(bp, c(sbs$POINT_X[sbIdx], sbs$POINT_Y[sbIdx]));
        dists <- c(dists, dist);
      }
      sbs$DIST <- dists;
      
      closestSchoolBlock <- sbs[which(sbs$DIST == min(sbs$DIST)),];
      school.blocks.ids <- c(school.blocks.ids, closestSchoolBlock$ID[1]);
      school.blocks.p_x <- c(school.blocks.p_x, closestSchoolBlock$POINT_X[1]);
      school.blocks.p_y <- c(school.blocks.p_y, closestSchoolBlock$POINT_Y[1]);
      bearings <- c(bearings, bearing(c(closestSchoolBlock$POINT_X[1], closestSchoolBlock$POINT_Y[1]), bp));
    };
      
    s2b$SCHOOL_BLOCK_ID <- school.blocks.ids;
    s2b$SCHOOL_BLOCK_POINT_X <- school.blocks.p_x;
    s2b$SCHOOL_BLOCK_POINT_Y <- school.blocks.p_y;
    s2b$BEARING <- bearings;
      
    s2b <- s2b[order(s2b$SCHOOL_BLOCK_ID, s2b$BEARING),]
      
    schs <- integer();
    s.blks <- integer()
    featureIds <- integer();
    pointXs <- numeric();
    pointYs <- numeric();
    for(s2bIdx in 1:nrow(s2b)) {
      featureId <- featureId + 1;
      
      schs <- c(schs, s2b$SCHOOL_ID[s2bIdx]);
      s.blks <- c(s.blks, s2b$SCHOOL_BLOCK_ID[s2bIdx]);
      featureIds <- c(featureIds, featureId);
      pointXs <- c(pointXs, s2b$BUILDING_POINT_X[s2bIdx]);
      pointYs <- c(pointYs, s2b$BUILDING_POINT_Y[s2bIdx]);
      
      schs <- c(schs, s2b$SCHOOL_ID[s2bIdx]);
      s.blks <- c(s.blks, s2b$SCHOOL_BLOCK_ID[s2bIdx]);
      featureIds <- c(featureIds, featureId);
      pointXs <- c(pointXs, s2b$SCHOOL_BLOCK_POINT_X[s2bIdx]);
      pointYs <- c(pointYs, s2b$SCHOOL_BLOCK_POINT_Y[s2bIdx]);
    }
    
    connection.features <- 
      data.frame(SCHOOL_ID = schs, SCHOOL_BLOCK_ID = s.blks, FEATURE_ID = featureIds, POINT_X = pointXs, POINT_Y = pointYs);
    
    generate.connection.features <- rbind(generate.connection.features, connection.features);
    
    print(paste(sIdx, " ", round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
  }
  generate.connection.features;
}

generate.area.features <- function(buildings, schools, school.blocks, school2building) {
  generate.area.features <- data.frame();
  
  featureId <- 0;
  for (sIdx in 1:nrow(schools)) {
    sId <- schools$ID[sIdx];

    s2b <- school2building[school2building$SCHOOL_ID == sId,];
    
    if (nrow(s2b) == 0) {
      next;
    }
    
    s2b <- merge(s2b, buildings[buildings$ID %in% s2b$BUILDING_ID,][c(2, 10, 11)], by.x = "BUILDING_ID", by.y = "ID");
    s2b$BUILDING_POINT_X <- s2b$POINT_X;
    s2b$BUILDING_POINT_Y <- s2b$POINT_Y;
    
    s2b$POINT_X <- NULL;
    s2b$POINT_Y <- NULL;
    
    sbs <- school.blocks[school.blocks$SCHOOL_ID == sId,];
    
    school.blocks.ids <- integer();
    school.blocks.p_x <- numeric();
    school.blocks.p_y <- numeric();
    bearings <- numeric();  
    for(s2bIdx in 1:nrow(s2b)) {
      bp <- c(s2b$BUILDING_POINT_X[s2bIdx],s2b$BUILDING_POINT_Y[s2bIdx]);
      
      dists <- integer();
      for (sbIdx in 1:nrow(sbs)) {
        dist <- distVincentyEllipsoid(bp, c(sbs$POINT_X[sbIdx], sbs$POINT_Y[sbIdx]));
        dists <- c(dists, dist);
      }
      sbs$DIST <- dists;
      
      closestSchoolBlock <- sbs[which(sbs$DIST == min(sbs$DIST)),];
      school.blocks.ids <- c(school.blocks.ids, closestSchoolBlock$ID[1]);
      school.blocks.p_x <- c(school.blocks.p_x, closestSchoolBlock$POINT_X[1]);
      school.blocks.p_y <- c(school.blocks.p_y, closestSchoolBlock$POINT_Y[1]);
      bearings <- c(bearings, bearing(c(closestSchoolBlock$POINT_X[1], closestSchoolBlock$POINT_Y[1]), bp));
    };
    
    s2b$SCHOOL_BLOCK_ID <- school.blocks.ids;
    s2b$SCHOOL_BLOCK_POINT_X <- school.blocks.p_x;
    s2b$SCHOOL_BLOCK_POINT_Y <- school.blocks.p_y;
    s2b$BEARING <- bearings;
    
    s2b <- s2b[order(s2b$SCHOOL_BLOCK_ID, s2b$BEARING),]
    
    schs <- integer();
    s.blks <- integer()
    featureIds <- integer();
    pointXs <- numeric();
    pointYs <- numeric();
    for(s2bIdx in 1:nrow(s2b)) {
      featureId <- featureId + 1;
      
      schs <- c(schs, s2b$SCHOOL_ID[s2bIdx]);
      s.blks <- c(s.blks, s2b$SCHOOL_BLOCK_ID[s2bIdx]);
      featureIds <- c(featureIds, featureId);
      pointXs <- c(pointXs, s2b$BUILDING_POINT_X[s2bIdx]);
      pointYs <- c(pointYs, s2b$BUILDING_POINT_Y[s2bIdx]);
      
      schs <- c(schs, s2b$SCHOOL_ID[s2bIdx]);
      s.blks <- c(s.blks, s2b$SCHOOL_BLOCK_ID[s2bIdx]);
      featureIds <- c(featureIds, featureId);
      if (s2bIdx + 1 > nrow(s2b) | s2b$SCHOOL_BLOCK_ID[s2bIdx] != s2b$SCHOOL_BLOCK_ID[s2bIdx+1]) {
        s2b.first <- s2b[s2b$SCHOOL_BLOCK_ID == s2b$SCHOOL_BLOCK_ID[s2bIdx],];
        
        pointXs <- c(pointXs, s2b.first$BUILDING_POINT_X[1]);
        pointYs <- c(pointYs, s2b.first$BUILDING_POINT_Y[1]);
      } else {
        pointXs <- c(pointXs, s2b$BUILDING_POINT_X[s2bIdx+1]);
        pointYs <- c(pointYs, s2b$BUILDING_POINT_Y[s2bIdx+1]);
      }
      
      schs <- c(schs, s2b$SCHOOL_ID[s2bIdx]);
      s.blks <- c(s.blks, s2b$SCHOOL_BLOCK_ID[s2bIdx]);
      featureIds <- c(featureIds, featureId);
      pointXs <- c(pointXs, s2b$SCHOOL_BLOCK_POINT_X[s2bIdx]);
      pointYs <- c(pointYs, s2b$SCHOOL_BLOCK_POINT_Y[s2bIdx]);
    }
    
    area.features <- 
      data.frame(SCHOOL_ID = schs, SCHOOL_BLOCK_ID = s.blks, FEATURE_ID = featureIds, POINT_X = pointXs, POINT_Y = pointYs);
    
    generate.area.features <- rbind(generate.area.features, area.features);
    
    print(paste(sIdx, " ", round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
  }
  generate.area.features;
}