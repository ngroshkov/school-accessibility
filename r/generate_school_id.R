building2school.generate.new.school.id = function(schools, school2building){
  
  for (sIdx in 1:nrow(schools)) {
    
    sId <- schools$ID[sIdx];
    sNewId <- schools$NEW_ID[sIdx];
    
    school2building$SCHOOL_NEW_ID[school2building$SCHOOL_NEW_ID == sId] <- sNewId;
    print(paste(sIdx, " ", schools$NAME[sIdx], " ", round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
  };
  
  building2school.generate.new.school.id <- school2building;
}

building2school.generate.new.building.id = function(buildings, school2building){
  
  for (bIdx in 1:nrow(buildings)) {
    
    bId <- buildings$ID[bIdx];
    bNewId <- buildings$NEW_ID[bIdx];
    
    school2building$NEW_BUILDING_ID[school2building$NEW_BUILDING_ID == bId] <- bNewId;
    print(paste(bIdx, " ", buildings$NAME[bIdx], " ", round((100*bIdx)/nrow(buildings), digits = 2), "%", sep = ""));
  };
  
  building2school.generate.new.building.id <- school2building;
}

building2school.new <-function(schools, school2building){
  building2school.new <- data.frame();
  
  for (sIdx in 1:nrow(schools)) {
    sId <- schools$ID[sIdx];
    sNewId <- schools$NEW_ID[sIdx];
    
    school2building.subset <- subset(school2building, SCHOOL_ID == sId);
    if (nrow(school2building.subset) > 0) {
      school2building.subset$SCHOOL_NEW_ID <- sNewId;
    }
    
    
    building2school.new <- rbind(building2school.new, school2building.subset);
    
    print(paste(sIdx, " ", round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
    
  };
  building2school.new;
}

assigned.area.best.school.ids <- function(schools.without.assigned.area, schools.new) {
  assigned.area.best.school.ids <- data.frame();
  
  for (sIdx in 1:nrow(schools.without.assigned.area)) {
    id <- schools.without.assigned.area$NEW_ID[sIdx];
    complexId <- schools.without.assigned.area$COMPLEX_ID[sIdx];
    
    ss <- subset(schools.new, COMPLEX_ID == complexId);
    ss <- ss[order(ss$HOUSE_ID_COUNT, decreasing = TRUE),];
    ss <- ss[c(1, 12)];
    
    ss$SCHOOL_ID <- id; 
    ss$ASSIGNED_SCHOOL_ID <- ss$NEW_ID;
    ss <- ss[c(3,4,2)];
    
#   ss <- ss[ss$SCHOOL_ID != ss$ASSIGNED_SCHOOL_ID,];
    
    ss$SELF <- ss$SCHOOL_ID == ss$ASSIGNED_SCHOOL_ID;
    ss$ZERO_ASSIGNED <- nrow(ss) >= 1 & ss$HOUSE_ID_COUNT[1] == 0;
    
    
    assigned.area.best.school.ids <- rbind(assigned.area.best.school.ids, ss[1,]);
#   assigned.area.best.school.ids <- rbind(assigned.area.best.school.ids, ss);

    
  };
  assigned.area.best.school.ids;
}

calc.distance.between.buildings.double <- function(agg.double, buildings) {
  calc.distance.between.buildings.double <- numeric();
  
  for (bIdx in 1:nrow(agg.double)) {
    id <- agg.double$ID[bIdx];
    
    builds <- buildings[buildings$ID == id,]
    
    p1 <- c(builds$POINT_X[1], builds$POINT_Y[1]);
    p2 <- c(builds$POINT_X[2], builds$POINT_Y[2])

    distance <- distVincentyEllipsoid(p1, p2);
    
    calc.distance.between.buildings.double <- c(calc.distance.between.buildings.double, distance)
    
    print(paste(bIdx, " ", round((100*bIdx)/nrow(agg.double), digits = 2), "%", sep = ""));
  };
  
  calc.distance.between.buildings.double;
}

calc.same.opl <- function(agg.double, buildings) {
  calc.same.opl <- logical();
  
  for (bIdx in 1:nrow(agg.double)) {
    id <- agg.double$ID[bIdx];
    
    builds <- buildings[buildings$ID == id,]
    s1 <- builds$OPL_G[1];
    s2 <- builds$OPL_G[2];

    calc.same.opl <- c(calc.same.opl, s1 == s2)
    
    print(paste(bIdx, " ", round((100*bIdx)/nrow(agg.double), digits = 2), "%", sep = ""));
  };
  
  calc.same.opl;
}

calc.same.opl <- function(agg.double, buildings) {
  for (bIdx in 1:nrow(agg.double)) {
    id <- agg.double$ID[bIdx];
    
    builds <- buildings[buildings$ID == id,]
    
    p1 <- c(builds$POINT_X[1], builds$POINT_Y[1]);
    p2 <- c(builds$POINT_X[2], builds$POINT_Y[2]);
    
    sch2bld <- school2building[school2building$BUILDING_ID == id];
    
    schoolIdList <- numeric();
    
    for (s2bIdx in 1:nrow(sch2bld)) {
      sId <- sch2bld$SCHOOL_ID[s2bIdx];
      
      if (nrow(schools[schools$ID == sId,]) > 0) {
        schoolIdList <- c(schoolIdList, sId);  
      };
      
    };
    
    for (sId in schoolIdList) {
      sch.blks <- school.blocks[school.blocks$SCHOOL_ID == sId,];
      
      sch.blks.list <- numeric();
      
      for (sbIdx in 1:nrow(sch.blks)){
        sbId <- sch.blks$ID[sbId];
        sbp <- c(sch.blks$POINT_X[sbId], sch.blks$POINT_Y[sbId]);
        
        dist1 <- distVincentyEllipsoid(p1, sbp);
        dist2 <- distVincentyEllipsoid(p2, sbp);
        
        sch.blks.list <- c(sch.blks.list, sbId)
      };
    };
    
  };
  
}