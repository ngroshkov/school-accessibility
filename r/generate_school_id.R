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

generate.school2buildings.for.close.doubles <- function(agg.double, buildings) {
  generate.school2buildings.for.close.doubles <- data.frame();
  
  for (bIdx in 1:nrow(agg.double)) {
    id <- agg.double$ID[bIdx];
    
    builds <- buildings[buildings$ID == id,]
    
    newId1 <- builds$NEW_ID[1];
    newId2 <- builds$NEW_ID[2];
    
    sch2bld <- school2building[school2building$BUILDING_ID == id,];
    
    if (nrow(sch2bld) > 0) {
      sch2bld$OBJECTID <- NA;
    
      sch2bld1 <- sch2bld2 <- sch2bld;
    
      sch2bld1$NEW_BUILDING_ID <- newId1;
      sch2bld2$NEW_BUILDING_ID <- newId2;
    
      generate.school2buildings.for.close.doubles <- rbind(generate.school2buildings.for.close.doubles, sch2bld1, sch2bld2);
    }
  }
  generate.school2buildings.for.close.doubles;
}

generate.school2buildings.for.far.doubles <- function(agg.double, buildings, school2building, schools, school.blocks) {
  generate.school2buildings.for.far.doubles <- data.frame();
  
  for (bIdx in 1:nrow(agg.double)) {
    id <- agg.double$ID[bIdx];
    
    builds <- buildings[buildings$ID == id,]
    
    newId1 <- builds$NEW_ID[1];
    newId2 <- builds$NEW_ID[2];
    
    p1 <- c(builds$POINT_X[1], builds$POINT_Y[1]);
    p2 <- c(builds$POINT_X[2], builds$POINT_Y[2]);
    
    schoolIds <- unique(school2building[school2building$BUILDING_ID == id,]$SCHOOL_ID);
    schoolIds <- schools[schools$ID %in% schoolIds,]$ID;
    
    for (sId in schoolIds) {
      sblks <- school.blocks[school.blocks$SCHOOL_ID == sId,];
      
      closeBuildings <- numeric();
      
      for (sblkIdx in 1:nrow(sblks)) {
        sblk <- sblks[sblkIdx,];
        sblkP <- c(sblk$POINT_X[1],sblk$POINT_Y[1]);
        
        dist1 <- distVincentyEllipsoid(p1, sblkP);
        dist2 <- distVincentyEllipsoid(p2, sblkP);
        
        newId <- if (dist1 < dist2) newId1 else newId2;
        
        closeBuildings <- c(closeBuildings, newId);
        
      };
      
      closeBuildings <- unique(closeBuildings);
      
      s2b <- data.frame(OBJECTID = NA, BUILDING_ID = id, SCHOOL_ID = sId, NEW_BUILDING_ID=closeBuildings);
      
      generate.school2buildings.for.far.doubles <- rbind(generate.school2buildings.for.far.doubles, s2b);
    };
    print(paste(bIdx, " ", round((100*bIdx)/nrow(agg.double), digits = 2), "%", sep = ""));
  };
  generate.school2buildings.for.far.doubles;
}

fix.school2building.right.district = function(school2building, schoolId, rightDistrictIds) {
  school2building.to.change <- school2building[school2building$SCHOOL_ID == schoolId,];
  fix.school2building.right.district <- school2building[school2building$SCHOOL_ID != schoolId,];
  
  school2building.to.change <- school2building.to.change[school2building.to.change$BUILDING_ID %in% 
                                                                             buildings[buildings$DISTRICT_ID %in% rightDistrictIds,]$ID,];
  fix.school2building.right.district <- rbind(fix.school2building.right.district, school2building.to.change);
  fix.school2building.right.district;
}  

fix.school2building.wrong.district = function(school2building, schoolId, wrongDistrictIds) {
  school2building.to.change <- school2building[school2building$SCHOOL_ID == schoolId,];
  fix.school2building.wrong.district <- school2building[school2building$SCHOOL_ID != schoolId,];
  
  school2building.to.change <- school2building.to.change[!(school2building.to.change$BUILDING_ID %in% 
                                                                             buildings[buildings$DISTRICT_ID %in% wrongDistrictIds,])$ID,];
  fix.school2building.wrong.district <- rbind(fix.school2building.right.district, school2building.to.change);
  fix.school2building.wrong.district;
}

build.school2building.for.school = function(buildings, schoolId, buildingsIds) {
  build.school2building.for.school <- data.frame(buildings[buildings$ID %in% buildingsIds,][c(2,3)])
  build.school2building.for.school$SCHOOL_ID <- schoolId
  build.school2building.for.school$OLD_BUILDING_ID <- build.school2building.for.school$OLD_ID
  build.school2building.for.school$BUILDING_ID <- build.school2building.for.school$ID
  build.school2building.for.school$OLD_ID <- NULL
  build.school2building.for.school$ID <- NULL
  build.school2building.for.school;
}
