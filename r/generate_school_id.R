building2school.generate.new.school.id = function(schools, school2building){
  
  for (sIdx in 1:nrow(schools)) {
    
    sId <- schools$ID[sIdx];
    sNewId <- schools$NEW_ID[sIdx];
    
    school2building$SCHOOL_NEW_ID[school2building$SCHOOL_NEW_ID == sId] <- sNewId;
    print(paste(sIdx, " ", schools$NAME[sIdx], " ", round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
  };
  
  building2school.generate.new.school.id <- school2building;
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