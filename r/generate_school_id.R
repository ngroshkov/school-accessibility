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