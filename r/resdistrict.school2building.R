resdistrict.school2building = function(buildings, schools, school.blocks){
  resdistrict.school2building <- data.frame();
  
  for (sIdx in 1:nrow(schools)) {
    sId <- schools$ID[sIdx];

    rdIds <- unique(school.blocks[school.blocks$SCHOOL_ID == sId,]$RESDISTRICT_ID);
    
    df <- data.frame(BUILDING_ID = buildings[buildings$RESDISTRICT_ID %in% rdIds,]$ID);
    if (nrow(df) == 0) {
      next;
    }
    
    df$SCHOOL_ID <- sId;
    
    resdistrict.school2building <- rbind(resdistrict.school2building, df);
    
    print(paste(sIdx, " ", schools$NAME[sIdx], " ", round((100*sIdx)/nrow(schools), digits = 2), "%", sep = ""));
  }
  resdistrict.school2building <- resdistrict.school2building[c(2,1)];
  resdistrict.school2building <- resdistrict.school2building[order(resdistrict.school2building$SCHOOL_ID),]
  resdistrict.school2building;
}