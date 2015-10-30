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

complex.school2building = function(complex, schools, buildings, school2building) {
  complex.school2building <- data.frame();
  
  excludeIds <- c(10087, 10050, 10903, 11337, 10223, 10224, 10856, 10771, 10906, 11030, 
                  11464, 11465, 11466, 11467,
                  10195, 11240, 11241)
  
  for (cIdx in 1:nrow(complex)) {
    cId <- complex$ID[cIdx];
    
    sls <- schools[schools$COMPLEX_ID == cId,];
    
    if (length(intersect(excludeIds, sls$ID)) != 0) {
      next;
    }
    
    bIds <- unique(school2building[school2building$SCHOOL_ID %in% sls$ID,]$BUILDING_ID);
    if (length(bIds) == 0) {
      next;
    }
    
    for (sIdx in 1:nrow(sls)) {
      sId <- sls$ID[sIdx];
      
      df <- data.frame(BUILDING_ID = bIds);
      df$SCHOOL_ID <- sId;
      df$COMPLEX_ID <- cId;
      
      complex.school2building <- rbind(complex.school2building, df);
    }
    print(paste(cIdx, " ", round((100*cIdx)/nrow(complex), digits = 2), "%", sep = ""));
  }
  
  complex.school2building <- complex.school2building[c(2,1,3)];
  complex.school2building <- complex.school2building[order(complex.school2building$SCHOOL_ID),]
  complex.school2building;
}