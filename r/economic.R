intended.students  <- function(schoolId, school2building, field) {
  
  bIds <- school2building[school2building$SCHOOL_ID == schoolId,]$BUILDING_ID;
  blds <- buildings[buildings$ID %in% bIds,]
  
  intended.students <- sum(blds$STUDENTS/blds[[field]]);
  intended.students;
}