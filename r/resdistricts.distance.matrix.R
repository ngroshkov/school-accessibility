calc.resdistrict.matrix  <- function(resdistricts) {
  calc.resdistrict.matrix <- data.frame();
  
  breaks <- c(100, 200, 300, 400, 500, 600, 637)
  
  for (limX in breaks) {
    df1 <- tail(head(resdistricts, limX), ifelse(limX == 637, 37, 100));
    
    for (limY in breaks) {
      df2 <- tail(head(resdistricts, limY), ifelse(limY == 637, 37, 100));
      
#      m <- matrix(c(1), nrow=nrow(df1), ncol=nrow(df2));
      m <- osrmTableOD(df1, "ID", "POINT_X", "POINT_Y", df2, "ID", "POINT_X", "POINT_Y", limit = 100)
      
      originIds <- c()
      destIds <- c()
      values <- c()
        
      for (i in 1:nrow(m)) {
        for (j in 1:ncol(m)) {
          originIds <- c(originIds, df1$ID[i]);
          destIds <- c(destIds, df2$ID[j]);
          values <- c(values, m[i,j]);
        }
      }
      
      calc.resdistrict.matrix <- rbind(calc.resdistrict.matrix, data.frame(ORIGIN_ID = originIds, DEST_ID = destIds, VALUE = values))
      
      print(paste(head(df1, 1)$ID, "+", tail(df1, 1)$ID, " ", head(df2, 1)$ID, "+", tail(df2, 1)$ID));
      
      Sys.sleep(15);
    }
  }
  
  
  calc.resdistrict.matrix;
}