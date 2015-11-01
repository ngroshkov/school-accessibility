complex.size = function(complexes){
  complex.size = data.frame();
  
  xPathStep1 <- "//div[text()='Сведения о реализации образовательных программ']/following-sibling::table[1]/tbody/tr/td[text()='Начальное общее']/following-sibling::td[2]/text()";
  xPathStep2 <- "//div[text()='Сведения о реализации образовательных программ']/following-sibling::table[1]/tbody/tr/td[text()='Основное общее']/following-sibling::td[2]/text()";
  xPathStep3 <- "//div[text()='Сведения о реализации образовательных программ']/following-sibling::table[1]/tbody/tr/td[text()='Среднее общее']/following-sibling::td[2]/text()";
    
  for (cIdx in 1:nrow(complexes)) {
    cId <- complexes$ID[cIdx];
    
    url <- complexes$WEBSITE[cIdx];
    print(paste(cIdx, " ", url, " ", round((100*cIdx)/nrow(complexes), digits = 2), "%", sep = ""));
    
    step1 <- NA;
    step2 <- NA;
    step3 <- NA;
    
    if (!is.null(url)) {
      page <- htmlParse(paste(sep = "", url, "info_edu/education/"));
      
      nodes1 <- xpathApply(doc = page, path = xPathStep1);
      nodes2 <- xpathApply(doc = page, path = xPathStep2);
      nodes3 <- xpathApply(doc = page, path = xPathStep3);
      if (length(nodes1) > 0) {
        step1 <- as.numeric(xmlValue(nodes1[[1]]));
      };
      if (length(nodes2) > 0) {
        step2 <- as.numeric(xmlValue(nodes2[[1]]));
      };
      if (length(nodes3) > 0) {
        step3 <- as.numeric(xmlValue(nodes3[[1]]));
      };
    };
    
    df <- data.frame(ID=cId, STEP1 = step1, STEP2 = step2, STEP3 = step3);
    complex.size <- rbind(complex.size, df);
    
    write.csv(complex.size, "~/Desktop/complex.size.csv", row.names = FALSE, na = "")
  }
  
  complex.size;
}