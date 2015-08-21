complex.size = function(complexes){
  
  xPathStep1 <- "//h3[text()='Сведения о реализации образовательных программ']/following-sibling::table[1]/tbody/tr/td[text()='Начальное общее']/following-sibling::td[2]/text()";
  xPathStep2 <- "//h3[text()='Сведения о реализации образовательных программ']/following-sibling::table[1]/tbody/tr/td[text()='Основное общее']/following-sibling::td[2]/text()";
  xPathStep3 <- "//h3[text()='Сведения о реализации образовательных программ']/following-sibling::table[1]/tbody/tr/td[text()='Среднее общее']/following-sibling::td[2]/text()";
    
  step1v <- numeric();
  step2v <- numeric();
  step3v <- numeric();
  
  for (cIdx in 1:nrow(complexes)) {
    url <- complexes$WEBSITE[cIdx];
    print(paste(cIdx, " ", url, " ", round((100*cIdx)/nrow(complexes), digits = 2), "%", sep = ""));
    
    step1 <- NA;
    step2 <- NA;
    step3 <- NA;
    
    if (!is.null(url)) {
      page <- htmlParse(paste(url, "info_edu/education/"));
      
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
    

    step1v <- c(step1v, step1);
    step2v <- c(step2v, step2);
    step3v <- c(step3v, step3);
    
    
  }
  
  complex.size = data.frame(step1v, step2v, step3v);
  complex.size;
}