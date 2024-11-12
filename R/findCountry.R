findCountry <- function(string, referenceTable = 
        rworldmap::countrySynonyms){
  ux <- toupper(string)
  iso3 <- toupper(referenceTable[, 2])
  ctrySyn <- lapply(
    referenceTable[-(1:2)], 
                    toupper)
  nRefs <- length(ctrySyn)
  nStr <- length(string)
  out <- rep(NA, nStr)
  names(out) <- string
  for(iStr in 1:nStr){ 
    for(i in 1:nRefs){
      ix <- which(ux[iStr] == ctrySyn[[i]])
      if(length(ix)>0) break
      ix <- grep(ux[iStr], ctrySyn[[i]])
      if(length(ix)>0) break  
    }
    if(length(ix)<1){
      out[iStr] <- paste('No match found for country', 
                         string[iStr])
    } else{
      out[iStr] <- iso3[ix]
      if(nchar(out[iStr])<1){ 
        out[iStr] <- paste0('referenceTable[', ix,
                    ", 2] = ''")
      }
    }
  }
  out
}
