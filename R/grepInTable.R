grepInTable <- function(pattern, referenceTable = 
        rworldmap::countrySynonyms[, -1], 
        ignore.case=TRUE, collapse=', ', ...){
  if(ignore.case){ 
    ux <- toupper(pattern)
    iso3 <- toupper(referenceTable[, 1])
    ctrySyn <- lapply(referenceTable[-1], toupper)
  } else {
    ux <- pattern
    iso3 <- referenceTable[, 1]
    ctrySyn <- referenceTable[-1]
  }
  nRefs <- length(ctrySyn)
  nStr <- length(pattern)
  out <- rep(NA, nStr)
  names(out) <- pattern
  for(iStr in 1:nStr){ 
    for(i in 1:nRefs){
      ix <- which(ux[iStr] == ctrySyn[[i]])
      if(length(ix)>0) break
      ix <- grep(ux[iStr], ctrySyn[[i]])
      if(length(ix)>0) break  
    }
    if(length(ix)<1){
      out[iStr] <- paste('No match found for', 
                         pattern[iStr])
    } else{
      out[iStr] <- iso3[ix]
      if(nchar(out[iStr])<1){ 
        out[iStr] <- paste('Found row', ix, 
            "of referenceTable with column 1 = ''")
      }
    }
  }
  out
}
