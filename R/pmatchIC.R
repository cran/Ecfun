pmatchIC <- function(x, table, 
      nomatch = NA_integer_, 
      duplicates.ok = FALSE, 
      ignoreCase=TRUE){
##
## 1.  create material for msgs
##     in case needed  
  if(length(x)<4){
    x3 <- paste(x, collapse=', ')
  } else{
    x3. <- paste(x[1:3], collapse=', ')
    x3 <- paste0(x3., ', ...')
  }
  msg0 <- paste0('x = "', x3, 
      '" not found in table = ')
  if(length(table)<4){
    tab3 <- paste(table, collapse=', ')
  } else {
    tab3. <- paste(table[1:3], 
                   collapse=', ')
    tab3 <- paste0(tab3., ', ...')
  }
##
## 2.  Is x not character?
##  
  if(!is.character(x)){
    X <- table[x]
    if(is.na(X)){
      stop(msg0, tab3)
    }
    return(X)
  }
##
## 3.  Else get numeric position
##
  if(ignoreCase){
    x1 <- pmatch(tolower(x), tolower(table), 
            nomatch = nomatch, 
            duplicates.ok = duplicates.ok)
  } else {
    x1 <- pmatch(x, table, 
            nomatch = nomatch, 
           duplicates.ok = duplicates.ok)
  }
  if(is.na(x1)){
    stop(msg0, tab3)
  }
  table[x1]
}
