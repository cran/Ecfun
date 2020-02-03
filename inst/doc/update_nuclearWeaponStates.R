## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----export-------------------------------------------------------------------
library(Ecdat)
availInPkg <- objects(2,
        pattern='nuclearWeaponStates')
canWrite <- FALSE 
if('nuclearWeaponStates' %in% availInPkg){
  data(nuclearWeaponStates)
  canWr <- try(write.csv(nuclearWeaponStates, 
            'nuclearWeaponStates.csv', 
            row.names=FALSE))
  if(!inherits(canWr, 'try-error')){
      canWrite <- TRUE
  }
} 
getwd()

## ----readCSV------------------------------------------------------------------
library(Ecfun)
if(canWrite){
  nWS <- read.csv(
    'nuclearWeaponStates.csv', 
    stringsAsFactors = FALSE)
  nWSdf <-
    asNumericDF(nWS, 
          ignore=1:2, Dates=3) 
}

## ----chk1---------------------------------------------------------------------
if(canWrite){
  nmsNucWeapSt <- names(nWSdf)
  if('firstTest' %in% nmsNucWeapSt){
    firstTestYr <- lubridate::decimal_date(
        nWSdf$firstTest) 
    yearsSinceLastFirstTest <- c(NA, 
      diff(firstTestYr) ) 
  } else {
    print(nmsNucWeapSt)
    err <- paste("'firstTest' not in", 
        "names(nuclearWeaponStates)")
    stop(err)
  }
  
  nuclearWeaponStates <- cbind(
    nWSdf[, c('nation', 'ctry', 'firstTest')], 
    firstTestYr = firstTestYr, 
    yearsSinceLastFirstTest = 
      yearsSinceLastFirstTest, 
  nWSdf[, c('nuclearWeapons', 'nYieldNA', 
      'nLowYield', 'nMidYield', 'nHighYield')]
  )
}

## ----nukes--------------------------------------------------------------------
if(canWrite){
  nukes <- rowSums(nuclearWeaponStates[, 
    c('nYieldNA', 'nLowYield', 'nMidYield',
      'nHighYield')])
  dnuk <- (nuclearWeaponStates[,
      'nuclearWeapons'] - nukes)
  if(length(oopsNuk <- which(dnuk != 0))>0){
    cat('count errors.  bad rows =')
    print(oopsNuk)
  }
}

## ----plot---------------------------------------------------------------------
if(canWrite){
  data(nuclearWeaponStates)
  plot(yearsSinceLastFirstTest~firstTest,
       nuclearWeaponStates, las=1, 
       type='h', xlab='', ylab='')
  with(nuclearWeaponStates, 
    text(firstTest, 
        yearsSinceLastFirstTest, ctry))
}

## ----svg----------------------------------------------------------------------
if(FALSE){
  svg('Time2nextNuclearState.svg')
  cex. <- 1.5
  plot(yearsSinceLastFirstTest~firstTest,
       nuclearWeaponStates, las=1, 
       type='h', xlab='', ylab='',
       cex.axis=cex.)
  with(nuclearWeaponStates, 
    text(firstTest, yearsSinceLastFirstTest,
         ctry, cex=cex.))
  dev.off()
}

## ----save---------------------------------------------------------------------
if(canWrite){
  cat('canWrite = TRUE')
  save(nuclearWeaponStates, 
       file='nuclearWeaponStates.rda')
}

