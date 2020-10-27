## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----export-------------------------------------------------------------------
library(Ecdat)
availInPkg <- objects(2,
        pattern='nuclearWeaponStates')
canWrite <- FALSE 
(TempDir <- tempdir())
# Write the name of TempDir so I can find it. 
# Or set 
# TempDir <- getwd()
# for manual processing.

if('nuclearWeaponStates' %in% availInPkg){
  data(nuclearWeaponStates)
  TempFile <- file.path(TempDir, 
                'nuclearWeaponStates.csv')
  canWr <- try(write.csv(nuclearWeaponStates, 
            TempFile, row.names=FALSE))
  if(!inherits(canWr, 'try-error')){
      canWrite <- TRUE
  }
} 
getwd()
dir(TempDir)

## ----readCSV------------------------------------------------------------------
library(Ecfun)
if(canWrite){
  nWS <- read.csv(TempFile, 
              stringsAsFactors = FALSE)
  igno <- c('nation', 'ctry', 'Maddison')
  igno. <- (igno %in% names(nWS))
  Dts <- c("firstTest", "startNucPgm")
  Dts. <- (Dts %in% names(nWS))
  nWSdf <-asNumericDF(nWS, 
      ignore=igno[igno.], Dates=Dts[Dts.]) 
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
  nuclearWeaponStates <- nWSdf
  nuclearWeaponStates$firstTestYr <- firstTestYr
  if('startNucPgm' %in% nmsNucWeapSt){
    startNucPgmYr <- lubridate::decimal_date(
        nWSdf$startNucPgm) 
    nuclearWeaponStates$startNucPgmYr <- startNucPgmYr
  } 
#  else {
#    print(nmsNucWeapSt)
#    err2 <- paste("'startNucPgm' not in", 
#        "names(nuclearWeaponStates)")
#    stop(err2)
#  }
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

