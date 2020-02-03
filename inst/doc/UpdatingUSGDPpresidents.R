## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----yrSpan-------------------------------------------------------------------
library(Ecdat)
(rngYrs <- range(USGDPpresidents$Year))

## ----csv----------------------------------------------------------------------
getwd()
(csv2 <- dir(pattern='\\.csv$'))
if(length(csv2)==2){
  Update0 <- TRUE
} else Update0 <- FALSE

## ----read.csv-----------------------------------------------------------------
Update <- FALSE
if(Update0){
  str(USCPI <- read.csv(csv2[1], skip=2))
  str(USGDP. <- read.csv(csv2[2], skip=1))
  library(Ecfun)
  USGDP <- asNumericDF(USGDP.)
  print(rngCPIyrs <- range(USCPI$Year) )
  print(rngGDPyrs <- range(USGDP$Year) )
  endYr <- max(rngCPIyrs, rngGDPyrs)
  if(endYr>rngYrs[2]) print(Update <- TRUE)
}

## ----cy-----------------------------------------------------------------------
if(Update){
  rowsNeeded <- (endYr - rngYrs[2])
  Nold <- nrow(USGDPpresidents)
  iRep <- c(1:Nold, rep(Nold, rowsNeeded))
  USGDPp2 <- USGDPpresidents[iRep,]
}

## ----Year---------------------------------------------------------------------
if(Update){
  iNew <- (Nold+(1:rowsNeeded))
  USGDPp2$Year[iNew] <- ((rngYrs[2]+1):endYr)
  rownames(USGDPp2) <- USGDPp2$Year
#
  USGDPp2[iNew, -1] <- NA
}

## ----CPI----------------------------------------------------------------------
if(Update){
  selCPI <- (USGDPp2$Year %in% USCPI$Year)
  if(any(!is.na(USGDPp2[!selCPI, 2]))){
    stop('ERROR:  There are CPI numbers ', 
         'in the current USGDPpresidents ', 
         'that are not in the new.  ', 
         'Manual review required.')
  }
  USGDPp2$CPI[selCPI] <- USCPI[,2]
}

## ----CPIref-------------------------------------------------------------------
if(Update){
  readLines(csv2[1], n=4)
}

## ----GDPdeflator--------------------------------------------------------------
if(Update){
  selGDP <- (USGDPp2$Year %in% USGDP$Year)
#
  if(any(!is.na(USGDPp2[!selGDP, 'GDPdeflator']))){
    stop('ERROR:  There are GDPdeflator numbers ', 
         'in the current USGDPpresidents ', 
         'that are not in the new.  ', 
         'Manual review required.')
  }
  selDefl <- grep('Deflator', names(USGDP))
  USGDPp2$GDPdeflator[selGDP] <- USGDP[,selDefl]
  print(names(USGDP)[selDefl])
}

## ----pop----------------------------------------------------------------------
if(Update){
  selPop <- grep('Population', names(USGDP))
  USGDPp2$population.K[selGDP] <- USGDP[,selPop]
  print(names(USGDP)[selPop])
}

## ----GDPperCap----------------------------------------------------------------
if(Update){
  if(any(!is.na(USGDPp2[!selGDP, 'readGDPperCapita']))){
    stop('ERROR:  There are realGDPperCapita numbers ', 
         'in the current USGDPpresidents ', 
         'that are not in the new.  ', 
         'Manual review required.')
  }
  selGDPperC <- grep('Real.GDP.per.c', names(USGDP))
  USGDPp2$realGDPperCapita[selGDP] <- USGDP[,selGDPperC]
  print(names(USGDP)[selGDPperC])
}

## ----executive----------------------------------------------------------------
if(Update){
  exec <- as.character(USGDPp2$executive)
  exec[is.na(exec)] <- c('Obama', 'Trump', 'Trump')
  lvlexec <- c(levels(USGDPp2$executive), 
               'Trump')
  USGDPp2$executive <- ordered(exec, lvlexec)
}

## ----war----------------------------------------------------------------------
if(Update){
  war <- as.character(USGDPp2$war)
  war[is.na(war)] <- ''
  lvlwar <- levels(USGDPp2$war)
  USGDPp2$war <- ordered(war, lvlwar)
}

## ----battleDeaths-------------------------------------------------------------
if(Update){
  USGDPp2$battleDeaths[iNew] <- 0 
#
  USGDPp2$battleDeathsPMP <- with(USGDPp2, 
          1000*battleDeaths/population.K) 
}

## ----Keynes-------------------------------------------------------------------
if(Update){
  USGDPp2$Keynes[iNew] <- 0 
}

## ----unemp--------------------------------------------------------------------
if(Update){
  USGDPp2$unemployment[iNew] <- c(4.875, 
                    4.35, 3.89166666666667)
  USGDPp2$unempSource[iNew] <- USGDPp2$unempSource[
    iNew[1]-1]
  tail(USGDPp2)
}

## ----save---------------------------------------------------------------------
if(Update){
  USGDPpresidents <- USGDPp2
  save(USGDPpresidents, file='USGDPpresidents.rda')
}

