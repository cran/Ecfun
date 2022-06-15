## ----setup, include=FALSE, echo=FALSE-----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----yrSpan-------------------------------------------------------------------
library(Ecdat)
(rngYrs <- range(USGDPpresidents$Year))

## ----csv----------------------------------------------------------------------
getwd()
(csv2 <- dir(pattern='\\.csv$'))
(CPIcsvs <- grep('^USCPI', csv2, value=TRUE))
(CPIcsv <- tail(CPIcsvs, 1))

(GDPcsvs <- grep('^USGDP', csv2, value=TRUE))
(GDPcsv <- tail(GDPcsvs, 1))

if((length(CPIcsv)==1) & (length(GDPcsv)==1)){
  Update0 <- TRUE
} else Update0 <- FALSE

## ----read.csv-----------------------------------------------------------------
Update <- FALSE
if(Update0){
  str(USCPI <- read.csv(CPIcsv, skip=2))
  str(USGDP. <- read.csv(GDPcsv, skip=1))
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
  readLines(CPIcsv, n=4)
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
  exec[is.na(exec)] <- c('Trump', 'Trump', 'Biden')
  lvlexec <- c(levels(USGDPp2$executive), 
               'Biden')
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

## ----xlsx---------------------------------------------------------------------
if(Update){
  (xls <- dir(pattern='\\.xlsx$'))
  (BLSxls <- grep('^Series', xls, value=TRUE))
}

## ----readBLS------------------------------------------------------------------
library(readxl)
if(Update){
  str(BLS <- read_excel(BLSxls, skip=11))
}

## ----AnnUnemp-----------------------------------------------------------------
if(Update){
  UNEMP <- as.matrix(BLS[2:13])
  str(unemp <- apply(UNEMP, 1, mean))
}

## ----unemp--------------------------------------------------------------------
if(Update){
  selU4GDP <- (USGDPp2$Year %in% BLS$Year)
  selBLS <- (BLS$Year %in% USGDPp2$Year)
  USGDPp2[selU4GDP, 'unemployment'] <- 
          unemp[selBLS]
#  USGDPp2$unemployment[iNew] <- c(4.875, 
#                    4.35, 3.89166666666667)
  USGDPp2$unempSource[iNew] <- USGDPp2$unempSource[
    iNew[1]-1]
  tail(USGDPp2)
}

## ----ods, eval=FALSE----------------------------------------------------------
#  if(Update){
#    (odsFile <- dir(pattern='\\.ods'))
#    (odsF <- grep('^hstat', odsFile, value=TRUE))
#  }

## ----readods, eval=FALSE------------------------------------------------------
#  if(Update){
#    library(readODS)
#    str(hstat <- read_ods(odsF, sheet='Receipts', skip=2))
#  }

## ----sortOld, eval=FALSE------------------------------------------------------
#  if(Update){
#    Hstat <- hstat[!is.na(hstat$Year), 1:3]
#    oOld <- order(Hstat$Year)
#    head(Hst <- Hstat[oOld, ])
#  }

## ----addNewVars, eval=FALSE---------------------------------------------------
#  if(Update){
#    USGDPp2$fedReceipts <- NA
#    USGDPp2$fedOutlays <- NA
#    selGDP4Hst <- (USGDPp2$Year %in% Hst$Year)
#    USGDPp2[selGDP4Hst, c("fedReceipts", "fedOutlays")] <-
#        (Hst[2:3] / 1000)
#    USGDPp2[c('Year', 'fedReceipts', 'fedOutlays')]
#  }

## ----BudgetFile---------------------------------------------------------------
if(Update){
  (BudgetFiles <- grep('^BUDGET', xls, value=TRUE))
  (BudgetF2_1 <- grep('2-1', BudgetFiles, value=TRUE))
  (BudgetFile <- tail(BudgetF2_1, 1))
}

## ----readBudget---------------------------------------------------------------
if(Update){
  str(Budget <- read_excel(BudgetFile, skip=3))
}

## ----drop2--------------------------------------------------------------------
if(Update){
  library(Ecfun)
  str(Budg <- asNumericDF(Budget[-(1:2), 1:3]))
}

## ----updateBudget-------------------------------------------------------------
if(Update){
  selGDP4budg <- (USGDPp2$Year %in% Budg[, 1])
  selBudg <- (Budg[, 1] %in% USGDPp2$Year)
  USGDPp2[selGDP4budg, 
    c('fedReceipts', 'fedOutlays')] <- Budg[selBudg, 2:3]
}

## ----fedOutlays_pGDP----------------------------------------------------------
if(Update){
  sum(i1843 <- (USGDP$Year==1843))
  GDPnom <- (USGDP$Nominal.GDP..million.of.Dollars.
          / (1+i1843))
  plot(USGDP$Year, GDPnom, type='l', log='y')
  abline(v=1843)

  fedOp <- (USGDPp2$fedOutlays[selGDP] / GDPnom)
  plot(USGDP$Year, fedOp, type='l', log='y')

  USGDPp2$fedOutlays_pGDP <- NA
  USGDPp2$fedOutlays_pGDP[selGDP] <- fedOp
}

## ----USGDPpresNew-------------------------------------------------------------
if(Update){
  USGDPpresidents <- USGDPp2

  sel <- !is.na(USGDPpresidents$fedOutlays_pGDP)
  plot(100*fedOutlays_pGDP~Year, 
     USGDPpresidents[sel,], type='l', log='y', 
     xlab='', ylab='US federal outlays, % of GDP')
  abline(h=2:3)
  war <- (USGDPpresidents$war !='')
  abline(v=USGDPpresidents$Year[war], 
    lty='dotted', col='light gray')
  abline(v=c(1929, 1933), col='red', lty='dotted')
  text(1931, 22, 'Hoover', srt=90, col='red')
}

## ----save---------------------------------------------------------------------
if(Update){
  save(USGDPpresidents, file='USGDPpresidents.rda')
  getwd()
}

