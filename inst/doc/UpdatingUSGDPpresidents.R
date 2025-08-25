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
  sPop <- (USGDP[,selPop]/1000)
  quantile(ePop <- ((USGDPp2$population.K[selGDP] /sPop)-1), 
           na.rm=TRUE)
}

## ----pop2---------------------------------------------------------------------
if(Update){
  USGDPp2$population.K[selGDP] <- sPop
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
  newExec <- 'Biden' 
  exec[is.na(exec)] <- newExec
  lvlexec <- levels(USGDPp2$executive)
  if(!(newExec %in% lvlexec))
    lvlexec <- c(lvlexec, newexec)
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
  dunemp <- (USGDPp2[selU4GDP, 'unemployment'] - 
          unemp[selBLS])
  cbind(USGDPp2[selU4GDP, c('Year', 'unemployment')], 
        unemp[selBLS], dunemp)
}

## ----replaceUnemp-------------------------------------------------------------
if(Update){
  USGDPp2[selU4GDP, 'unemployment'] <- unemp[selBLS]
  USGDPp2$unempSource[iNew] <- USGDPp2$unempSource[
    iNew[1]-1]
  tail(USGDPp2)
}

## ----ods, eval=FALSE----------------------------------------------------------
# if(Update){
#   (odsFile <- dir(pattern='\\.ods'))
#   (odsF <- grep('^hstat', odsFile, value=TRUE))
# }

## ----readods, eval=FALSE------------------------------------------------------
# if(Update){
#   library(readODS)
#   str(hstat <- read_ods(odsF, sheet='Receipts', skip=2))
# }

## ----sortOld, eval=FALSE------------------------------------------------------
# if(Update){
#   Hstat <- hstat[!is.na(hstat$Year), 1:3]
#   oOld <- order(Hstat$Year)
#   head(Hst <- Hstat[oOld, ])
# }

## ----addNewVars, eval=FALSE---------------------------------------------------
# if(Update){
#   USGDPp2$fedReceipts <- NA
#   USGDPp2$fedOutlays <- NA
#   selGDP4Hst <- (USGDPp2$Year %in% Hst$Year)
#   USGDPp2[selGDP4Hst, c("fedReceipts", "fedOutlays")] <-
#       (Hst[2:3] / 1000)
#   USGDPp2[c('Year', 'fedReceipts', 'fedOutlays')]
# }

## ----BudgetFile---------------------------------------------------------------
(xls2 <- dir(pattern='\\.xlsx$'))
if(Update){
  (BudgetFiles <- grep('^BUDGET', xls2, value=TRUE))
  (BudgetF2_1 <- grep('2-1', BudgetFiles, value=TRUE))
  (BudgetFile <- (if(length(BudgetF2_1)>0) 
    tail(BudgetF2_1, 1) else tail(BudgetFiles, 1)))
}

## ----readBudget---------------------------------------------------------------
if(Update){
  Budget <- read_excel(BudgetFile, 
          sheet='hist01z1', skip=3)
  head(Budget)
  tail(Budget)  
}

## ----drop2--------------------------------------------------------------------
if(Update){
  library(Ecfun)
  nBudg0 <- nrow(Budget)
  iBudg <- sort(seq(to=nBudg0-2, length=40))
  str(Budg <- asNumericDF(Budget[iBudg, 1:4]))
  tail(Budg)
}

## ----updateBudget-------------------------------------------------------------
if(Update){
  selGDP4budg <- (USGDPp2$Year %in% Budg[, 1])
  selBudg <- (Budg[, 1] %in% USGDPp2$Year)
  dfedR <- (USGDPp2[selGDP4budg, 'fedReceipts'] 
              - Budg[selBudg, 2])
  dfedO <- (USGDPp2[selGDP4budg, 'fedOutlays'] 
              - Budg[selBudg, 3])
  dfedS <- (USGDPp2[selGDP4budg, 'fedSurplus'] 
              - Budg[selBudg, 4])
  tail(cbind(USGDPp2[selGDP4budg, c('Year', 
          'fedReceipts', 'fedOutlays', 'fedSurplus')], 
       Budg[selBudg, 2:4], dfedR, dfedO, dfedS), 10)
  matplot(cbind(dfedR, dfedO, dfedS), type='l')
}

## ----updateBudget2------------------------------------------------------------
if(Update){
  table(sel2017_2024 <- (USGDPp2$Year %in% 2017:2024))
  table(s2017_2024 <- (Budg[, 1] %in% 2017:2024))

  USGDPp2[sel2017_2024, c('fedReceipts', 'fedOutlays', 
      'fedSurplus')] <- Budg[s2017_2024, 2:4]
  tail(USGDPp2)
}

## ----plotBudget---------------------------------------------------------------
if(Update){
  Xlim <- c(1790, max(USGDPp2$Year, na.rm=TRUE))
  plot(fedReceipts ~Year, USGDPp2, log='y', type='l', 
     xlim=Xlim, las=2)
  Xlim <- c(1790, max(USGDPp2$Year, na.rm=TRUE))

  plot(fedOutlays ~Year, USGDPp2, log='y', type='l', 
     xlim=Xlim, las=2)

  plot(fedSurplus ~Year, USGDPp2, type='l', 
     xlim=Xlim, las=2)
}

## ----debtData-----------------------------------------------------------------
(csv3 <- dir(pattern='\\.csv$'))
if(Update){
  (debtFiles <- grep('^HstDebt', csv3, value=TRUE))
  tail(HstDebt <- read.csv(debtFiles))
  (HstDebt6 <- head(HstDebt))
  tail(USGDPp2[c('Year', 'fedDebt')])
}

## ----debtD--------------------------------------------------------------------
if(Update){
  nobs <- nrow(USGDPp2)
  (endRows <- seq(nobs, by=-1, length=6))
  (dHstDebt6 <- (USGDPp2$fedDebt[endRows]-HstDebt6[, 2]))
}

## ----newDebt------------------------------------------------------------------
if(Update){
  (USGDPp2$fedDebt[endRows] <-HstDebt6[, 2])
  tail(USGDPp2)
  plot(fedDebt ~Year, USGDPp2, type='l', log='y',
     xlim=Xlim, las=2)
}

## ----currentGDP---------------------------------------------------------------
if(Update){
  selEnd <- (USGDPp2$Year>1843) 
  currentGDP <- with(USGDPp2[selEnd, ], 
      1000 * population.K * realGDPperCapita 
          * GDPdeflator / 100)
  plot(USGDPp2$Year[selEnd], currentGDP, 
       log='y', type='l', las=2)
  tail(currentGDP)
}

## ----fedReceipts--------------------------------------------------------------
if(Update){
  plot(fedReceipts~Year, USGDPp2[selEnd, ], log='y', 
     type='l', las=2)
}

## ----fedR_p-------------------------------------------------------------------
if(Update){
  fedR_p <- (1e6*USGDPp2$fedReceipts[selEnd] / 
             currentGDP)
  plot(USGDPp2$Year[selEnd], fedR_p, type='l', 
     las=2, log='y')
  matplot(USGDPp2$Year[selEnd], 
        cbind(USGDPp2$fedReceipts_pGDP[selEnd], fedR_p), 
        type='l', las=2, log='y')
}

## ----fedR_p2------------------------------------------------------------------
if(Update){
  plot(USGDPp2$Year[selEnd], 
      USGDPp2$fedReceipts_pGDP[selEnd] / fedR_p, 
        type='l', las=2, log='y')
}

## ----fedR_p3------------------------------------------------------------------
if(Update){
  USGDPp2$fedReceipts_pGDP[selEnd] <- fedR_p
  tail(USGDPp2)
}

## ----fedO_p-------------------------------------------------------------------
if(Update){
  fedO_p <- (1e6*USGDPp2$fedOutlays[selEnd] / 
             currentGDP)
  matplot(USGDPp2$Year[selEnd], 
        cbind(USGDPp2$fedOutlays_pGDP[selEnd], fedO_p), 
        type='l', las=2, log='y')
}

## ----fedO_p2------------------------------------------------------------------
if(Update){
  plot(USGDPp2$Year[selEnd], 
      USGDPp2$fedOutlays_pGDP[selEnd] / fedO_p, 
        type='l', las=2, log='y')
}

## ----fedO_p3------------------------------------------------------------------
if(Update){
  USGDPp2$fedOutlays_pGDP[selEnd] <- fedO_p
  tail(USGDPp2)
}

## ----fedS_p-------------------------------------------------------------------
if(Update){
  fedS_p <- (1e6*USGDPp2$fedSurplus[selEnd] / 
             currentGDP)
  matplot(USGDPp2$Year[selEnd], 
        cbind(USGDPp2$fedSurplus_pGDP[selEnd], fedS_p), 
        type='l', las=2)
}

## ----fedS_p2------------------------------------------------------------------
if(Update){
  plot(USGDPp2$Year[selEnd], 
      USGDPp2$fedSurplus_pGDP[selEnd] / fedS_p, 
        type='l', las=2)
  quantile(rSup <- (USGDPp2$fedSurplus_pGDP[selEnd] / fedS_p), 
           na.rm=TRUE)
}

## ----fedS_p3------------------------------------------------------------------
if(Update){
  USGDPp2$fedSurplus_pGDP[selEnd] <- fedS_p
  tail(USGDPp2)
}

## ----fedD_p-------------------------------------------------------------------
if(Update){
  fedD_p <- (USGDPp2$fedDebt[selEnd] / 
             currentGDP)
  matplot(USGDPp2$Year[selEnd], 
        cbind(USGDPp2$fedDebt_pGDP[selEnd], fedD_p), 
        type='l', las=2, log='y')
}

## ----fedD_p2------------------------------------------------------------------
if(Update){
  plot(USGDPp2$Year[selEnd], 
      USGDPp2$fedDebt_pGDP[selEnd] / fedD_p, 
        type='l', las=2)
}

## ----fedD_p3------------------------------------------------------------------
if(Update){
  USGDPp2$fedDebt_pGDP[selEnd] <- fedD_p
  tail(USGDPp2)
}

## ----USGDPpresNew-------------------------------------------------------------
if(Update){
  USGDPpresidents <- USGDPp2

  sel <- !is.na(USGDPpresidents$fedOutlays_pGDP)
  plot(100*fedOutlays_pGDP~Year, 
     USGDPpresidents[sel,], type='l', log='y', 
     xlab='', ylab='US federal outlays, % of GDP')
  abline(h=2:3)
  War <- (USGDPpresidents$war !='')
  abline(v=USGDPpresidents$Year[War], 
    lty='dotted', col='light gray')
  abline(v=c(1929, 1933), col='red', lty='dotted')
  text(1931, 22, 'Hoover', srt=90, col='red')
}

## ----Defecit?-----------------------------------------------------------------
if(Update){
  selD <- !is.na(USGDPpresidents$fedSurplus_pGDP)
  plot(-100*fedSurplus_pGDP~Year, 
     USGDPpresidents[sel,], type='l', 
     xlab='', ylab='US federal deficit, % of GDP')
  abline(h=2:3)
  abline(v=USGDPpresidents$Year[War], 
    lty='dotted', col='light gray')
  abline(v=c(1929, 1933), col='red', lty='dotted')
  text(1931, 22, 'Hoover', srt=90, col='red')
}

## ----inflation----------------------------------------------------------------
if(Update){
  selI <- (USGDPpresidents$Year>1789)
  quantile(diff(USGDPpresidents$Year[selI]))
}

## ----infl1--------------------------------------------------------------------
if(Update){
  infl <- 100*diff(log(USGDPpresidents$CPI[selI]))
  yr2 <- USGDPpresidents$Year[selI][-1]
  plot(yr2, infl, type='l', las=2)
  abline(h=c(-2, 0, 2, 10))
  abline(v=USGDPpresidents$Year[War], 
    lty='dotted', col='light gray')
  abline(v=c(1929, 1933), col='red', lty='dotted')
  text(1931, 22, 'Hoover', srt=90, col='red')
}

## ----infl2--------------------------------------------------------------------
if(Update){
  infl2 <- 100*diff(log(
    USGDPpresidents$GDPdeflator[selI]))
  plot(yr2, infl2, type='l', las=2)
  abline(h=c(-2, 0, 2, 10))
  abline(v=USGDPpresidents$Year[War], 
    lty='dotted', col='light gray')
  abline(v=c(1929, 1933), col='red', lty='dotted')
  text(1931, 22, 'Hoover', srt=90, col='red')
}

## ----battleDeaths2------------------------------------------------------------
if(Update){
  plot(battleDeathsPMP~Year, USGDPpresidents,  
       type='l', las=2, xlim=Xlim)
  abline(h=100)
  
  plot(1+battleDeathsPMP~Year, USGDPpresidents,  
       type='l', las=2, xlim=Xlim, log='y')
  abline(h=100)
  abline(v=USGDPpresidents$Year[War], 
    lty='dotted', col='light gray')
  abline(v=c(1929, 1933), col='red', lty='dotted')
  text(1931, 22, 'Hoover', srt=90, col='red')
}

## ----save---------------------------------------------------------------------
if(Update){
  save(USGDPpresidents, file='USGDPpresidents.rda')
  getwd()
}

