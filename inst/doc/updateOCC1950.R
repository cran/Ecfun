## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----ipums--------------------------------------------------------------------
# The code in this snippet is a slight modification 
# of the R code from usa_00006.R, 2020-03-18.  
if (!require("ipumsr")){
  msg <- paste("Reading IPUMS data into R", 
    "requires the ipumsr package. It can be",
    "installed using:\ninstall.packages('ipumsr')")
  stop(msg)
}

# NOTE:  base::dir works differently  
# within an R Markdown file than 
# from an ordinary command prompt, 
# at least under macOS 10.15.3 on 2020-03-18.  
# With getwd() = the parent directory of 
# "~/fda/vignettes", 
# When I highlighted "dir()" and executed it
# using <command + enter>, I got  the contents 
# of "~/fda/vignettes".
# When I executed "dir()" outside the *.Rmd file, 
# I got the parent directory.  

dir()
dir(getwd())

# copy and paste the following 
# from "Accountants-IPUMS.Rmd" in R Studio 
# into the Console below:  

IPUMSdir <- 'IPUMS'
(ddiXml <- dir(IPUMSdir, pattern="usa_00006.xml", 
      full.names = TRUE))
# OR execute the following inside 
# "Accountants-IPUMS.Rmd" in R Studio:  
if(length(ddiXml)!=1){
#  print(ddiXml <- file.path('..', '..', 'IPUMS'))
  print(ddiXml <- dir(pattern="usa_00006.xml",      
      full.names = TRUE))
# NOTE:  This worked under macOS 10.15.3 
# with R 3.6.3 and RStudio 1.2.5033.  
# It has not been tested on other platforms.  
}

## ----skipOnCRAN---------------------------------------------------------------
readAndCompute <- FALSE
if((length(ddiXml)==1) && (!fda::CRAN())){
  readAndCompute <- TRUE
  ddiDat <- read_ipums_ddi(ddiXml)
  (readDatTime <- system.time(
    IPUMSdata <- read_ipums_micro(ddiDat)
  ))
}

## ----data---------------------------------------------------------------------
if(readAndCompute){
  str(IPUMSdata)
  nrow(IPUMSdata)/1e6
}

## ----tbl_year-----------------------------------------------------------------
if(readAndCompute){
  print(etYr <-  system.time(
    tbl_year <- table(IPUMSdata$YEAR)
  ))
  plot(tbl_year)
  tbl_year
}

## ----IPUMSna------------------------------------------------------------------
if(readAndCompute){
  print(etNA <-  system.time(
    nNA <- sapply(IPUMSdata, function(x)sum(is.na(x)))
  ))
  print(nNA)
}

## ----HHWT---------------------------------------------------------------------
if(readAndCompute){
  attributes(IPUMSdata$HHWT)
}

## ----q_HHWT-------------------------------------------------------------------
if(readAndCompute){
  print(etQ <-  system.time({
    rngHHWT <- range(IPUMSdata$HHWT)
    qtleHHWT <- quantile(IPUMSdata$HHWT)
  }))
  print(rngHHWT)
  qtleHHWT
}

## ----OCC1950------------------------------------------------------------------
if(readAndCompute){
  print(etCodes <-  system.time(
    OCC50codes <- attributes(IPUMSdata$OCC1950)
  ))
  str(OCC50codes)
}

## ----OCClbls------------------------------------------------------------------
if(readAndCompute){
#  OCCcodes$labels
  print(OCC50codes$var_desc)
  print(head(OCC50codes$labels))
  tail(OCC50codes$labels)
}

## ----tabOcc-------------------------------------------------------------------
if(readAndCompute){
  print(etOcc1 <-  system.time(
    Occ1 <- table(IPUMSdata$OCC1950)
  ))
  str(Occ1)
}

## ----unusedOcc----------------------------------------------------------------
if(readAndCompute){
  OCC50codes$labels[!(
    OCC50codes$labels %in% names(Occ1))]
}

## ----tabYrOcc0----------------------------------------------------------------
if(readAndCompute){
  print(etOccYr <-  system.time(
    OccYr <- tapply(IPUMSdata$HHWT, 
        IPUMSdata[c("OCC1950", "YEAR")], sum)
  ))
  str(OccYr)
}

## ----totWts0------------------------------------------------------------------
if(readAndCompute){
  (totWts <- colSums(OccYr))
}

## ----tabOY--------------------------------------------------------------------
if(readAndCompute){
  print(etOY <-  system.time(
    OY <- with(IPUMSdata, table(OCC1950, YEAR))  
  ))
  print(str(OY))
  sum(is.na(OccYr) - (OY==0))
}

## ----OY0----------------------------------------------------------------------
if(readAndCompute){
  OccYr[is.na(OccYr)] <- 0
}

## ----totWts-------------------------------------------------------------------
if(readAndCompute){
  (totWts <- colSums(OccYr))
  library(Ecdat)
  selGDP <- (USGDPpresidents$Year %in% names(totWts))
  USpops <- USGDPpresidents[selGDP, ]
  ylim <- range(totWts/1e6, USpops$population.K/1e3)
# png('IPUMS HHWT and US Population.png')  
  plot(names(totWts), totWts/1e6, xlab='',
       ylab="millions", 
       main='sum(HHWT) vs. US Population', las=1)
  with(USpops, lines(Year, population.K/1e3))
# dev.off()
}

## ----totWts2------------------------------------------------------------------
if(readAndCompute){
  tot_pop <- (totWts / (USpops$population.K*1000))
  plot(USpops$Year, tot_pop, type='b', las=1)
  abline(h=1, lty='dotted', col='red')
}

## ----Occ1950------------------------------------------------------------------
if(readAndCompute){
  nOcc <- nrow(OccYr)
  Occ1950 <- (OccYr / rep(tot_pop, e=nOcc))
  (revTots <- colSums(Occ1950))
  plot(USpops$Year, revTots/1e6, type='b', las=1)
}

## ----OCC1950mat---------------------------------------------------------------
if(readAndCompute){
  OCC1950 <- (Occ1950 / rep(revTots, e=nOcc))
  quantile(chkTots <- colSums(OCC1950))
}

## ----OCC1950mt----------------------------------------------------------------
if(readAndCompute){
  rownames(OCC1950) <- names(Occ1)
}

## ----save---------------------------------------------------------------------
if(readAndCompute){
  save(OCC1950, file='OCC1950.rda')
  dir(full=TRUE)
}

