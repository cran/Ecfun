## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----events-------------------------------------------------------------------
str(eventDates <- c(as.Date(
  c('1962-10-16', '1983-09-26')), Sys.Date()))
(daysBetween <- difftime(tail(eventDates, -1), 
        head(eventDates, -1), units='days'))
(yearsBetween <- as.numeric(daysBetween)/365.24)
names(yearsBetween) <- c('observed', 'censored')
str(yearsBetween)

## ----lik----------------------------------------------------------------------
Lik <- function(lambda, Times=yearsBetween){
  Lik <- (exp(-sum(Times)/lambda) / 
      (lambda^(length(Times)-1)))
  Lik
}

## ----logLik-------------------------------------------------------------------
logLk <- function(lambda, Times=yearsBetween){
  logL <- (-sum(Times)/lambda - 
        (length(Times)-1)*log(lambda))
  logL
}

## ----mle----------------------------------------------------------------------
(lambdaHat <- (sum(yearsBetween) / 
                 (length(yearsBetween)-1)))

## ----chisq--------------------------------------------------------------------
#(chisq2 <- qchisq(
#             c(.8, .95, .99, .999, 1-1e-6), 1))
(chisq2 <- qchisq(c(.2, .05, .01, .001, 1e-6), 
                  1, lower.tail=FALSE))

## ----CI-----------------------------------------------------------------------
lambda <- lambdaHat+seq(-50, 1000, 50)
(logLR2 <- 2*(logLk(lambdaHat) - logLk(lambda)))

## ----CIlog--------------------------------------------------------------------
l_lam <- log(lambdaHat)+seq(-2, 6, 1)
(logLR2_u <- 2*(logLk(lambdaHat) - logLk(exp(l_lam))))

## ----CItheta------------------------------------------------------------------
theta <- (1/lambdaHat + seq(-.016, .06, .004))
(logLR2_th <- 2*(logLk(lambdaHat) - logLk(1/theta)))

## ----plot_u-------------------------------------------------------------------
makePlots <- FALSE 

library(grDevices)
outType <- ''
#outType = 'png'

switch(outType, 
       svg=svg('yrs2Armageddon.svg'), 
#   need png(..., 960, 960), because the default 480 
#   is not sufficiently clear to easily read the labels       
       png=png('yrs2Armageddon.png', 960, 960)
)
op <- par(mar=c(6, 4, 4, 2)+.1)

# Experiment with the range of "seq" here until 
# head and tail of logLR2_u. are just over 6.63:
u. <- log(lambdaHat)+seq(-1.86, 4.36, .02)
lam. <- exp(u.)
logLR2_u. <- 2*(logLk(lambdaHat) - logLk(lam.))
head(logLR2_u., 1)
tail(logLR2_u., 1)

if(makePlots){
  plot(lam., logLR2_u., type='l', bty='n', log='x', 
     xlab='', ylab='', las=1, axes=FALSE, lwd=2)
  axis(1, padj=-1)
  axis(2, las=1)

# xlab = \lambda:  
# Greek letters did not render in GIMP 2.10.8 on 2018-12-30, 
# so don't use svg until this is fixed.  
  switch(outType, 
# cex doesn't work properly with svg > GIMP 
# Therefore, I can NOT use svg
    svg={cex2 <- 2; mtext('lambda', 1, 1.6, cex=cex2)}, 
    png={cex2 <- 2; mtext(expression(lambda), 1, 
                        1.6, cex=cex2)}, 
    {cex2 <- 1.3; mtext(expression(lambda), 1, 
                       1.6, cex=cex2)}
  )

  lamTicks <- axTicks(1)
  thTicks <- 1/lamTicks
  axis(1, lamTicks, thTicks, line=3, padj=-1)
  switch(outType, 
    svg=mtext('theta == 1/lambda', 1, 4.9, cex=cex2), 
    mtext(expression(theta == 1/lambda), 1, 4.9, cex=cex2)
  )

  abline(h=chisq2, col='red', lty=c('dotted', 'dashed'), 
       lwd=2)
  (CI.8 <- range(lam.[logLR2_u. <= chisq2[1]]))
  text(lambdaHat, chisq2[1], 
     paste0('80% CI =\n(', 
       paste(round(CI.8), collapse=', '), ')'), 
     cex=cex2)

  (CI.95 <- range(lam.[logLR2_u. <= chisq2[2]]))
  text(lambdaHat, chisq2[2], 
     paste0('95% CI =\n(', 
       paste(round(CI.95), collapse=', '), ')'), 
     cex=cex2)
  abline(v=CI.8, col='red', lty='dotted', lwd=2)
  abline(v=CI.95, col='red', lty='dashed', lwd=2)

  (CI.99 <- range(lam.[logLR2_u. <= chisq2[3]]))
  text(lambdaHat, chisq2[3], 
     paste0('99% CI =\n(', 
       paste(round(CI.99), collapse=', '), ')'), 
     cex=cex2)

  abline(v=CI.8, col='red', lty='dotted', lwd=2)
  abline(v=CI.95, col='red', lty='dashed', lwd=2)
  abline(v=CI.99, col='red', lty='dashed', lwd=2)
  if(outType != '')dev.off()

  par(op)
}

## ----plot_lin-----------------------------------------------------------------
# copy the code from the last snippet 
# and delete "log='x'", then adjust the placement 
# of CI text
switch(outType, 
       svg=svg('yrs2Armageddon_lin.svg'), 
#   need png(..., 960, 960), because the default 480 
#   is not sufficiently clear to easily read the labels
       png=png('yrs2Armageddon_lin.png', 960, 960)
)
op <- par(mar=c(6, 4, 4, 2)+.1)

u. <- log(lambdaHat)+seq(-1.86, 4.36, .02)
lam. <- exp(u.)
logLR2_u. <- 2*(logLk(lambdaHat) - logLk(lam.))
head(logLR2_u., 1)
tail(logLR2_u., 1)

if(makePlots){
  plot(lam., logLR2_u., type='l', bty='n', 
     xlab='', ylab='', las=1, axes=FALSE, lwd=2)
  axis(1, padj=-1)
  axis(2, las=1)

# xlab = \lambda:  
# Greek letters did not render in GIMP 2.10.8 on 2018-12-30, 
# so don't use svg until this is fixed.  
  switch(outType, 
# cex doesn't work properly with svg > GIMP 
# Therefore, I can NOT use svg
    svg={cex2 <- 2; mtext('lambda', 1, 1.6, cex=cex2)}, 
    png={cex2 <- 2; mtext(expression(lambda), 1, 
                          1.6, cex=cex2)}, 
    {cex2 <- 1.3; mtext(expression(lambda), 1, 
                        1.6, cex=cex2)}
  )

  lamTicks <- axTicks(1)
  thTicks <- 1/lamTicks
  axis(1, lamTicks, thTicks, line=3, padj=-1)
  switch(outType, 
    svg=mtext('theta == 1/lambda', 1, 4.9, cex=cex2), 
    mtext(expression(theta == 1/lambda), 1, 4.9, cex=cex2)
  )

  abline(h=chisq2, col='red', lty=c('dotted', 'dashed'), 
       lwd=2)
  (CI.8 <- range(lam.[logLR2_u. <= chisq2[1]]))
#text(lambdaHat, chisq2[1], 
  text(400, chisq2[1], 
      paste0('80% CI =\n(', 
       paste(round(CI.8), collapse=', '), ')'), 
     cex=cex2)

  (CI.95 <- range(lam.[logLR2_u. <= chisq2[2]]))
#text(lambdaHat, chisq2[2], 
  text(800, chisq2[2], 
     paste0('95% CI =\n(', 
       paste(round(CI.95), collapse=', '), ')'), 
     cex=cex2)
  abline(v=CI.8, col='red', lty='dotted', lwd=2)
  abline(v=CI.95, col='red', lty='dashed', lwd=2)

  (CI.99 <- range(lam.[logLR2_u. <= chisq2[3]]))
#text(lambdaHat, chisq2[3], 
  text(3000, chisq2[3],      
     paste0('99% CI =\n(', 
       paste(round(CI.99), collapse=', '), ')'), 
     cex=cex2)

  abline(v=CI.8, col='red', lty='dotted', lwd=2)
  abline(v=CI.95, col='red', lty='dashed', lwd=2)
  abline(v=CI.99, col='red', lty='dashed', lwd=2)

  if(outType != '')dev.off()
}
par(op)

## ----plot_inverse-------------------------------------------------------------
switch(outType, 
       svg=svg('yrs2Armageddon_inverse.svg'), 
       png=png('yrs2Armageddon_inverse.png', 960, 960)
)
op <- par(mar=c(6, 4, 4, 2)+.1)

# This will require more changes than just deleting log='x':
u. <- log(lambdaHat)+seq(-1.86, 4.36, .02)
lam. <- exp(u.)
logLR2_u. <- 2*(logLk(lambdaHat) - logLk(lam.))
head(logLR2_u., 1)
tail(logLR2_u., 1)

if(makePlots){
  plot(-1/lam., logLR2_u., type='l', bty='n', 
     xlab='', ylab='', las=1, axes=FALSE, lwd=2)

  thTicks <- (-axTicks(1))
  axis(1, -thTicks, abs(1/thTicks), padj=-1)
  axis(2, las=1)

# xlab = \lambda:  
# Greek letters did not render in GIMP 2.10.8 on 2018-12-30, 
# so don't use svg until this is fixed.  
  switch(outType, 
# cex doesn't work properly with svg > GIMP 
# Therefore, I can NOT use svg
    svg={cex2 <- 2; mtext('lambda', 1, 1.6, cex=cex2)}, 
    png={cex2 <- 2; mtext(expression(lambda), 1, 
                          1.6, cex=cex2)}, 
    {cex2 <- 1.3; mtext(expression(lambda), 1, 
                        1.6, cex=cex2)}
  )

  axis(1, -thTicks, thTicks, line=3, padj=-1)
  switch(outType, 
    svg=mtext('theta == 1/lambda', 1, 4.9, cex=cex2), 
    mtext(expression(theta == 1/lambda), 1, 4.9, cex=cex2)
  )

  abline(h=chisq2, col='red', lty=c('dotted', 'dashed'), 
       lwd=2)
  (CI.8 <- range(lam.[logLR2_u. <= chisq2[1]]))
#text(lambdaHat, chisq2[1], 
  text(-.02, chisq2[1], 
      paste0('80% CI =\n(', 
       paste(round(CI.8), collapse=', '), ')'), 
     cex=cex2)

  (CI.95 <- range(lam.[logLR2_u. <= chisq2[2]]))
#text(lambdaHat, chisq2[2], 
  text(-.04, chisq2[2], 
     paste0('95% CI =\n(', 
       paste(round(CI.95), collapse=', '), ')'), 
     cex=cex2)
  abline(v=CI.8, col='red', lty='dotted', lwd=2)
  abline(v=CI.95, col='red', lty='dashed', lwd=2)

  (CI.99 <- range(lam.[logLR2_u. <= chisq2[3]]))
#text(lambdaHat, chisq2[3], 
  text(-.06, chisq2[3],      
     paste0('99% CI =\n(', 
       paste(round(CI.99), collapse=', '), ')'), 
     cex=cex2)

  abline(v=-1/CI.8, col='red', lty='dotted', lwd=2)
  abline(v=-1/CI.95, col='red', lty='dashed', lwd=2)
  abline(v=-1/CI.99, col='red', lty='dashed', lwd=2)

  if(outType != '')dev.off()
}
par(op)

## ----simExp-------------------------------------------------------------------
set.seed(1)
simExp <- rexp(1000)
if(makePlots){
  qqnorm(simExp, datax=TRUE)
  qqnorm(simExp, datax=TRUE, log='x')
  qqnorm(1/simExp, datax=TRUE)
}

## ----rlam---------------------------------------------------------------------
library(invgamma)
set.seed(123)

rlambda2 <- function(n, sumTimes=lambdaHat){
#  -sumTimes/log(runif(n))
# k <- 2; rinvgamma(n k-1, scale=sumTimes)  
  rinvgamma(n, 1, rate=sumTimes)
}
simLam <- rlambda2(1e4)
quantile(simLam)
mean(simLam)

## ----betaSSQ------------------------------------------------------------------
Dev2 <- function(shapes, p=c(.3, .6), q=c(.1, .9)){
  devs <- (q - pbeta(p, shapes[1], shapes[2]))
  sum(devs^2)
}
# test
Dev2(c(1, 1))

## ----betaSolve----------------------------------------------------------------
(betaSolve <-optim(c(1,1), Dev2, 
            method="L-BFGS-B", lower=c(0,0)))

## ----meanBeta-----------------------------------------------------------------
a.b <- sum(betaSolve$par)
(meanBeta <- betaSolve$par[1]/a.b)
(varBeta <- with(betaSolve, par[1]*par[2] / 
            (a.b^2 * (a.b+1))))

## ----mcArmageddon-------------------------------------------------------------
mcArmageddon <- function(N, 
    betapars=betaSolve$par, 
    sumTimes=lambdaHat){
# 1.  Start time
  start <- proc.time()
# 2.  Q ~ B(\alpha, \beta)
  Q <- rbeta(N, betapars[1], betapars[2])
# 3.  K ~ NB(Q, 1)
  K <- (1+rnbinom(N, 1, Q))
# 4.  Time[i] <- sum(rlambda2(K[i]))
  Time <- numeric(N)
  for(i in 1:N){
    Time[i] <- sum(rlambda2(K[i], 
          sumTimes=sumTimes))
  }
  attr(Time, 'Qbar') <- mean(Q)
  attr(Time, 'quantileQ') <- quantile(Q)
  attr(Time, 'Kbar') <- mean(K)
  attr(Time, 'quantileK') <- quantile(K)
# 5.  quantiles
  cat('meanTime = ', mean(Time), '\n')
  print(quantile(Time))
# 6.  elapsed.time 
  et <- (proc.time()-start)
# 7.  Return et as an attribute
  attr(Time, 'elapsed.time') <- et
  cat('et = ', et, '\n')
  Time
}  

set.seed(1)
(mcArm1 <- mcArmageddon(10))

## ----mcArm2-------------------------------------------------------------------
set.seed(2)
mcArm2 <- mcArmageddon(100)
attributes(mcArm2)

## ----mcArm3-------------------------------------------------------------------
set.seed(3)
mcArm3 <- mcArmageddon(1000)
attributes(mcArm3)

## ----mcArm4-------------------------------------------------------------------
set.seed(4)
mcArm4 <- mcArmageddon(1e4)
attributes(mcArm4)

## ----mcArm5-------------------------------------------------------------------
set.seed(5)
mcArm5 <- mcArmageddon(1e5)
attributes(mcArm5)

## ----million------------------------------------------------------------------
set.seed(6)
mcArm6 <- mcArmageddon(1e6)
attributes(mcArm6)

## ----MCsumStats---------------------------------------------------------------
mean(mcArm6)
mean(mcArm6<40)
mean(mcArm6<60)
quantile(mcArm6, c(1e-6, 1e-3))

## ----1e7----------------------------------------------------------------------
if(!fda::CRAN()){
# Don't run this with CRAN tests, 
# because it takes too long   
  set.seed(7)
  mcArm7 <- mcArmageddon(1e7)
  print(attributes(mcArm7))
  print(mean(mcArm7))
  print(quantile(mcArm7, c(1e-6, 1e-3)))
}

## ----sort7--------------------------------------------------------------------
if(fda::CRAN()){
  mcArm. <- mcArm6
} else mcArm. <- mcArm7

mcArm.s <- sort(mcArm.)
quantile(mcArm.s)

## ----qqnorm7------------------------------------------------------------------
str(qq7 <-as.data.frame(qqnorm(mcArm.s,
                          plot.it=FALSE)))

## ----indx---------------------------------------------------------------------
N. <- length(mcArm.s)
#index.5 <- c(1:10, seq(20, 100, 10), 
#             seq(200, 1000, 100), 
#             seq(3000, (N./2)-2000, 1000))
index.5 <- c(1:1000, 
        seq(2000, (N./2)-2000, 1000))
index <- c(index.5, N.+1-rev(index.5))
tail(index, 30) 
length(index)
# yes:  I think I did this right.  

## ----plot7--------------------------------------------------------------------
switch(outType, 
       svg=svg('yrs2ArmageddonQQ.svg'), 
#   need png(..., 960, 960), 
#     because the default 480 is not sufficiently
#     clear to easily read the labels       
       png=png('yrs2ArmageddonQQ.png', 960, 960)
)
op <- par(mar=c(5, 5, 4, 5)+.1)

if(makePlots){
  with(qq7, plot(y[index], x[index], type='l', 
               log='x', las=1, bty='n', lwd=2, 
               xlab='', ylab='', 
               cex.lab=2, axes=FALSE) )
#               xlab='years to Armageddon', 
#               ylab='standard normal scores', 
  axis(1, cex.axis=2)
  axis(2, cex.axis=2, las=1)
  probs <- c(.001, .01, .1, .25, .5, .75, 
           .9, .99, .999)
  z <- qnorm(probs)
  if(outType==''){
    cex.txt <- 1.5 
    cex.ax4 <- 1.3
  } else {
    cex.txt <- 3
    cex.ax4 <- 2
  }

  axis(4, z, probs, cex.axis=cex.ax4, 
     las=1, line=-.5)

  p40 <- mean(mcArm.s<40)
  p60 <- mean(mcArm.s<60)
  z40.60 <- qnorm(c(p40, p60))
  max7 <- tail(mcArm.s, 1)

  lines(c(rep(40, 2), max7), 
      c(-5, rep(z40.60[1], 2)), 
      lty='dotted', lwd=2, col='red')
  lines(c(rep(60, 2), max7), 
      c(-5, rep(z40.60[2], 2)), 
      lty='dashed', lwd=2, col='purple')

  text(15, -5, '40', col='red', cex=cex.txt)
  text(200, -2.5, '60', col='purple', cex=cex.txt)

  text(.2*max7, z40.60[1]-.6, 
     paste0(round(100*p40), "%"), cex=cex.txt, 
     col='red')
  text(.2*max7, z40.60[2]+.6, 
     paste0(round(100*p60), "%"), cex=cex.txt, 
     col='purple')
}
par(op)
if(outType != '')dev.off()

