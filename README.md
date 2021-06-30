# Ecfun 

Functions and vignettes to update a few data sets in 'Ecdat' and to create, manipulate, plot, and analyze some of those and similar data sets.
    
# 2021-06-29 
All `read*` functions in this package have been removed, because it wasn't clear if anyone was using them.  Other functions that called an `Ecfun::read*` function by default were also deleted, especially `UShouse.senate`, `USsenateClass`, and `mergeUShouse.senate`.  These all scraped websites, and the demand for them seemed not to justify the work of maintaining them.  

# 2021-05-20
<!-- badges: start -->
[![R-CMD-check](https://github.com/sbgraves237/Ecfun/workflows/R-CMD-check/badge.svg)](https://github.com/sbgraves237/Ecfun/actions)
<!-- badges: end -->

# 2020-01-23
`testURLs` and `read.testURLs` have been removed, 
because it wasn't clear that anyone was using them, 
and more modern tools are available from:  
http://www.measurementlab.net/

Thanks to Iñaki Ucar, Adam H Sparks, and Roy 
Mendelssohn for their replies to a question 
posted to R-Devel helped me understand what 
I needed to do to fix problems identified in 
the CRAN Checks.
