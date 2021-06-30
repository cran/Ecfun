2021-06-29
Ecfun 0.2-4 

Deleted all `read*` functions and functions that called them be default, because they no longer worked, and the demand for them did not seem to justify the work of fixing them.  Most of these were written to scrape information from specific websites.  They are all still available in the CRAN archives in case anyone cares to resurrect them.  

2020-09-13 
Ecfun 0.2-4 

`financialCrisisFiles` and `readFinancialCrisisFiles` have been removed, because (a) it wasn't clear that anyone was using them, (b) the `gdata` package that was used to read them was not being maintained, and (c) the work required to read them with another package exceeded the need of the maintainer.  If you need them please so inform the maintainer.    

Ecfun 0.2-2

`testURLs` and `read.testURLs` have been removed, 
because it wasn't clear that anyone was using them, 
and more modern tools are available from:  
http://www.measurementlab.net/
