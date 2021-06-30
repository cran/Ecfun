readFinancialCrisisFiles <- function(files, crisisType=7, 
                                     ...){
  cat('This function was deleted, because it used gdata,\n')
  cat('which was scheduled to be deleted from CRAN\n')
  cat('with all packages that used it, and it did not\n')
  cat('seem to be used enought to justify fixing it.\n')
  cat('If you need it, try Ecfun 0.2-0.')
  return(data.frame(year=0, crisisType=0))
}

readCookPVI <- function(url.=
    "https://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index"){
  cat('This function was deleted, because it ')
  cat('scraped information from a web site,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain it.\n')
  cat('If you need it, try Ecfun 0.2-0.')
  stop()  
}
readCookPVI. <- function(url.=
  "https://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
  UShouse=readUShouse(), USsenate=readUSsenate(), ...){
  cat('This function was deleted, because it ')
  cat('scraped information from a web site,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain it.\n')
  cat('If you need it, try Ecfun 0.2-0.')
  stop()  
}
readUShouse <- function(url.=
      "https://www.house.gov/representatives/",
      nonvoting=c('American Samoa', 'District of Columbia',
         'Guam', 'Northern Mariana Islands', 'Puerto Rico',
         'Virgin Islands'),
      fixNonStandard=subNonStandardNames, ...){
  cat('This function was deleted, because it ')
  cat('scraped information from a web site,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain it.\n')
  cat('If you need it, try Ecfun 0.2-0.')
  stop()  
}
readUSsenate <- function(url.=
  "https://www.senate.gov/general/contact_information/senators_cfm.xml",
  stateAbbreviations=Ecdat::USstateAbbreviations,
  fixNonStandard=subNonStandardNames, ...){
  cat('This function was deleted, because it ')
  cat('scraped information from a web site,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain it.\n')
  cat('If you need it, try Ecfun 0.2-0.')
  stop()  
}
readUSstateAbbreviations <- function(url.=
  "https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations",
  clean=TRUE, Names=c('Name', 'Status', 'ISO', 'ANSI.letters',
  'ANSI.digits', 'USPS', 'USCG', 'Old.GPO', 'AP', 'Other') ){
  cat('This function was deleted, because it ')
  cat('scraped information from a web site,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain it.\n')
  cat('If you need it, try Ecfun 0.2-0.')
  stop()  
}
UShouse.senate <- function(house=readUShouse(), 
                           senate=readUSsenate()){
  cat('This function was deleted, because by default, ')
  cat('it called readUShouse() and readUSsenate, which ')
  cat('scraped information from web sites,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain these functions.\n')
  cat('If you need them, try Ecfun 0.2-0.')
  stop()  
}
USsenateClass <- function(x, senate=readUSsenate(),
  Office='Office', state='state',
  surname='surname', district='district',
  senatePattern='^Senate') {
  cat('This function was deleted, because by default, ')
  cat('it called readUSsenate, which ')
  cat('scraped information from a web site,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain these functions.\n')
  cat('If you need them, try Ecfun 0.2-0.')
  stop()  
}  
mergeUShouse.senate <- function(x, UScongress=UShouse.senate(),
      newrows='amount0',
      default=list(member=FALSE, amount=0, #vote="notEligible",
      incumbent=TRUE) ){
  cat('This function was deleted, because by default, ')
  cat('it called readUShouse() and readUSsenate, which ')
  cat('scraped information from web sites,\n')
  cat('and the need for the information seemed insufficient ')
  cat('to justify the work required to maintain these functions.\n')
  cat('If you need them, try Ecfun 0.2-0.')
  stop()  
}
