readDW_NOMINATE <- function(file=
  "https://voteview.com/static/data/out/members/HSall_members.csv", 
  ...){
  dwn <- readr::read_csv(file, ...)
  dwn$Year <- (2*dwn$congress + 1787)
  dwn
}
