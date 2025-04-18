library(rvest)
library(tidyverse)
library(rlist)

### Getting team names for each player
teamlist <- c( "SEA", "DEN", "DAL", "TB", "NYG", "TEN", "MIN", "GB", "LAC", "LV", "KC",
               "ARI", "JAX", "WAS", "NYJ", "BAL", "NE", "MIA", "IND", "HOU",
               "DET", "PHI", "PIT", "CIN", "CHI", "SF", "CAR", "CLE", "ATL", "NO",
               "BUF", "LA")
filelist <- list()
for(team in teamlist){
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data/Offense by Team")
  firstfilename <- paste0(team, "file.rds")
  mydf <- readRDS(firstfilename)
  checkdf <- mydf |> distinct(displayName, .keep_all = TRUE)
  finaldf <- checkdf |> select(nflId, teamAbbr)
  filelist <- list.append(filelist, finaldf)
  print(team)
}

basefile <- filelist[[1]]
for(i in 2: length(filelist)){
  basefile <- rbind(basefile, filelist[[i]])
}
allplayerswithteam <- unique(basefile)
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
write_csv(allplayerswithteam, "AllPlayerswithTeam.csv")
###

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
playersteam <- read_csv("AllPlayerswithTeam.csv")
  
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
passing <- read_csv("passingnflID+pffID.csv")
rushrecblock <- read_csv("rushrecblocknflID+pffID.csv")
defense <- read_csv("defensenflID+pffID.csv")

newpassing <- left_join(passing, playersteam)
newrushrecblock <- left_join(rushrecblock, playersteam)
newdefense <- left_join(defense, playersteam)

newpassing <- newpassing |> relocate(teamAbbr, .after = nflId)
newrushrecblock <- newrushrecblock |> relocate(teamAbbr, .after = nflId)
newdefense <- newdefense |> relocate(teamAbbr, .after = nflId)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
write_csv(newpassing, "passingnflID+pffID.csv")
write_csv(newrushrecblock, "rushrecblocknflID+pffID.csv")
write_csv(newdefense, "defensenflID+pffID.csv")


# Test for getting player grades for game simulator -----------------------
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
passing <- read_csv("passingnflID+pffID.csv")
rushrecblock <- read_csv("rushrecblocknflID+pffID.csv")
defense <- read_csv("defensenflID+pffID.csv")

test <- passing |> filter(teamAbbr=="DEN" & position=="QB")
newtest <- test |> select(displayName, nflId, attempts)

test2 <- rushrecblock |> filter(teamAbbr=="DEN" & (position=="RB" |
                                                     position=="FB" |
                                                     position=="WR" |
                                                     position=="TE"))
test3 <- rushrecblock |> filter(teamAbbr=="DEN" & position=="OL")




