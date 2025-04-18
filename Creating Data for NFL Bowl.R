library(tidyverse)
library(rlist)


setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data")

playertracking <- readRDS("all_tracking.rds")
player_playsdf <- read_csv("player_play.csv")
gamesdf <- read_csv("games.csv")
playersdf <- read_csv("players.csv")
playsdf <- read_csv("plays.csv")

dummyplayersdf <- playersdf |> select(nflId, position)


# Adding players in (I don't think this is actually necessary til later --------

newdf1 <- left_join(player_playsdf, dummyplayersdf, 
                    by = c("pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
newdf1 <- newdf1 |> relocate(displayName, .after = pff_primaryDefensiveCoverageMatchupNflId)
colnames(newdf1)[colnames(newdf1)=="displayName"] <- "PrimDefCovPlayer"

newdf1 <- left_join(newdf1, dummyplayersdf, 
                    by = c("pff_secondaryDefensiveCoverageMatchupNflId" = "nflId"))
newdf1 <- newdf1 |> relocate(displayName, .after = pff_secondaryDefensiveCoverageMatchupNflId)
colnames(newdf1)[colnames(newdf1)=="displayName"] <- "SecDefCovPlayer"

newdf1 <- left_join(newdf1, dummyplayersdf, 
                    by = c("blockedPlayerNFLId1" = "nflId"))
newdf1 <- newdf1 |> relocate(displayName, .after = blockedPlayerNFLId1)
colnames(newdf1)[colnames(newdf1)=="displayName"] <- "blockedPlayer1"

newdf1 <- left_join(newdf1, dummyplayersdf, 
                    by = c("blockedPlayerNFLId2" = "nflId"))
newdf1 <- newdf1 |> relocate(displayName, .after = blockedPlayerNFLId2)
colnames(newdf1)[colnames(newdf1)=="displayName"] <- "blockedPlayer2"

newdf1 <- left_join(newdf1, dummyplayersdf, 
                    by = c("blockedPlayerNFLId3" = "nflId"))
newdf1 <- newdf1 |> relocate(displayName, .after = blockedPlayerNFLId3)
colnames(newdf1)[colnames(newdf1)=="displayName"] <- "blockedPlayer3"


# Adding YdstoEZBef  ------------------------------------------------------

playsdf <- playsdf |> 
  mutate(YdstoEZBef = if_else(possessionTeam==yardlineSide, 100 - yardlineNumber,
                              yardlineNumber))
playsdf <- playsdf |> relocate(YdstoEZBef, .after = yardlineNumber)
playsdf <- playsdf |> select(-penaltyYards)

finalplaysdf <- left_join(newdf1, playsdf)

playertracking <- left_join(playertracking, dummyplayersdf)
playertracking <- playertracking |> relocate(position, .after = displayName)

testfinalplays <- left_join(finalplaysdf, gamesdf)

finaldf <- left_join(playertracking, testfinalplays)
saveRDS(finaldf, "totaldata.rds")


# Splitting it up by team (OFFENSE) -------------------------------------------------
mydf <- readRDS("totaldata.rds")
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data/Offense by Team")

tmlist <- unique(mydf$possessionTeam)
for(tm in tmlist){
  filename <- paste0(tm, "file.rds")
  filtdf <- mydf |> filter(possessionTeam==tm)
  saveRDS(filtdf, filename)
  print(tm)
}


# Splitting it up by team (DEFENSE) -------------------------------------------------
mydf <- readRDS("totaldata.rds")
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data/Defense by Team")

tmlist <- unique(mydf$possessionTeam)
for(tm in tmlist){
  filename <- paste0(tm, "file.rds")
  filtdf <- mydf |> filter(defensiveTeam==tm)
  saveRDS(filtdf, filename)
  print(tm)
}



