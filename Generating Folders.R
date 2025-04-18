library(tidyverse)

tmlist <- c("SEA", "DEN", "DAL", "TB", "NYG", "TEN", "MIN", "GB", "LAC", "LV",
            "KC", "ARI", "JAX", "WAS", "NYJ", "BAL", "NE", "MIA", "IND", "HOU",
            "DET", "PHI", "PIT", "CIN", "CHI", "SF", "CAR", "CLE", "ATL", "NO",
            "BUF", "LA")
for(team in tmlist){
  team_elmt <- team
  
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data/Offense by Team")
  firstfilename <- paste0(team, "file.rds")
  mydf <- readRDS(firstfilename)
  
  gamelist <- unique(mydf$gameId)
  
  parent_folder <- team_elmt
  subfolders <- gamelist
  
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Play Graphs")
  if (!dir.exists(parent_folder)) {
    dir.create(parent_folder)
  }
  
  # Create the subfolders within the parent folder
  for (subfolder in subfolders) {
    dir.create(file.path(parent_folder, subfolder))
  }
  print(team)
}
