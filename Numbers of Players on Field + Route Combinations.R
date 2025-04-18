library(tidyverse)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")


position_counter <- function(positions) {
  for (pos in positions) {
    pos_number_columns <- grep(paste0("^", pos, "\\d+$"), names(offdf), value = TRUE)
    if (length(pos_number_columns) == 0) {
      message(paste("No columns matching the pattern", paste0("^", pos, "\\d+$"), "found."))
      next 
    }
    non_na_counts <- rowSums(!is.na(offdf[pos_number_columns]))
    offdf <<- offdf |> mutate(!!paste0(pos, "count") := non_na_counts)
  }
  return(offdf)
}


position_counter(c("QB", "RB", "WR", "TE", "T", "G", "C", "NT", "DT", "DE",
                   "LB", "OLB", "ILB", "MLB", "CB", "DB", "FS", "SS"))

offdf <- offdf |> 
  mutate(Off_Personnel = paste0(RBcount, "-", WRcount, "-", TEcount),
         dline_count = NTcount + DTcount + DEcount,
         linebacker_count = LBcount + OLBcount + ILBcount + MLBcount,
         secondary_count = CBcount + DBcount + FScount + SScount,
         Def_Personnel = paste0(dline_count, "-", linebacker_count, "-", secondary_count),
         Def_Pers_Name = case_when(
           secondary_count < 5 ~ paste0(dline_count, "-", linebacker_count),
           secondary_count == 5 ~ "Nickel",
           secondary_count == 6 ~ "Dime",
           secondary_count == 7 ~ "Quarter",
           secondary_count == 8 ~ "Dollar",
           secondary_count > 8 ~ "Prevent"
         ))
offdf <- offdf |> relocate(Def_Pers_Name, .after = Def_Personnel)
write_csv(offdf, "AllOffensivePersonnel.csv")


# Finding route combinations ----------------------------------------------
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

offdf <- offdf |> 
  mutate(route1 = sub("^\\d+\\s+", "", route_1),
         route2 = sub("^\\d+\\s+", "", route_2),
         route3 = sub("^\\d+\\s+", "", route_3),
         route4 = sub("^\\d+\\s+", "", route_4),
         route5 = sub("^\\d+\\s+", "", route_5),
         )

offdf$routecomb <- ""
for(n in 1:nrow(offdf)){
  vec <- c(offdf$route1[n], offdf$route2[n], offdf$route3[n], offdf$route4[n], offdf$route5[n])
  vec <- na.omit(vec)
  newvec <- sort(vec)
  string <- newvec[1]
  for(m in 2:length(newvec)){
    string <- paste0(string, ", ", newvec[m])
  }
  offdf$routecomb[n] <- string
  print(n)
}

offdf <- offdf |> relocate(routecomb, .after = route_5)

routelist <- sort(unique(offdf$route1))

for(route in routelist){
  offdf <- offdf %>%
    mutate(dummycol = rowSums(across(c(route1, route2, route3, route4, route5), ~ . == route), na.rm = TRUE))
  colnames(offdf)[colnames(offdf)=="dummycol"] <- paste0(route,"_count")
  print(route)
}


# Creating/Manipulating Other Variables -----------------------------------

offdf[c("Minutes","Seconds", "Extra")] <- str_split_fixed(offdf$gameClock, ":", 3)
offdf$Minutes <- as.numeric(offdf$Minutes)
offdf$Seconds <- as.numeric(offdf$Seconds)
offdf <- offdf |> select(-Extra)

offdf <- offdf |> 
  mutate(SecondsLeft = case_when(
    quarter == 1 ~ 2700 + (60 * Minutes) + Seconds,
    quarter == 2 ~ 1800 + (60 * Minutes) + Seconds,
    quarter == 3 ~ 900 + (60 * Minutes) + Seconds,
    quarter >= 4 ~ (60 * Minutes) + Seconds,
  ),
  QuarterSeconds = (60 * Minutes) + Seconds)

offdf <- offdf |> relocate(SecondsLeft, .after = gameClock)
offdf <- offdf |> relocate(QuarterSeconds, .after = SecondsLeft)

offdf <- offdf |> filter(!is.na(off_form))

offdf <- offdf |> 
  mutate(motionfactor = if_else(is.na(motionplayer), 0, 1))

offdf <- offdf |> 
  mutate(runpassfactor = if_else(runorpass == "Pass", 1, 0))

offdf$runpassfactor <- as.factor(offdf$runpassfactor)

offdf <- offdf |> mutate(
  motion = if_else(is.na(motionplayer), 0, 1)
)

offdf <- offdf |> relocate(motion, .after = motionplayer)

offdf <- offdf |> 
  mutate(togo_category = case_when(
    yardsToGo <= 3 ~ "short",
    yardsToGo >= 4 & yardsToGo <= 7 ~ "medium",
    yardsToGo >= 8 & yardsToGo <= 11 ~ "long",
    yardsToGo >= 12 & yardsToGo <= 15 ~ "very long",
    yardsToGo > 15 ~ "longest"
  ))

### figuring out which route was targetted
offdf$targettedplayer <- as.character(offdf$targettedplayer)
offdf$targettedroute <- ""
for(n in 1:nrow(offdf)){
  if(isTRUE(grepl(offdf$targettedplayer[n], offdf$route_1[n]))){
    offdf$targettedroute[n] <- sub(".*? ", "", offdf$route_1[n])
  } else if(isTRUE(grepl(offdf$targettedplayer[n], offdf$route_2[n]))){
    offdf$targettedroute[n] <- sub(".*? ", "", offdf$route_2[n])
  } else if(isTRUE(grepl(offdf$targettedplayer[n], offdf$route_3[n]))){
    offdf$targettedroute[n] <- sub(".*? ", "", offdf$route_3[n])
  } else if(isTRUE(grepl(offdf$targettedplayer[n], offdf$route_4[n]))){
    offdf$targettedroute[n] <- sub(".*? ", "", offdf$route_4[n])
  } else if(isTRUE(grepl(offdf$targettedplayer[n], offdf$route_5[n]))){
    offdf$targettedroute[n] <- sub(".*? ", "", offdf$route_5[n])
  } else{
    offdf$targettedroute[n] <- NA
  }
  print(n)
}
offdf <- offdf |> relocate(targettedroute, .after = targettedplayer)

write_csv(offdf, "AllOffensivePersonnel.csv")
