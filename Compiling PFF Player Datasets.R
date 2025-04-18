library(tidyverse)
library(rlist)

setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/passing_summary/2006")

statlist <- c(
  "passing_summary", "passing_depth", "time_in_pocket",
  "passing_allowed_pressure",
           "rushing_summary",
  "receiving_summary", "receiving_depth", "receiving_concept",
           "receiving_scheme",
           "offense_blocking", "defense_summary",
  "passing_concept", "passing_pressure",
           "pass_rush_summary", "run_defense_summary", "defense_coverage_summary", 
           "defense_coverage_scheme")
yearlist <- c(2015:2024)
for(stat in statlist){
  statfilelist <- list()
  for(year in yearlist){
    mywd <- paste0("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/",
                   stat, "/", year)
    setwd(mywd)
    if(year<=2020){
      weeklist <- c(1:17)
    } else{
      weeklist <- c(1:18)
    }
    yearfilelist <- list()
    for(week in weeklist){
      file <- suppressMessages(read_csv(paste0("Week ", week, ".csv")))
      colstoselect <- c("attempts", "routes", "snap_counts_offense", 
                        "snap_counts_run_defense", "snap_counts_pass_rush",
                        "snap_counts_coverage")
      file <- file %>%
        select(matches("^player$|^player_id$|^position$|^team_name$|grade") |
                 any_of(colstoselect))
      file$Year <- year
      file$Week <- week
      file <- file |> relocate(Year:Week, .before = player)
      yearfilelist <- list.append(yearfilelist, file)
    }
    common_cols <- Reduce(intersect, lapply(yearfilelist, names))
    yearfilelist <- lapply(yearfilelist, function(df) df[, common_cols, drop = FALSE])
    yearfile <- yearfilelist[[1]]
    for(k in 2:length(yearfilelist)){
      yearfile <- rbind(yearfile, yearfilelist[[k]])
    }
    statfilelist <- list.append(statfilelist, yearfile)
    print(paste0(stat,": ", year))
  }
  newcommon_cols <- Reduce(intersect, lapply(statfilelist, names))
  statfilelist <- lapply(statfilelist, function(df) df[, newcommon_cols, drop = FALSE])
  statfile <- statfilelist[[1]]
  for(m in 2:length(statfilelist)){
    statfile <- rbind(statfile, statfilelist[[m]])
  }
  filename <- paste0("All", stat, ".csv")
  setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/compiled datasets")
  write_csv(statfile, filename)
}


### combining them all into two giant datasets (One for offense, one for defense)
# DEFENSE
setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/compiled datasets")
myfiles <- list.files()
myfiles <- myfiles[grepl("defense", myfiles) | grepl("pass_rush_summary", myfiles)]

basefile <- suppressMessages(read_csv(myfiles[[1]]))
for(b in 2:length(myfiles)){
  dummyfile <- suppressMessages(read_csv(myfiles[[b]]))
  basefile <- full_join(basefile, dummyfile)
  print(b)
}

setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats")
write_csv(basefile, "AllDefense2015+.csv")

# PASSING
setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/compiled datasets")
myfiles <- list.files()
myfiles <- myfiles[grepl("passing", myfiles) | grepl("time_in_pocket", myfiles)]

basefile <- suppressMessages(read_csv(myfiles[[1]]))
basefile <- basefile |> distinct(Year, Week, player, player_id, position, team_name,
                                 .keep_all = TRUE)
for(b in 2:length(myfiles)){
  dummyfile <- suppressMessages(read_csv(myfiles[[b]]))
  dummyfile <- dummyfile |> distinct(Year, Week, player, player_id, position, team_name,
                                     .keep_all = TRUE)
  basefile <- full_join(basefile, dummyfile)
  print(b)
}
setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats")
write_csv(basefile, "AllPassing2015+.csv")

# RUSHING/RECEIVING/BLOCKING
setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/compiled datasets")
myfiles <- list.files()
myfiles <- myfiles[!grepl("passing", myfiles) & !grepl("time_in_pocket", myfiles) &
                     !grepl("defense", myfiles) & !grepl("pass_rush_summary", myfiles)]

basefile <- suppressMessages(read_csv(myfiles[[1]]))
for(b in 2:length(myfiles)){
  dummyfile <- suppressMessages(read_csv(myfiles[[b]]))
  basefile <- full_join(basefile, dummyfile)
  print(b)
}
setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats")
write_csv(basefile, "AllRushRecBlock2015+.csv")
