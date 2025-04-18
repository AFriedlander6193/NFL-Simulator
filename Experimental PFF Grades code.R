library(tidyverse)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
passingdf <- read_csv("passingnflID+pffID.csv")
passingdf <- passingdf |> filter(!is.na(avg_grades_pass))
rushrecblockdf <- read_csv("rushrecblocknflID+pffID.csv")
defensedf <- read_csv("defensenflID+pffID.csv")

passingdf <- passingdf %>%
  select(-matches("penalty", ignore.case = TRUE))
passingdf <- passingdf %>%
  select(-matches("fumble", ignore.case = TRUE))


# FOR THE PASSING GRADES --------------------------------------------------
offdf <- offdf |> 
  mutate(pressurekey = if_else(runorpass=="Run", NA,
                            case_when(
                              pressureplayers>=1 ~ "pressure", 
                              pressureplayers<1 | is.na(pressureplayers) ~ "no_pressure")),
         timetothrowkey = if_else(runorpass=="Run", NA,
                                  case_when(
                                    timeToThrow<2.5 ~ "less",
                                    timeToThrow>=2.5 | is.na(timeToThrow) ~ "more")),
         screenkey = if_else(runorpass=="Run", NA,
                             case_when(
                               targettedroute=="SCREEN" ~ "screen",
                               targettedroute!="SCREEN" | is.na(targettedroute) ~ "no_screen")),
         playactionkey = if_else(runorpass=="Run", NA,
                                 if_else(playAction==TRUE, "pa", "npa")),
         distancekey = if_else(runorpass=="run", NA,
                               case_when(
                                 passLength >= 20 ~ "deep",
                                 passLength >= 10 & passLength < 20 ~ "medium",
                                 passLength >= 0 & passLength < 10 ~ "short",
                                 passLength < 0 ~ "behind_los",
                                 is.na(passLength) ~ NA
                               )))


offdf$pass_grade_mean <- ""
# offdf$pass_grade_sd <- ""
for(p in 1:nrow(offdf)){
  if(offdf$runorpass[p]=="Run"){
    offdf$pass_grade_mean[p] <- NA
    # offdf$pass_grade_sd[p] <- NA
  } else{
    filtered_pass_df <- passingdf |> filter(nflId==offdf$QB1[p])
    pressure_part <- paste0("avg_", offdf$pressurekey[p], "_grades_pass")
    timetothrow_part <- paste0("avg_", offdf$timetothrowkey[p], "_grades_pass")
    # screen_part <- paste0("avg_", offdf$screenkey[p], "_grades_pass")
    playaction_part <- paste0("avg_", offdf$playactionkey[p], "_grades_pass")
    distance_part <- paste0("avg_", offdf$distancekey[p], "_grades_pass")
    # pressure_part2 <- paste0("sd_", offdf$pressurekey[p], "_grades_pass")
    # timetothrow_part2 <- paste0("sd_", offdf$timetothrowkey[p], "_grades_pass")
    # screen_part2 <- paste0("sd_", offdf$screenkey[p], "_grades_pass")
    # playaction_part2 <- paste0("sd_", offdf$playactionkey[p], "_grades_pass")
    selectcols <- c(pressure_part, timetothrow_part, playaction_part, distance_part
                    # pressure_part2, timetothrow_part2, screen_part2, playaction_part2
                    )
    final_pass_df <- filtered_pass_df[colnames(filtered_pass_df) %in% selectcols]
    means <- as.numeric(final_pass_df)
    # sds <- as.numeric(final_pass_df[5:8])
    offdf$pass_grade_mean[p] <- mean(means)
    # offdf$pass_grade_sd[p] <- mean(sds)
  }
  print(p)
}
offdf$pass_grade_mean <- as.numeric(offdf$pass_grade_mean)

# FOR THE RUSHING GRADES --------------------------------------------------
offdf$rush_grade_mean <- ""
# offdf$rush_grade_sd <- ""
for(r in 1:nrow(offdf)){
  if(offdf$runorpass[r]=="Pass"){
    offdf$rush_grade_mean[r] <- NA
    # offdf$rush_grade_sd[r] <- NA
  } else{
    rushplayerdf <- rushrecblockdf |> filter(nflId==offdf$rushingplayer[r])
    offdf$rush_grade_mean[r] <- as.numeric(rushplayerdf$avg_grades_run)
    # offdf$rush_grade_sd[r] <- as.numeric(rushplayerdf$sd_grades_run)
  }
  print(r)
}
offdf$rush_grade_mean <- as.numeric(offdf$rush_grade_mean)



# FOR TARGETTED RECEIVER RECEIVING GRADES ---------------------------------
offdf <- offdf |> 
  mutate(newmanzone = case_when(
    manzone == "Other" & 
      (coverage == "Red Zone" | coverage == "Bracket" | coverage == "Goal Line") ~ "Man",
    manzone == "Other" & 
      (coverage == "Prevent" | coverage == "Miscellaneous") ~ "Zone",
    TRUE ~ manzone
    
  ))
offdf <- offdf |> relocate(newmanzone, .after = manzone)


offdf$receiving_grade_mean <- ""
for(c in 1: nrow(offdf)){
  if(offdf$runorpass[c]=="Run"){
    offdf$receiving_grade_mean[c] <- NA
  } else{
    recplayerdf <- rushrecblockdf |> filter(nflId==offdf$targettedplayer[c])
    if(nrow(recplayerdf)==0){
      offdf$receiving_grade_mean[c] <- NA
    } else{
      distance_route_part <- paste0("avg_", offdf$distancekey[c], "_grades_pass_route")
      distance_drop_part <- paste0("avg_", offdf$distancekey[c], "_grades_hands_drop")
      manzone_route_part <- paste0("avg_", tolower(offdf$newmanzone[c]), "_grades_pass_route")
      manzone_drop_part <- paste0("avg_", tolower(offdf$newmanzone[c]), "_grades_hands_drop")
      selectcols <- c(distance_route_part, distance_drop_part, manzone_route_part,
                      manzone_drop_part)
      final_pass_df <- recplayerdf[colnames(recplayerdf) %in% selectcols]
      means <- as.numeric(final_pass_df)
      offdf$receiving_grade_mean[c] <- mean(means)
    }
  }
  print(c)
}
offdf$receiving_grade_mean <- as.numeric(offdf$receiving_grade_mean)


# FOR OLINE BLOCKING GRADES -----------------------------------------------

offdf$oline_brocking_grade_mean <- ""
for(b in 1:nrow(offdf)){
  blockers <- na.omit(as.numeric(offdf |> select(T1:C1) |> slice(b)))
  blockdf <- rushrecblockdf |> filter(nflId %in% blockers)
  if(offdf$runorpass[b]=="Run"){
    means <- as.numeric(blockdf$avg_grades_run_block)
  } else{
    means <- as.numeric(blockdf$avg_grades_pass_block)
  }
  offdf$oline_brocking_grade_mean[b] <- sum(means)
  print(b)
}
offdf$oline_brocking_grade_mean <- as.numeric(offdf$oline_brocking_grade_mean)
offdf <- offdf |>
  mutate(oline_brocking_grade_mean = (oline_brocking_grade_mean/460) * 100)


# GRADES FOR REST OF OFFENSIVE PLAYERS ------------------------------------

offdf$allother_offensive_grade_mean <- ""
for(o in 1:nrow(offdf)){
  allotheroff <- na.omit(as.numeric(offdf |> select(RB1:TE3) |> slice(o)))
  if(offdf$runorpass[o]=="Run"){
    finalotheroff <- allotheroff[-which(allotheroff==offdf$rushingplayer[o])]
  } else{
    finalotheroff <- allotheroff[-which(allotheroff==offdf$targettedplayer[o])]
  }
  if(is_empty(finalotheroff)){
    finalotheroff <- allotheroff
  }
  newdf <- rushrecblockdf |> filter(nflId %in% finalotheroff)
  if(offdf$runorpass[o]=="Run"){
    means <- as.numeric(newdf$avg_grades_run_block)
  } else{
    means <- as.numeric(newdf$avg_grades_offense)
  }
  offdf$allother_offensive_grade_mean[o] <- sum(means)
  print(o)
}
offdf$allother_offensive_grade_mean <- as.numeric(offdf$allother_offensive_grade_mean)
offdf <- offdf |> 
  mutate(allother_offensive_grade_mean = (allother_offensive_grade_mean/375) * 100)



# GRADES FOR PLAYERS COVERING TARGETTED PLAYER -----------------------------

offdf$cover_target_route_grade_mean <- ""
for(tc in 1:nrow(offdf)){
  if(offdf$runorpass[tc]=="Run"){
    offdf$cover_target_route_grade_mean[tc] <- NA
  } else{
    startkey <- which(colnames(offdf)=="QB1")
    endkey <- which(colnames(offdf)=="TE3")
    slice <- offdf[startkey:endkey][tc,]
    indkey <- colnames(slice)[which(slice==offdf$targettedplayer[tc])]
    if(is_empty(indkey)){
      offdf$cover_target_route_grade_mean[tc] <- NA
    } else{
      newdf <- offdf |> select(matches(paste0(indkey, "matchup")))
      newdf1 <- newdf[tc,]
      coverplayers <- as.numeric(newdf1)
      coverplayers <- na.omit(coverplayers)
      if(is_empty(coverplayers)){
        offdf$cover_target_route_grade_mean[tc] <- NA
      } else{
        defstartkey <- which(colnames(offdf)=="coverage_zone_1")
        defendkey <- which(colnames(offdf)=="coverage_zone_8")
        defslice <- offdf[defstartkey:defendkey][tc,]
        coverplayerassignment <- function(player){
          defdf <- defensedf |> filter(nflId==player)
          mykey <- which(grepl(player, defslice[1,]))
          if(is_empty(mykey)){
            manzonecheck <- "ZONE"
          } else{
            varvec <- c(str_split_fixed(defslice[mykey], " ", 3))
            manzonecheck <- varvec[3]
          }
          if(manzonecheck=="MAN"){
            value <- (2*defdf$avg_man_grades_coverage_defense + defdf$avg_grades_tackle) / 3
          } else{
            value <- (2*defdf$avg_zone_grades_coverage_defense + defdf$avg_grades_tackle) / 3
          }
          value
        }
        allcovervalues <- c(coverplayerassignment(coverplayers[1]))
        if(length(coverplayers)>=2){
          allcovervalues <- append(allcovervalues, coverplayerassignment(coverplayers[2]))
        }
        if(length(coverplayers)==3){
          allcovervalues <- append(allcovervalues, coverplayerassignment(coverplayers[3]))
        }
        finalvalue <- (sum(allcovervalues) / (length(allcovervalues))^(1/3)) 
        offdf$cover_target_route_grade_mean[tc] <- finalvalue
      }
    }
  }
  print(tc)
}
offdf$cover_target_route_grade_mean <- as.numeric(offdf$cover_target_route_grade_mean)



# CODE FOR ALL OTHER COVER PLAYERS ----------------------------------------

offdf$allother_cover_players_grade_mean <- ""
for(oc in 1:nrow(offdf)){
  if(offdf$runorpass[oc]=="Run"){
    defstartkey <- which(colnames(offdf)=="NT1")
    defendkey <- which(colnames(offdf)=="SS2")
    defplayers <- as.numeric(offdf[defstartkey:defendkey][oc,])
    defplayers <- na.omit(defplayers)
    defdf <- defensedf |> filter(nflId %in% defplayers)
    rundefpart <- as.numeric(defdf$avg_grades_run_defense)
    tacklepart <- as.numeric(defdf$avg_grades_tackle)
    finalpart <- (rundefpart + tacklepart) / 2
    offdf$allother_cover_players_grade_mean[oc] <- mean(finalpart)
  } else{
    startkey <- which(colnames(offdf)=="QB1")
    endkey <- which(colnames(offdf)=="TE3")
    slice <- offdf[startkey:endkey][oc,]
    indkey <- colnames(slice)[which(slice==offdf$targettedplayer[oc])]
    if(is_empty(indkey)){
      removeplayers <- c()
    } else{
      newdf <- offdf |> select(matches(paste0(indkey, "matchup")))
      newdf1 <- newdf[oc,]
      coverplayers <- as.numeric(newdf1)
      removeplayers <- na.omit(coverplayers)
    }
    defstartkey <- which(colnames(offdf)=="coverage_zone_1")
    defendkey <- which(colnames(offdf)=="coverage_zone_8")
    defslice <- as.character(offdf[defstartkey:defendkey][oc,])
    result <- as.data.frame(sapply(defslice, function(x) str_split_fixed(x, " ", 3)))
    coverplayers <- na.omit(as.numeric(result[1,]))
    if(coverplayers[1] %in% removeplayers){
      coverplayers <- coverplayers[-which(coverplayers %in% removeplayers)]
    }
    defdf <- defensedf |> filter(nflId %in% coverplayers)
    grades <- as.numeric(defdf$avg_grades_defense)
    offdf$allother_cover_players_grade_mean[oc] <- mean(grades)
  }
  print(oc)
}
offdf$allother_cover_players_grade_mean <- as.numeric(offdf$allother_cover_players_grade_mean)


# CODE FOR PASS RUSH PLAYERS ----------------------------------------------

offdf$passrush_players_grade_mean <- ""
for(pr in 1:nrow(offdf)){
  if(offdf$runorpass[pr]=="Run"){
    offdf$passrush_players_grade_mean[pr] <- NA
  } else{
    defstartkey <- which(colnames(offdf)=="coverage_zone_1")
    defendkey <- which(colnames(offdf)=="coverage_zone_8")
    defslice <- as.character(offdf[defstartkey:defendkey][pr,])
    result <- as.data.frame(sapply(defslice, function(x) str_split_fixed(x, " ", 3)))
    coverplayers <- na.omit(as.numeric(result[1,]))
    posstartkey <- which(colnames(offdf)=="NT1")
    posendkey <- which(colnames(offdf)=="SS2")
    defplayers <- as.numeric(offdf[posstartkey:posendkey][pr,])
    defplayers <- na.omit(defplayers)
    coverkeys <- which(defplayers %in% coverplayers)
    passrushers <- defplayers[-coverkeys]
    defdf <- defensedf |> filter(nflId %in% passrushers)
    grades <- as.numeric(defdf$avg_grades_pass_rush_defense)
    offdf$passrush_players_grade_mean[pr] <- sum(grades)
  }
  print(pr)
}
offdf$passrush_players_grade_mean <- as.numeric(offdf$passrush_players_grade_mean)
offdf <- offdf |> 
  mutate(passrush_players_grade_mean = passrush_players_grade_mean/4)
offdf <- offdf |> 
  mutate(passrush_players_grade_mean =
           if_else(passrush_players_grade_mean>=100, 99.9, passrush_players_grade_mean))

offdf <- offdf |> filter(!is.na(allother_cover_players_grade_mean))

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
write_csv(offdf, "AllOffensivePersonnel.csv")
