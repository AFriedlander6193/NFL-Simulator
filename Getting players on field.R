
# Setup -------------------------------------------------------------------

library(tidyverse)
library(purrr)
library(rlist)



tmlist <- c( "SEA", "DEN", "DAL", "TB", "NYG", "TEN", "MIN", "GB", "LAC", "LV", "KC",
            "ARI", "JAX", "WAS", "NYJ", "BAL", "NE", "MIA", "IND", "HOU",
            "DET", "PHI", "PIT", "CIN", "CHI", "SF", "CAR", "CLE", "ATL", "NO",
            "BUF", "LA")

### OFFENSIVE PERSONNEL

for(team in tmlist){
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data/Offense by Team")
  firstfilename <- paste0(team, "file.rds")
  mydf <- readRDS(firstfilename)
  
  mydf <- mydf |>
    mutate(PossTmDiff = if_else(possessionTeam==homeTeamAbbr, 
                                preSnapHomeScore - preSnapVisitorScore,
                                preSnapVisitorScore - preSnapHomeScore),
           PossWinProb = if_else(possessionTeam==homeTeamAbbr, preSnapHomeTeamWinProbability,
                                 preSnapVisitorTeamWinProbability),
           defensiveTeam = if_else(possessionTeam==homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr))
  mydf$nflId <- as.character(mydf$nflId)
  mydf$yardageGainedAfterTheCatch <- as.character(mydf$yardageGainedAfterTheCatch)
  
  # 
  newdf <- mydf |> 
    group_by(gameId, playId) |>  # Group by gameId and playId
    summarise(
      # Extract unique players for each position
      QB = list(unique(nflId[position == "QB"])),
      RB = list(unique(nflId[position == "RB"])),
      WR = list(unique(nflId[position == "WR"])),
      TE = list(unique(nflId[position == "TE"])),
      FB = list(unique(nflId[position == "FB"])),
      `T` = list(unique(nflId[position == "T"])),
      G = list(unique(nflId[position == "G"])),
      C = list(unique(nflId[position == "C"])),
      NT = list(unique(nflId[position == "NT"])),
      DT = list(unique(nflId[position == "DT"])),
      DE = list(unique(nflId[position == "DE"])),
      LB = list(unique(nflId[position == "LB"])),
      OLB = list(unique(nflId[position == "OLB"])),
      ILB = list(unique(nflId[position == "ILB"])),
      MLB = list(unique(nflId[position == "MLB"])),
      CB = list(unique(nflId[position == "CB"])),
      DB = list(unique(nflId[position == "DB"])),
      FS = list(unique(nflId[position == "FS"])),
      SS = list(unique(nflId[position == "SS"])),
      off_form = offenseFormation[1],
      receiverAlignment = receiverAlignment[1],
      runorpass = if_else(is.na(passResult[1]), "Run", "Pass"),
      rushLocationType = rushLocationType[1],
      timeToThrow = timeToThrow[1],
      dropbackDistance = dropbackDistance[1],
      dropbackType = dropbackType[1],
      playAction = playAction[1],
      passLength = passLength[1],
      coverage = pff_passCoverage[1],
      manzone = pff_manZone[1],
      quarter = quarter[1],
      gameClock = gameClock[1],
      down = down[1],
      possessionTeam = team,
      defensiveTeam = defensiveTeam[1],
      yardsToGo = yardsToGo[1],
      YdstoEZBef = YdstoEZBef[1],
      Detail = playDescription[1],
      yardsGained = yardsGained[1],
      PossTmDiff = PossTmDiff[1],
      PossWinProb = PossWinProb[1],
      playevents = list(event[!is.na(event)]),
      eventtimes = list(time[!is.na(event)]),
      motionplayer = list(nflId[inMotionAtBallSnap == TRUE]),
      pressureplayers = list(displayName[causedPressure== TRUE]),
      routesrun = list(paste0(nflId[!is.na(routeRan)], " ", routeRan[!is.na(routeRan)])),
      targettedplayer = case_when(
        runorpass[1] == "Pass" ~ unique(nflId[wasTargettedReceiver == 1])[1], 
        TRUE ~ NA_character_
      ),
      receptionplayer = case_when(
        runorpass[1] == "Pass" & length(which(hadPassReception==1))>0 ~ 
          unique(nflId[hadPassReception == 1])[1], 
        TRUE ~ NA_character_
      ),
      yardsaftercatch = case_when(
        runorpass[1] == "Pass" & length(which(hadPassReception==1))>0 ~ 
          unique(yardageGainedAfterTheCatch)[1], 
        TRUE ~ NA_character_
      ),
      inCoverage = list(displayName[!is.na(pff_defensiveCoverageAssignment)]),
      .groups = "drop"
    ) |> 
    mutate(
      # Dynamically create position columns (e.g., QB1, QB2, ...)
      QB1 = sapply(QB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      QB2 = sapply(QB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      RB1 = sapply(RB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      RB2 = sapply(RB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      RB3 = sapply(RB, function(x) ifelse(length(x) >= 2, x[3], NA)),
      FB1 = sapply(FB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      FB2 = sapply(FB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      WR1 = sapply(WR, function(x) ifelse(length(x) >= 1, x[1], NA)),
      WR2 = sapply(WR, function(x) ifelse(length(x) >= 2, x[2], NA)),
      WR3 = sapply(WR, function(x) ifelse(length(x) >= 3, x[3], NA)),
      WR4 = sapply(WR, function(x) ifelse(length(x) >= 4, x[4], NA)),
      WR5 = sapply(WR, function(x) ifelse(length(x) >= 5, x[5], NA)),
      TE1 = sapply(TE, function(x) ifelse(length(x) >= 1, x[1], NA)),
      TE2 = sapply(TE, function(x) ifelse(length(x) >= 2, x[2], NA)),
      TE3 = sapply(TE, function(x) ifelse(length(x) >= 3, x[3], NA)),
      T1 = sapply(`T`, function(x) ifelse(length(x) >= 1, x[1], NA)),
      T2 = sapply(`T`, function(x) ifelse(length(x) >= 2, x[2], NA)),
      T3 = sapply(`T`, function(x) ifelse(length(x) >= 3, x[3], NA)),
      G1 = sapply(G, function(x) ifelse(length(x) >= 1, x[1], NA)),
      G2 = sapply(G, function(x) ifelse(length(x) >= 2, x[2], NA)),
      G3 = sapply(G, function(x) ifelse(length(x) >= 3, x[3], NA)),
      C1 = sapply(C, function(x) ifelse(length(x) >= 1, x[1], NA)),
      NT1 = sapply(NT, function(x) ifelse(length(x) >= 1, x[1], NA)),
      NT2 = sapply(NT, function(x) ifelse(length(x) >= 2, x[2], NA)),
      NT3 = sapply(NT, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DT1 = sapply(DT, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DT2 = sapply(DT, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DT3 = sapply(DT, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DE1 = sapply(DE, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DE2 = sapply(DE, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DE3 = sapply(DE, function(x) ifelse(length(x) >= 3, x[3], NA)),
      LB1 = sapply(LB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      LB2 = sapply(LB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      LB3 = sapply(LB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      OLB1 = sapply(OLB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      OLB2 = sapply(OLB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      OLB3 = sapply(OLB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      ILB1 = sapply(ILB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      ILB2 = sapply(ILB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      ILB3 = sapply(ILB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      MLB1 = sapply(MLB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      MLB2 = sapply(MLB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      MLB3 = sapply(MLB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      CB1 = sapply(CB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      CB2 = sapply(CB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      CB3 = sapply(CB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      CB4 = sapply(CB, function(x) ifelse(length(x) >= 4, x[4], NA)),
      CB5 = sapply(CB, function(x) ifelse(length(x) >= 5, x[5], NA)),
      DB1 = sapply(DB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DB2 = sapply(DB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DB3 = sapply(DB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DB4 = sapply(DB, function(x) ifelse(length(x) >= 4, x[4], NA)),
      DB5 = sapply(DB, function(x) ifelse(length(x) >= 5, x[5], NA)),
      DB6 = sapply(DB, function(x) ifelse(length(x) >= 6, x[6], NA)),
      FS1 = sapply(FS, function(x) ifelse(length(x) >= 1, x[1], NA)),
      FS2 = sapply(FS, function(x) ifelse(length(x) >= 2, x[2], NA)),
      SS1 = sapply(SS, function(x) ifelse(length(x) >= 1, x[1], NA)),
      SS2 = sapply(SS, function(x) ifelse(length(x) >= 2, x[2], NA)),
    ) |> 
    select(-(QB:SS))
  
  uniqdf <- mydf |> distinct(gameId, playId, nflId, .keep_all = TRUE)
  newdf <- newdf |> select(-FB2)
  
  ### Getting coverage assingments/zones
  newdf$coveragezones <- list(list())
  for(dd in 1:nrow(newdf)){
    tempdf <- uniqdf |> filter(gameId==newdf$gameId[dd] & playId==newdf$playId[dd])
    tempdf <- tempdf |> filter(!is.na(pff_defensiveCoverageAssignment))
    coveragelist <- c()
    for(ff in 1:nrow(tempdf)){
      coverage <- paste0(tempdf$nflId[ff], " ", tempdf$position[ff],
                         " ", tempdf$pff_defensiveCoverageAssignment[ff])
      coveragelist <- append(coveragelist, coverage)
    }
    newdf$coveragezones[[dd]] <- coveragelist
  }
  for(ii in 1:8) {
    newdf[[paste0("coverage_zone_", ii)]] <- sapply(newdf$coveragezones, function(x) if(length(x) >= ii) x[ii] else NA)
  }
  newdf <- newdf |> select(-coveragezones)
  
  ### Getting player assignments
  positionlist <- c("RB1", "RB2", "RB3", "FB1", "WR1", "WR2", "WR3", "WR4", "WR5",
                    "TE1", "TE2", "TE3", "T1", "T2", "T3", "G1", "G2", "G3", "C1")
  for(position in positionlist){
    newdf$dummycol1 <- ""
    newdf$dummycol2 <- ""
    newdf$dummycol3 <- ""
    for(qq in 1:nrow(newdf)){
      colkey <- which(colnames(newdf)==position)
      if(!is.na(as.numeric(newdf[colkey][qq,]))){
        block_assign_check <- uniqdf |> filter(gameId==newdf$gameId[qq] &
                                                 playId==newdf$playId[qq] &
                                                 nflId==as.numeric(newdf[colkey][qq,]))
        blockvals <- block_assign_check$blockedPlayerNFLId1
        coverage_assign_check <- uniqdf |> filter(gameId==newdf$gameId[qq] &
                                                    playId==newdf$playId[qq] &
                                                    pff_primaryDefensiveCoverageMatchupNflId==as.numeric(newdf[colkey][qq,]))
        covervals <- coverage_assign_check$nflId
        allmatchvals <- na.omit(c(blockvals, covervals))
        if(!is_empty(allmatchvals)){
          newdf$dummycol1[qq] <- allmatchvals[1]
          allmatchvals <- allmatchvals[-1]
        } else{
          newdf$dummycol1[qq] <- NA
        }
        if(!is_empty(allmatchvals)){
          newdf$dummycol2[qq] <- allmatchvals[1]
          allmatchvals <- allmatchvals[-1]
        } else{
          newdf$dummycol2[qq] <- NA
        }
        if(!is_empty(allmatchvals)){
          newdf$dummycol3[qq] <- allmatchvals[1]
          allmatchvals <- allmatchvals[-1]
        } else{
          newdf$dummycol3[qq] <- NA
        }
      } else{
        newdf$dummycol1[qq] <- NA
        newdf$dummycol2[qq] <- NA
        newdf$dummycol3[qq] <- NA
      }
    }
    colnames(newdf)[colnames(newdf)=="dummycol1"] <- paste0(position, "matchup1")
    colnames(newdf)[colnames(newdf)=="dummycol2"] <- paste0(position, "matchup2")
    colnames(newdf)[colnames(newdf)=="dummycol3"] <- paste0(position, "matchup3")
  }
  
  newdf$rushingplayer <- ""
  newdf$sackdefender <- ""
  newdf$sackyards <- ""
  newdf$interceptiondefender <- ""
  newdf$interceptionyards <- ""
  newdf$fumbleplayer <- ""
  newdf$fumblelost <- ""
  for(z in 1:nrow(newdf)){
    fumbdf <- uniqdf |> filter(gameId==newdf$gameId[z] & playId==newdf$playId[z])
    fumbdf <- fumbdf |> filter(position %in% c("QB", "RB", "FB", "WR", "TE", "T",
                                               "G", "C"))
    fumbcheck <- length(unique(fumbdf$fumbles))
    if(isTRUE(fumbcheck==1)){
      newdf$fumbleplayer[z] <- NA
      newdf$fumblelost[z] <- NA
    } else{
      newdf$fumbleplayer[z] <- fumbdf$nflId[fumbdf$fumbles==1]
      newdf$fumblelost[z] <- fumbdf$fumbleLost[fumbdf$fumbles==1]
    }
    if(newdf$runorpass[z]=="Pass"){
      newdf$rushingplayer[z] <- NA
      intdf <- uniqdf |> filter(gameId==newdf$gameId[z] & playId==newdf$playId[z])
      intcheck <- length(unique(intdf$hadInterception))
      sackcheck <- length(unique(intdf$sackYardsAsDefense))
      if(isTRUE(intcheck==1)){
        newdf$interceptiondefender[z] <- NA
        newdf$interceptionyards[z] <- NA
      } else{
        newdf$interceptiondefender[z] <- intdf$nflId[intdf$hadInterception==1]
        newdf$interceptionyards[z] <- intdf$interceptionYards[intdf$hadInterception==1]
      }
      if(isTRUE(sackcheck==1)){
        newdf$sackdefender[z] <- NA
        newdf$sackyards[z] <- NA
      } else{
        newdf$sackdefender[z] <- intdf$nflId[intdf$sackYardsAsDefense!=0]
        newdf$sackyards[z] <- intdf$sackYardsAsDefense[intdf$sackYardsAsDefense!=0]
      }
      
    } else{
      uniqplaydf <- uniqdf |> filter(gameId==newdf$gameId[z] & playId==newdf$playId[z])
      player <- uniqplaydf$nflId[uniqplaydf$hadRushAttempt==1]
      newdf$rushingplayer[z] <- player
      newdf$interceptiondefender[z] <- NA
      newdf$interceptionyards[z] <- NA
      newdf$sackdefender[z] <- NA
      newdf$sackyards[z] <- NA
    }
  }
  
  newdf$yardsaftercatch <- as.numeric(newdf$yardsaftercatch)
  newdf <- newdf |> 
    mutate(yardsbeforecatch = yardsGained - yardsaftercatch)
  newdf <- newdf |> relocate(yardsbeforecatch, .before = yardsaftercatch)
  newdf <- newdf |> relocate(yardsGained, .before = yardsbeforecatch)
  
  newdf$routescount <- " "
  newdf <- newdf |> relocate(routescount, .before = routesrun)
  for(f in 1: nrow(newdf)){
    mylist <- newdf$routesrun[[f]]
    newlist <- unique(mylist)
    routenum <- length(newlist)
    if(routenum==1){
      routenum <- 0
    }
    newdf$routescount[f] <- routenum
    newdf$routesrun[[f]] <- newlist
  }
  max_length <- max(sapply(newdf$routesrun, length))
  padded_values <- lapply(newdf$routesrun, function(x) {
    length(x) <- max_length
    return(x)
  })
  values_df <- do.call(rbind, lapply(padded_values, function(x) as.data.frame(t(x))))
  colnames(values_df) <- paste0("route_", seq_len(ncol(values_df)))
  newdf <- cbind(newdf, values_df)
  newdf <- newdf |> relocate((route_1:route_5), .after = routesrun)
  newdf <- newdf |> select(-routesrun)
  
  for(g in 1:nrow(newdf)){
    mylist <- newdf$motionplayer[[g]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    if(!is_empty(player)){
      newdf$motionplayer[g] <- player
    }else{
      newdf$motionplayer[g] <- NA
    }
  }
  
  for(h in 1:nrow(newdf)){
    mylist <- newdf$inCoverage[[h]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    newdf$inCoverage[h] <- length(player)
  }
  
  for(k in 1:nrow(newdf)){
    mylist <- newdf$pressureplayers[[k]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    newdf$pressureplayers[k] <- length(player)
  }
  
  newdf$event_count <- ""
  for(j in 1:15){
    newdf$dummycol <- ""
    newdf$dummytimecol <- ""
    finaleventname <- paste0("event_", j)
    finaltimename <- paste0("event_time_", j)
    colnames(newdf)[colnames(newdf)=="dummycol"] <- finaleventname
    colnames(newdf)[colnames(newdf)=="dummytimecol"] <- finaltimename
  }
  
  for(i in 1:nrow(newdf)){
    templist <- newdf$playevents[[i]]
    timelist <- newdf$eventtimes[[i]]
    huddle_test <- grep("huddle_break_offense", templist)
    if(is_empty(huddle_test)){
      set_test <- grep("line_set", templist)
      if(is_empty(set_test)){
        snap_test <- grep("ball_snap", templist)
        key <- snap_test[2]
      }else{
        key <- set_test[2]
      }
    }else{
      key <- huddle_test[2]
    }
    newlist <- templist[1:(key-1)]
    newtimelist <- timelist[1:(key-1)]
    newdf$event_count[i] <- key-1
    mark <- which(colnames(newdf)=="event_1")
    timemark <- mark + 1
    for(k in 1:15){
      newdf[mark][i,] <- newlist[k]
      newdf[timemark][i,] <- as.character(newtimelist[k])
      mark <- mark + 2
      timemark <- timemark + 2
    }
  }
  
  #### Figuring out time to throw
  newdf$time_to_throw <- ""
  for(b in 1:nrow(newdf)){
    ballsnapkey <- which(newdf[b,]=="ball_snap")
    passkey <- which(newdf[b,]=="pass_forward" | newdf[b,]=="pass_shovel")
    if(is_empty(passkey)){
      newdf$time_to_throw[b] <- NA
    } else{
      time1 <- newdf[ballsnapkey+1][b,]
      time1 <- sub(".* ", "", time1)
      time1_vec <- unlist(strsplit(time1, ":"))
      time1_nums <- as.numeric(time1_vec)
      time1_secs <- 3600*(time1_nums[1]) + 60*(time1_nums[2]) + time1_nums[3]
      
      time2 <- newdf[passkey+1][b,]
      time2 <- sub(".* ", "", time2)
      time2_vec <- unlist(strsplit(time2, ":"))
      time2_nums <- as.numeric(time2_vec)
      time2_secs <- 3600*(time2_nums[1]) + 60*(time2_nums[2]) + time2_nums[3]
      tmtothrow <- time2_secs - time1_secs
      newdf$time_to_throw[b] <- tmtothrow
    }
  }
  newdf$time_to_throw <- as.numeric(newdf$time_to_throw)
  
  poslist <- c("QB", "RB", "FB", "WR", "TE", "T", "G", "C", "NT", "DT", "DE",
               "LB", "OLB", "ILB", "MLB", "CB", "DB", "FS", "SS")
  
  for(pos in poslist){
    regex <- paste0("^", pos, "\\d+$")
    df_cols <- grep(regex, colnames(newdf))
    df_pos <- tibble(newdf[, df_cols])
    df_pos <- df_pos |> 
      mutate(non_na_count = rowSums(!is.na(across(everything()))))
    colname <- paste0(pos, "count")
    newdf$dummyvar <- df_pos$non_na_count
    colnames(newdf)[colnames(newdf)=="dummyvar"] <- colname
  }
  
  newdf <- newdf |> 
    mutate(QBtotal = QBcount,
           backstotal = RBcount + FBcount,
           WRtotal = WRcount,
           TEtotal = TEcount,
           olinetotal = Tcount + Gcount + Ccount,
           dlinetotal = NTcount + DTcount + DEcount,
           linebackerstotal = LBcount + ILBcount + MLBcount + OLBcount,
           secondarytotal = DBcount + CBcount + FScount + SScount)
  
  newdf <- newdf |> 
    mutate(Off_Personnel = paste0(backstotal, "-", WRtotal, "-", TEtotal),
           Def_Personnel = case_when(
             secondarytotal < 5 ~ paste0(dlinetotal, "-", linebackerstotal),
             secondarytotal == 5 ~ "Nickel",
             secondarytotal == 6 ~ "Dime",
             secondarytotal == 7 ~ "Quarter",
             secondarytotal > 7 ~ "Prevent"
           ))
  newdf <- newdf |> relocate(Off_Personnel, .after = off_form) |> 
    relocate(Def_Personnel, .after = manzone)
  
  newdf <- newdf |> select(-(playevents:eventtimes))
  newdf$motionplayer <- as.character(newdf$motionplayer)
  newdf$pressureplayers <- as.numeric(newdf$pressureplayers)
  newdf$inCoverage <- as.numeric(newdf$inCoverage)
  

  
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
  myfilename <- paste0(team, "personnel.csv")
  write.csv(newdf, myfilename)
  print(team)
}

df <- df[-(nrow(df)),]

# Defensive Personnel -----------------------------------------------------

for(team in tmlist){
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data/Defense by Team")
  firstfilename <- paste0(team, "file.rds")
  mydf <- readRDS(firstfilename)
  mydf <- mydf |>
    mutate(PossTmDiff = if_else(possessionTeam==homeTeamAbbr, 
                                preSnapHomeScore - preSnapVisitorScore,
                                preSnapVisitorScore - preSnapHomeScore),
           PossWinProb = if_else(possessionTeam==homeTeamAbbr, preSnapHomeTeamWinProbability,
                                 preSnapVisitorTeamWinProbability))
  mydf$nflId <- as.character(mydf$nflId)
  mydf$yardageGainedAfterTheCatch <- as.character(mydf$yardageGainedAfterTheCatch)
  
  newdf <- mydf |> 
    group_by(gameId, playId) |>  # Group by gameId and playId
    summarise(
      # Extract unique players for each position
      QB = list(unique(nflId[position == "QB"])),
      RB = list(unique(nflId[position == "RB"])),
      WR = list(unique(nflId[position == "WR"])),
      TE = list(unique(nflId[position == "TE"])),
      FB = list(unique(nflId[position == "FB"])),
      `T` = list(unique(nflId[position == "T"])),
      G = list(unique(nflId[position == "G"])),
      C = list(unique(nflId[position == "C"])),
      NT = list(unique(nflId[position == "NT"])),
      DT = list(unique(nflId[position == "DT"])),
      DE = list(unique(nflId[position == "DE"])),
      LB = list(unique(nflId[position == "LB"])),
      OLB = list(unique(nflId[position == "OLB"])),
      ILB = list(unique(nflId[position == "ILB"])),
      MLB = list(unique(nflId[position == "MLB"])),
      CB = list(unique(nflId[position == "CB"])),
      DB = list(unique(nflId[position == "DB"])),
      FS = list(unique(nflId[position == "FS"])),
      SS = list(unique(nflId[position == "SS"])),
      off_form = offenseFormation[1],
      receiverAlignment = receiverAlignment[1],
      runorpass = if_else(is.na(passResult[1]), "Run", "Pass"),
      coverage = pff_passCoverage[1],
      manzone = pff_manZone[1],
      quarter = quarter[1],
      gameClock = gameClock[1],
      timestart = time[1],
      timeend = time[length(mydf)],
      down = down[1],
      possessionTeam = possessionTeam[1],
      defensiveTeam = team,
      yardsToGo = yardsToGo[1],
      YdstoEZBef = YdstoEZBef[1],
      yardsGained = yardsGained[1],
      PossTmDiff = PossTmDiff[1],
      PossWinProb = PossWinProb[1],
      playevents = list(event[!is.na(event)]),
      eventtimes = list(time[!is.na(event)]),
      motionplayer = list(nflId[inMotionAtBallSnap == TRUE]),
      pressureplayers = list(displayName[causedPressure== TRUE]),
      routesrun = list(paste0(nflId[!is.na(routeRan)], " ", routeRan[!is.na(routeRan)])),
      targettedplayer = case_when(
        runorpass[1] == "Pass" ~ unique(nflId[wasTargettedReceiver == 1])[1], 
        TRUE ~ NA_character_
      ),
      receptionplayer = case_when(
        runorpass[1] == "Pass" & length(which(hadPassReception==1))>0 ~ 
          unique(nflId[hadPassReception == 1])[1], 
        TRUE ~ NA_character_
      ),
      yardsaftercatch = case_when(
        runorpass[1] == "Pass" & length(which(hadPassReception==1))>0 ~ 
          unique(yardageGainedAfterTheCatch)[1], 
        TRUE ~ NA_character_
      ),
      inCoverage = list(displayName[!is.na(pff_defensiveCoverageAssignment)]),
      .groups = "drop"
    ) |> 
    mutate(
      # Dynamically create position columns (e.g., QB1, QB2, ...)
      QB1 = sapply(QB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      QB2 = sapply(QB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      RB1 = sapply(RB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      RB2 = sapply(RB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      RB3 = sapply(RB, function(x) ifelse(length(x) >= 2, x[3], NA)),
      FB1 = sapply(FB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      FB2 = sapply(FB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      WR1 = sapply(WR, function(x) ifelse(length(x) >= 1, x[1], NA)),
      WR2 = sapply(WR, function(x) ifelse(length(x) >= 2, x[2], NA)),
      WR3 = sapply(WR, function(x) ifelse(length(x) >= 3, x[3], NA)),
      WR4 = sapply(WR, function(x) ifelse(length(x) >= 4, x[4], NA)),
      WR5 = sapply(WR, function(x) ifelse(length(x) >= 5, x[5], NA)),
      TE1 = sapply(TE, function(x) ifelse(length(x) >= 1, x[1], NA)),
      TE2 = sapply(TE, function(x) ifelse(length(x) >= 2, x[2], NA)),
      TE3 = sapply(TE, function(x) ifelse(length(x) >= 3, x[3], NA)),
      T1 = sapply(`T`, function(x) ifelse(length(x) >= 1, x[1], NA)),
      T2 = sapply(`T`, function(x) ifelse(length(x) >= 2, x[2], NA)),
      T3 = sapply(`T`, function(x) ifelse(length(x) >= 3, x[3], NA)),
      G1 = sapply(G, function(x) ifelse(length(x) >= 1, x[1], NA)),
      G2 = sapply(G, function(x) ifelse(length(x) >= 2, x[2], NA)),
      G3 = sapply(G, function(x) ifelse(length(x) >= 3, x[3], NA)),
      C1 = sapply(C, function(x) ifelse(length(x) >= 1, x[1], NA)),
      NT1 = sapply(NT, function(x) ifelse(length(x) >= 1, x[1], NA)),
      NT2 = sapply(NT, function(x) ifelse(length(x) >= 2, x[2], NA)),
      NT3 = sapply(NT, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DT1 = sapply(DT, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DT2 = sapply(DT, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DT3 = sapply(DT, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DE1 = sapply(DE, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DE2 = sapply(DE, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DE3 = sapply(DE, function(x) ifelse(length(x) >= 3, x[3], NA)),
      LB1 = sapply(LB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      LB2 = sapply(LB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      LB3 = sapply(LB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      OLB1 = sapply(OLB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      OLB2 = sapply(OLB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      OLB3 = sapply(OLB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      ILB1 = sapply(ILB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      ILB2 = sapply(ILB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      ILB3 = sapply(ILB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      MLB1 = sapply(MLB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      MLB2 = sapply(MLB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      MLB3 = sapply(MLB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      CB1 = sapply(CB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      CB2 = sapply(CB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      CB3 = sapply(CB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      CB4 = sapply(CB, function(x) ifelse(length(x) >= 4, x[4], NA)),
      CB5 = sapply(CB, function(x) ifelse(length(x) >= 5, x[5], NA)),
      DB1 = sapply(DB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DB2 = sapply(DB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DB3 = sapply(DB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DB4 = sapply(DB, function(x) ifelse(length(x) >= 4, x[4], NA)),
      DB5 = sapply(DB, function(x) ifelse(length(x) >= 5, x[5], NA)),
      DB6 = sapply(DB, function(x) ifelse(length(x) >= 6, x[6], NA)),
      FS1 = sapply(FS, function(x) ifelse(length(x) >= 1, x[1], NA)),
      FS2 = sapply(FS, function(x) ifelse(length(x) >= 2, x[2], NA)),
      SS1 = sapply(SS, function(x) ifelse(length(x) >= 1, x[1], NA)),
      SS2 = sapply(SS, function(x) ifelse(length(x) >= 2, x[2], NA)),
    ) |> 
    select(-(QB:SS))
  
  uniqdf <- mydf |> distinct(gameId, playId, nflId, .keep_all = TRUE)
  
  newdf$rushingplayer <- ""
  newdf$sackdefender <- ""
  newdf$sackyards <- ""
  newdf$interceptiondefender <- ""
  newdf$interceptionyards <- ""
  newdf$fumbleplayer <- ""
  newdf$fumblelost <- ""
  for(z in 1:nrow(newdf)){
    fumbdf <- uniqdf |> filter(playId==newdf$playId[z])
    fumbdf <- fumbdf |> filter(position %in% c("QB", "RB", "FB", "WR", "TE", "T",
                                               "G", "C"))
    fumbcheck <- length(unique(fumbdf$fumbles))
    if(isTRUE(fumbcheck==1)){
      newdf$fumbleplayer[z] <- NA
      newdf$fumblelost[z] <- NA
    } else{
      newdf$fumbleplayer[z] <- fumbdf$nflId[fumbdf$fumbles==1]
      newdf$fumblelost[z] <- fumbdf$fumbleLost[fumbdf$fumbles==1]
    }
    if(newdf$runorpass[z]=="Pass"){
      newdf$rushingplayer[z] <- NA
      intdf <- uniqdf |> filter(playId==newdf$playId[z])
      intcheck <- length(unique(intdf$hadInterception))
      sackcheck <- length(unique(intdf$sackYardsAsDefense))
      if(isTRUE(intcheck==1)){
        newdf$interceptiondefender[z] <- NA
        newdf$interceptionyards[z] <- NA
      } else{
        newdf$interceptiondefender[z] <- intdf$nflId[intdf$hadInterception==1]
        newdf$interceptionyards[z] <- intdf$interceptionYards[intdf$hadInterception==1]
      }
      if(isTRUE(sackcheck==1)){
        newdf$sackdefender[z] <- NA
        newdf$sackyards[z] <- NA
      } else{
        newdf$sackdefender[z] <- intdf$nflId[intdf$sackYardsAsDefense!=0]
        newdf$sackyards[z] <- intdf$sackYardsAsDefense[intdf$sackYardsAsDefense!=0]
      }
      
    } else{
      uniqplaydf <- uniqdf |> filter(playId==newdf$playId[z])
      player <- uniqplaydf$nflId[uniqplaydf$hadRushAttempt==1]
      newdf$rushingplayer[z] <- player
      newdf$interceptiondefender[z] <- NA
      newdf$interceptionyards[z] <- NA
      newdf$sackdefender[z] <- NA
      newdf$sackyards[z] <- NA
    }
  }

  
  newdf$yardsaftercatch <- as.numeric(newdf$yardsaftercatch)
  newdf <- newdf |> 
    mutate(yardsbeforecatch = yardsGained - yardsaftercatch)
  newdf <- newdf |> relocate(yardsbeforecatch, .before = yardsaftercatch)
  newdf <- newdf |> relocate(yardsGained, .before = yardsbeforecatch)
  
  newdf$routescount <- " "
  newdf <- newdf |> relocate(routescount, .before = routesrun)
  for(f in 1: nrow(newdf)){
    mylist <- newdf$routesrun[[f]]
    newlist <- unique(mylist)
    routenum <- length(newlist)
    if(routenum==1){
      routenum <- 0
    }
    newdf$routescount[f] <- routenum
    newdf$routesrun[[f]] <- newlist
  }
  max_length <- max(sapply(newdf$routesrun, length))
  padded_values <- lapply(newdf$routesrun, function(x) {
    length(x) <- max_length
    return(x)
  })
  values_df <- do.call(rbind, lapply(padded_values, function(x) as.data.frame(t(x))))
  colnames(values_df) <- paste0("route_", seq_len(ncol(values_df)))
  newdf <- cbind(newdf, values_df)
  newdf <- newdf |> relocate((route_1:route_5), .after = routesrun)
  newdf <- newdf |> select(-routesrun)
  
  
  for(g in 1:nrow(newdf)){
    mylist <- newdf$motionplayer[[g]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    if(!is_empty(player)){
      newdf$motionplayer[g] <- player
    }else{
      newdf$motionplayer[g] <- NA
    }
  }
  
  for(h in 1:nrow(newdf)){
    mylist <- newdf$inCoverage[[h]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    newdf$inCoverage[h] <- length(player)
  }
  
  for(k in 1:nrow(newdf)){
    mylist <- newdf$pressureplayers[[k]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    newdf$pressureplayers[k] <- length(player)
  }
  
  newdf$event_count <- ""
  for(j in 1:15){
    newdf$dummycol <- ""
    newdf$dummytimecol <- ""
    finaleventname <- paste0("event_", j)
    finaltimename <- paste0("event_time_", j)
    colnames(newdf)[colnames(newdf)=="dummycol"] <- finaleventname
    colnames(newdf)[colnames(newdf)=="dummytimecol"] <- finaltimename
  }
  
  for(i in 1:nrow(newdf)){
    templist <- newdf$playevents[[i]]
    timelist <- newdf$eventtimes[[i]]
    huddle_test <- grep("huddle_break_offense", templist)
    if(is_empty(huddle_test)){
      set_test <- grep("line_set", templist)
      if(is_empty(set_test)){
        snap_test <- grep("ball_snap", templist)
        key <- snap_test[2]
      }else{
        key <- set_test[2]
      }
    }else{
      key <- huddle_test[2]
    }
    newlist <- templist[1:(key-1)]
    newtimelist <- timelist[1:(key-1)]
    newdf$event_count[i] <- key-1
    mark <- which(colnames(newdf)=="event_1")
    timemark <- mark + 1
    for(k in 1:15){
      newdf[mark][i,] <- newlist[k]
      newdf[timemark][i,] <- as.character(newtimelist[k])
      mark <- mark + 2
      timemark <- timemark + 2
    }
  }
  
  poslist <- c("QB", "RB", "FB", "WR", "TE", "T", "G", "C", "NT", "DT", "DE",
               "LB", "OLB", "ILB", "MLB", "CB", "DB", "FS", "SS")
  
  for(pos in poslist){
    regex <- paste0("^", pos, "\\d+$")
    df_cols <- grep(regex, colnames(newdf))
    df_pos <- tibble(newdf[, df_cols])
    df_pos <- df_pos |> 
      mutate(non_na_count = rowSums(!is.na(across(everything()))))
    colname <- paste0(pos, "count")
    newdf$dummyvar <- df_pos$non_na_count
    colnames(newdf)[colnames(newdf)=="dummyvar"] <- colname
  }
  
  newdf <- newdf |> 
    mutate(QBtotal = QBcount,
           backstotal = RBcount + FBcount,
           WRtotal = WRcount,
           TEtotal = TEcount,
           olinetotal = Tcount + Gcount + Ccount,
           dlinetotal = NTcount + DTcount + DEcount,
           linebackerstotal = LBcount + ILBcount + MLBcount + OLBcount,
           secondarytotal = DBcount + CBcount + FScount + SScount)
  
  newdf <- newdf |> 
    mutate(Off_Personnel = paste0(backstotal, "-", WRtotal, "-", TEtotal),
           Def_Personnel = case_when(
             secondarytotal < 5 ~ paste0(dlinetotal, "-", linebackerstotal),
             secondarytotal == 5 ~ "Nickel",
             secondarytotal == 6 ~ "Dime",
             secondarytotal == 7 ~ "Quarter",
             secondarytotal > 7 ~ "Prevent"
           ))
  newdf <- newdf |> relocate(Off_Personnel, .after = off_form) |> 
    relocate(Def_Personnel, .after = manzone)
  
  newdf <- newdf |> select(-(playevents:eventtimes))
  newdf$motionplayer <- as.character(newdf$motionplayer)
  newdf$pressureplayers <- as.numeric(newdf$pressureplayers)
  newdf$inCoverage <- as.numeric(newdf$inCoverage)
  
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Defensive Personnel Data")
  myfilename <- paste0(team, "personnel.csv")
  write.csv(newdf, myfilename)
  print(team)
}


# Offensive Personnel -----------------------------------------------------

for(team in tmlist){
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data/Offense by Team")
  firstfilename <- paste0(team, "file.rds")
  mydf <- readRDS(firstfilename)
  
  mydf <- mydf |>
    mutate(PossTmDiff = if_else(possessionTeam==homeTeamAbbr, 
                                preSnapHomeScore - preSnapVisitorScore,
                                preSnapVisitorScore - preSnapHomeScore),
           PossWinProb = if_else(possessionTeam==homeTeamAbbr, preSnapHomeTeamWinProbability,
                                 preSnapVisitorTeamWinProbability),
           defensiveTeam = if_else(possessionTeam==homeTeamAbbr, visitorTeamAbbr, homeTeamAbbr))
  mydf$nflId <- as.character(mydf$nflId)
  mydf$yardageGainedAfterTheCatch <- as.character(mydf$yardageGainedAfterTheCatch)

  # 
  newdf <- mydf |> 
    group_by(gameId, playId) |>  # Group by gameId and playId
    summarise(
      # Extract unique players for each position
      QB = list(unique(nflId[position == "QB"])),
      RB = list(unique(nflId[position == "RB"])),
      WR = list(unique(nflId[position == "WR"])),
      TE = list(unique(nflId[position == "TE"])),
      FB = list(unique(nflId[position == "FB"])),
      `T` = list(unique(nflId[position == "T"])),
      G = list(unique(nflId[position == "G"])),
      C = list(unique(nflId[position == "C"])),
      NT = list(unique(nflId[position == "NT"])),
      DT = list(unique(nflId[position == "DT"])),
      DE = list(unique(nflId[position == "DE"])),
      LB = list(unique(nflId[position == "LB"])),
      OLB = list(unique(nflId[position == "OLB"])),
      ILB = list(unique(nflId[position == "ILB"])),
      MLB = list(unique(nflId[position == "MLB"])),
      CB = list(unique(nflId[position == "CB"])),
      DB = list(unique(nflId[position == "DB"])),
      FS = list(unique(nflId[position == "FS"])),
      SS = list(unique(nflId[position == "SS"])),
      off_form = offenseFormation[1],
      receiverAlignment = receiverAlignment[1],
      runorpass = if_else(is.na(passResult[1]), "Run", "Pass"),
      coverage = pff_passCoverage[1],
      manzone = pff_manZone[1],
      quarter = quarter[1],
      gameClock = gameClock[1],
      down = down[1],
      possessionTeam = team,
      defensiveTeam = defensiveTeam[1],
      yardsToGo = yardsToGo[1],
      YdstoEZBef = YdstoEZBef[1],
      yardsGained = yardsGained[1],
      PossTmDiff = PossTmDiff[1],
      PossWinProb = PossWinProb[1],
      playevents = list(event[!is.na(event)]),
      eventtimes = list(time[!is.na(event)]),
      motionplayer = list(nflId[inMotionAtBallSnap == TRUE]),
      pressureplayers = list(displayName[causedPressure== TRUE]),
      routesrun = list(paste0(nflId[!is.na(routeRan)], " ", routeRan[!is.na(routeRan)])),
      targettedplayer = case_when(
        runorpass[1] == "Pass" ~ unique(nflId[wasTargettedReceiver == 1])[1], 
        TRUE ~ NA_character_
      ),
      receptionplayer = case_when(
        runorpass[1] == "Pass" & length(which(hadPassReception==1))>0 ~ 
          unique(nflId[hadPassReception == 1])[1], 
        TRUE ~ NA_character_
      ),
      yardsaftercatch = case_when(
        runorpass[1] == "Pass" & length(which(hadPassReception==1))>0 ~ 
          unique(yardageGainedAfterTheCatch)[1], 
        TRUE ~ NA_character_
      ),
      inCoverage = list(displayName[!is.na(pff_defensiveCoverageAssignment)]),
      .groups = "drop"
    ) |> 
    mutate(
      # Dynamically create position columns (e.g., QB1, QB2, ...)
      QB1 = sapply(QB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      QB2 = sapply(QB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      RB1 = sapply(RB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      RB2 = sapply(RB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      RB3 = sapply(RB, function(x) ifelse(length(x) >= 2, x[3], NA)),
      FB1 = sapply(FB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      FB2 = sapply(FB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      WR1 = sapply(WR, function(x) ifelse(length(x) >= 1, x[1], NA)),
      WR2 = sapply(WR, function(x) ifelse(length(x) >= 2, x[2], NA)),
      WR3 = sapply(WR, function(x) ifelse(length(x) >= 3, x[3], NA)),
      WR4 = sapply(WR, function(x) ifelse(length(x) >= 4, x[4], NA)),
      WR5 = sapply(WR, function(x) ifelse(length(x) >= 5, x[5], NA)),
      TE1 = sapply(TE, function(x) ifelse(length(x) >= 1, x[1], NA)),
      TE2 = sapply(TE, function(x) ifelse(length(x) >= 2, x[2], NA)),
      TE3 = sapply(TE, function(x) ifelse(length(x) >= 3, x[3], NA)),
      T1 = sapply(`T`, function(x) ifelse(length(x) >= 1, x[1], NA)),
      T2 = sapply(`T`, function(x) ifelse(length(x) >= 2, x[2], NA)),
      T3 = sapply(`T`, function(x) ifelse(length(x) >= 3, x[3], NA)),
      G1 = sapply(G, function(x) ifelse(length(x) >= 1, x[1], NA)),
      G2 = sapply(G, function(x) ifelse(length(x) >= 2, x[2], NA)),
      G3 = sapply(G, function(x) ifelse(length(x) >= 3, x[3], NA)),
      C1 = sapply(C, function(x) ifelse(length(x) >= 1, x[1], NA)),
      NT1 = sapply(NT, function(x) ifelse(length(x) >= 1, x[1], NA)),
      NT2 = sapply(NT, function(x) ifelse(length(x) >= 2, x[2], NA)),
      NT3 = sapply(NT, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DT1 = sapply(DT, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DT2 = sapply(DT, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DT3 = sapply(DT, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DE1 = sapply(DE, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DE2 = sapply(DE, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DE3 = sapply(DE, function(x) ifelse(length(x) >= 3, x[3], NA)),
      LB1 = sapply(LB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      LB2 = sapply(LB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      LB3 = sapply(LB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      OLB1 = sapply(OLB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      OLB2 = sapply(OLB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      OLB3 = sapply(OLB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      ILB1 = sapply(ILB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      ILB2 = sapply(ILB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      ILB3 = sapply(ILB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      MLB1 = sapply(MLB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      MLB2 = sapply(MLB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      MLB3 = sapply(MLB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      CB1 = sapply(CB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      CB2 = sapply(CB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      CB3 = sapply(CB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      CB4 = sapply(CB, function(x) ifelse(length(x) >= 4, x[4], NA)),
      CB5 = sapply(CB, function(x) ifelse(length(x) >= 5, x[5], NA)),
      DB1 = sapply(DB, function(x) ifelse(length(x) >= 1, x[1], NA)),
      DB2 = sapply(DB, function(x) ifelse(length(x) >= 2, x[2], NA)),
      DB3 = sapply(DB, function(x) ifelse(length(x) >= 3, x[3], NA)),
      DB4 = sapply(DB, function(x) ifelse(length(x) >= 4, x[4], NA)),
      DB5 = sapply(DB, function(x) ifelse(length(x) >= 5, x[5], NA)),
      DB6 = sapply(DB, function(x) ifelse(length(x) >= 6, x[6], NA)),
      FS1 = sapply(FS, function(x) ifelse(length(x) >= 1, x[1], NA)),
      FS2 = sapply(FS, function(x) ifelse(length(x) >= 2, x[2], NA)),
      SS1 = sapply(SS, function(x) ifelse(length(x) >= 1, x[1], NA)),
      SS2 = sapply(SS, function(x) ifelse(length(x) >= 2, x[2], NA)),
    ) |> 
    select(-(QB:SS))
  
  uniqdf <- mydf |> distinct(gameId, playId, nflId, .keep_all = TRUE)
  
  newdf$rushingplayer <- ""
  newdf$sackdefender <- ""
  newdf$sackyards <- ""
  newdf$interceptiondefender <- ""
  newdf$interceptionyards <- ""
  newdf$fumbleplayer <- ""
  newdf$fumblelost <- ""
  for(z in 1:nrow(newdf)){
    fumbdf <- uniqdf |> filter(gameId==newdf$gameId[z] & playId==newdf$playId[z])
    fumbdf <- fumbdf |> filter(position %in% c("QB", "RB", "FB", "WR", "TE", "T",
                                               "G", "C"))
    fumbcheck <- length(unique(fumbdf$fumbles))
    if(isTRUE(fumbcheck==1)){
      newdf$fumbleplayer[z] <- NA
      newdf$fumblelost[z] <- NA
    } else{
      newdf$fumbleplayer[z] <- fumbdf$nflId[fumbdf$fumbles==1]
      newdf$fumblelost[z] <- fumbdf$fumbleLost[fumbdf$fumbles==1]
    }
    if(newdf$runorpass[z]=="Pass"){
      newdf$rushingplayer[z] <- NA
      intdf <- uniqdf |> filter(gameId==newdf$gameId[z] & playId==newdf$playId[z])
      intcheck <- length(unique(intdf$hadInterception))
      sackcheck <- length(unique(intdf$sackYardsAsDefense))
      if(isTRUE(intcheck==1)){
        newdf$interceptiondefender[z] <- NA
        newdf$interceptionyards[z] <- NA
      } else{
        newdf$interceptiondefender[z] <- intdf$nflId[intdf$hadInterception==1]
        newdf$interceptionyards[z] <- intdf$interceptionYards[intdf$hadInterception==1]
      }
      if(isTRUE(sackcheck==1)){
        newdf$sackdefender[z] <- NA
        newdf$sackyards[z] <- NA
      } else{
        newdf$sackdefender[z] <- intdf$nflId[intdf$sackYardsAsDefense!=0]
        newdf$sackyards[z] <- intdf$sackYardsAsDefense[intdf$sackYardsAsDefense!=0]
      }
      
    } else{
      uniqplaydf <- uniqdf |> filter(gameId==newdf$gameId[z] & playId==newdf$playId[z])
      player <- uniqplaydf$nflId[uniqplaydf$hadRushAttempt==1]
      newdf$rushingplayer[z] <- player
      newdf$interceptiondefender[z] <- NA
      newdf$interceptionyards[z] <- NA
      newdf$sackdefender[z] <- NA
      newdf$sackyards[z] <- NA
    }
  }
  
  newdf$yardsaftercatch <- as.numeric(newdf$yardsaftercatch)
  newdf <- newdf |> 
    mutate(yardsbeforecatch = yardsGained - yardsaftercatch)
  newdf <- newdf |> relocate(yardsbeforecatch, .before = yardsaftercatch)
  newdf <- newdf |> relocate(yardsGained, .before = yardsbeforecatch)
  
  newdf$routescount <- " "
  newdf <- newdf |> relocate(routescount, .before = routesrun)
  for(f in 1: nrow(newdf)){
    mylist <- newdf$routesrun[[f]]
    newlist <- unique(mylist)
    routenum <- length(newlist)
    if(routenum==1){
      routenum <- 0
    }
    newdf$routescount[f] <- routenum
    newdf$routesrun[[f]] <- newlist
  }
  max_length <- max(sapply(newdf$routesrun, length))
  padded_values <- lapply(newdf$routesrun, function(x) {
    length(x) <- max_length
    return(x)
  })
  values_df <- do.call(rbind, lapply(padded_values, function(x) as.data.frame(t(x))))
  colnames(values_df) <- paste0("route_", seq_len(ncol(values_df)))
  newdf <- cbind(newdf, values_df)
  newdf <- newdf |> relocate((route_1:route_5), .after = routesrun)
  newdf <- newdf |> select(-routesrun)
  
  for(g in 1:nrow(newdf)){
    mylist <- newdf$motionplayer[[g]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    if(!is_empty(player)){
      newdf$motionplayer[g] <- player
    }else{
      newdf$motionplayer[g] <- NA
    }
  }
  
  for(h in 1:nrow(newdf)){
    mylist <- newdf$inCoverage[[h]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    newdf$inCoverage[h] <- length(player)
  }
  
  for(k in 1:nrow(newdf)){
    mylist <- newdf$pressureplayers[[k]]
    cleaned_list <- unlist(lapply(mylist, function(x) gsub("\\s+", "", x)))
    newlist <- unique(cleaned_list)
    player <- newlist[!is.na(newlist)]
    newdf$pressureplayers[k] <- length(player)
  }
  
  newdf$event_count <- ""
  for(j in 1:15){
    newdf$dummycol <- ""
    newdf$dummytimecol <- ""
    finaleventname <- paste0("event_", j)
    finaltimename <- paste0("event_time_", j)
    colnames(newdf)[colnames(newdf)=="dummycol"] <- finaleventname
    colnames(newdf)[colnames(newdf)=="dummytimecol"] <- finaltimename
  }
  
  for(i in 1:nrow(newdf)){
    templist <- newdf$playevents[[i]]
    timelist <- newdf$eventtimes[[i]]
    huddle_test <- grep("huddle_break_offense", templist)
    if(is_empty(huddle_test)){
      set_test <- grep("line_set", templist)
      if(is_empty(set_test)){
        snap_test <- grep("ball_snap", templist)
        key <- snap_test[2]
      }else{
        key <- set_test[2]
      }
    }else{
      key <- huddle_test[2]
    }
    newlist <- templist[1:(key-1)]
    newtimelist <- timelist[1:(key-1)]
    newdf$event_count[i] <- key-1
    mark <- which(colnames(newdf)=="event_1")
    timemark <- mark + 1
    for(k in 1:15){
      newdf[mark][i,] <- newlist[k]
      newdf[timemark][i,] <- as.character(newtimelist[k])
      mark <- mark + 2
      timemark <- timemark + 2
    }
  }
  
  
  poslist <- c("QB", "RB", "FB", "WR", "TE", "T", "G", "C", "NT", "DT", "DE",
               "LB", "OLB", "ILB", "MLB", "CB", "DB", "FS", "SS")
  
  for(pos in poslist){
    regex <- paste0("^", pos, "\\d+$")
    df_cols <- grep(regex, colnames(newdf))
    df_pos <- tibble(newdf[, df_cols])
    df_pos <- df_pos |> 
      mutate(non_na_count = rowSums(!is.na(across(everything()))))
    colname <- paste0(pos, "count")
    newdf$dummyvar <- df_pos$non_na_count
    colnames(newdf)[colnames(newdf)=="dummyvar"] <- colname
  }
  
  newdf <- newdf |> 
    mutate(QBtotal = QBcount,
           backstotal = RBcount + FBcount,
           WRtotal = WRcount,
           TEtotal = TEcount,
           olinetotal = Tcount + Gcount + Ccount,
           dlinetotal = NTcount + DTcount + DEcount,
           linebackerstotal = LBcount + ILBcount + MLBcount + OLBcount,
           secondarytotal = DBcount + CBcount + FScount + SScount)
  
  newdf <- newdf |> 
    mutate(Off_Personnel = paste0(backstotal, "-", WRtotal, "-", TEtotal),
           Def_Personnel = case_when(
             secondarytotal < 5 ~ paste0(dlinetotal, "-", linebackerstotal),
             secondarytotal == 5 ~ "Nickel",
             secondarytotal == 6 ~ "Dime",
             secondarytotal == 7 ~ "Quarter",
             secondarytotal > 7 ~ "Prevent"
           ))
  newdf <- newdf |> relocate(Off_Personnel, .after = off_form) |> 
    relocate(Def_Personnel, .after = manzone)
  
  newdf <- newdf |> select(-(playevents:eventtimes))
  newdf$motionplayer <- as.character(newdf$motionplayer)
  newdf$pressureplayers <- as.numeric(newdf$pressureplayers)
  newdf$inCoverage <- as.numeric(newdf$inCoverage)
  
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
  myfilename <- paste0(team, "personnel.csv")
  write.csv(newdf, myfilename)
  print(team)
}



