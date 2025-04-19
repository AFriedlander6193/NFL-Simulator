library(tidyverse)
library(keras)
library(broom)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(sure)
library(rlist)
library(brms)
library(cmdstanr)
library(rvest)
library(stringr)
library(fs)
library(XML)
library(xml2)
library(togglr)
library(glmnet)
library(tidymodels)
library(yardstick)
library(caret)
library(rms)
library(nnet)
library(purrr)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- suppressMessages(read_csv("AllOffensivePersonnel.csv"))
offdf <- offdf |> 
  mutate(simple_personnel = paste0(RBcount, TEcount))

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
posroutedf <- read_csv("PosRoutesRan.csv")
genericroutecompdf <- read_csv("generic_route_comp_probs.csv")
teamroutecompdf <- read_csv("team_route_comp_probs.csv")
genericrushposdf <- read_csv("generic_rush_pos.csv")
teamrushposdf <- read_csv("team_rush_pos.csv")
fumblostdf <- read_csv("FumbleLostProb.csv")
passing <- read_csv("passingnflID+pffID.csv")
rushrecblock <- read_csv("rushrecblocknflID+pffID.csv")
defense <- read_csv("defensenflID+pffID.csv")

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Formation Probabilities (Offense)")
byteam <- read_csv("byteam.csv")
formbydowntogo <- read_csv("formbydown+togo.csv")
runpassbyteamform <- read_csv("runpassbyteam+form.csv")
yardsbyteamform <- read_csv("yardsbyteam+form+runpass.csv")
turnoversdf <- read_csv("TurnoversbyTeam.csv")
routesbyformtogo <- read_csv("RouteProbabilities(Form+ToGo).csv")
routesbyformteam <- read_csv("RouteProbabilities(Form+Team).csv")

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
off_personnel_model <- readRDS("Personnel_Ord_Reg_Model.rds")
off_formation_model <- readRDS("Formation_Ord_Reg_Model.rds")
runpass_model <- readRDS("RunPass_Log_Reg_Model.rds")
targetted_route_model <- readRDS("Targetted_Route_Multinom_Model.rds")
passing_yds_model <- readRDS("PassingYdsModel.rds")
rushing_yds_model <- readRDS("RushingYdsModel.rds")
sack_model <- readRDS("SackModel.rds")
sack_yards_model <- readRDS("SackYardsModel.rds")
rushfumble_model <- readRDS("RushFumbleModel.rds")
interception_model <- readRDS("InterceptionModel.rds")
int_yds_model <- readRDS("IntYdsModel.rds")
defpersonnelmodel <- readRDS("DefPersonnel_Multinom_Reg_Model.rds")
defcoveragemodel <- readRDS("Def_Coverage_Ord_Reg_Model.rds")
timeToThrowmodel <- readRDS("TimetoThrow_Model.rds")
coverplayersmodel <- readRDS("Cover_Players_Model.rds")
passresultmodel <- readRDS("PassResult_Multinom_Reg_Model.rds")
pressureplayersmodel <- readRDS("PressurePlayersModel.rds")

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models/Route Models")
angle_model <- readRDS("ANGLERoute_Ord_Reg_Model.rds")
corner_model <- readRDS("CORNERRoute_Ord_Reg_Model.rds")
cross_model <- readRDS("CROSSRoute_Ord_Reg_Model.rds")
flat_model <- readRDS("FLATRoute_Ord_Reg_Model.rds")
go_model <- readRDS("GORoute_Ord_Reg_Model.rds")
hitch_model <- readRDS("HITCHRoute_Ord_Reg_Model.rds")
in_model <- readRDS("INRoute_Ord_Reg_Model.rds")
out_model <- readRDS("OUTRoute_Ord_Reg_Model.rds")
post_model <- readRDS("POSTRoute_Ord_Reg_Model.rds")
screen_model <- readRDS("SCREENRoute_Ord_Reg_Model.rds")
slant_model <- readRDS("SLANTRoute_Ord_Reg_Model.rds")
wheel_model <- readRDS("WHEELRoute_Ord_Reg_Model.rds")
route_count_model <- readRDS("Route_Count_Ord_Reg_Model.rds")

### FIELD GOAL STUFF
setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
fgdf <- readRDS("FirstDownLogRegResults.rds")
fgdf$FGAtt <- as.factor(fgdf$FGAtt)
fgmodf <- glm(FGAtt ~ FGDist + Quarter + Time2 + PossTmMargin + Down + ToGo, 
              data = fgdf, family = "binomial")
fgmakedf <- fgdf |> group_by(FGDist) |> 
  summarise(make_prob = mean(FGMakeProb))
###




passing$nflId <- as.character(passing$nflId)
rushrecblock$nflId <- as.character(rushrecblock$nflId)
defense$nflId <- as.character(defense$nflId)


# Time Each Play Takes ----------------------------------------------------

reg_snap_time <- function(){
  round(rnorm(1, 25, 5))
}

quick_snap_time <- function(){
  round(rnorm(1, 10, 1))
}

complete_pass_time <- function(){
  round(rnorm(1, 5, 2))
}

run_play_time <- function(){
  round(rnorm(1, 3, 1))
}

incomplete_pass_time <- function(){
  round(rnorm(1, 3, 1))
}

punt_time <- function(){
  round(rnorm(1, 9, 1.5))
}

fg_time <- function(){
  round(rnorm(1, 5, 1))
}


# Offensive Play Details ------------------------------------------------------------

choose_personnel <- function(posstm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter){
  modeldat <- data.frame(
    possessionTeam = posstm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo
  )
  probs <- predict(off_personnel_model, modeldat, type = "probs")
  personnels <- sort(unique(offdf$simple_personnel))
  sample(personnels, size = 1, prob = probs)
}

choose_def_personnel <- function(deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter, personnel){
  modeldat <- data.frame(
    defensiveTeam = deftm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo,
    simple_personnel = personnel
  )
  probs <- predict(defpersonnelmodel, modeldat, type = "probs")
  def_personnels <- sort(unique(offdf$simple_def_personnel))
  sample(def_personnels, size = 1, prob = probs)
}

choose_formation <- function(posstm, down, togo, YdsBef, posstmdiff, 
                             quarter_secs, quarter, personnel, def_personnel){
  modeldat <- data.frame(
    possessionTeam = posstm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    simple_personnel = personnel,
    simple_def_personnel = def_personnel
  )
  probs <- predict(off_formation_model, modeldat, type = "probs")
  formations <- sort(unique(offdf$off_form))
  sample(formations, size = 1, prob = probs)
}

choose_def_coverage <- function(deftm, down, togo, YdsBef, posstmdiff, quarter_secs,
                                quarter, personnel, def_personnel, formation){
  modeldat <- data.frame(
    defensiveTeam = deftm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    simple_personnel = personnel,
    simple_def_personnel = def_personnel,
    off_form = formation
  )
  probs <- predict(defcoveragemodel, modeldat, type = "probs")
  coverages <- sort(unique(offdf$def_simple_coverage))
  sample(coverages, size = 1, prob = probs)
}

runorpass <- function(posstm, down, togo, YdsBef, posstmdiff, quarter_secs, 
                      quarter, personnel, formation, def_personnel, coverage){
  modeldat <- data.frame(
    possessionTeam = posstm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    simple_personnel = personnel,
    off_form = formation,
    simple_def_personnel = def_personnel,
    def_simple_coverage = coverage
  )
  passprob <- predict(runpass_model, modeldat, type = "response")
  runprob <- 1 - passprob
  probs <- c(passprob, runprob)
  types <- c("Pass", "Run")
  sample(types, size = 1, prob = probs)
}


routeselection <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter){
  personnel <- choose_personnel(posstm, down, togo, YdsBef, posstmdiff, 
                                quarter_secs, quarter)
  def_personnel <- choose_def_personnel(deftm, down, togo, YdsBef, posstmdiff, 
                                        quarter_secs, quarter, personnel)
  formation <- choose_formation(posstm, down, togo, YdsBef, posstmdiff, 
                                quarter_secs, quarter, personnel, def_personnel)
  coverage <- choose_def_coverage(deftm, down, togo, YdsBef, posstmdiff, quarter_secs,
                                  quarter, personnel, def_personnel, formation)
  runpassselection <- runorpass(posstm, down, togo, YdsBef, posstmdiff, quarter_secs, 
                                quarter, personnel, formation, def_personnel, coverage)
  if(personnel %in% c("03", "02", "01", "00")){
    runpassselection <- "Pass"
  }
  if(runpassselection=="Pass"){
    modeldat <- data.frame(
      possessionTeam = posstm,
      QuarterSeconds = quarter_secs,
      quarter = quarter,
      PossTmDiff = posstmdiff,
      down = down,
      yardsToGo = togo,
      YdstoEZBef = YdsBef,
      simple_personnel = personnel,
      off_form = formation,
      runorpass = "Pass",
      defensiveTeam = deftm,
      simple_def_personnel = def_personnel,
      def_simple_coverage = coverage
    )
    
    coverx <- posterior_predict(coverplayersmodel, newdata = modeldat)
    coverplayers <- round(apply(coverx, 2, sample, size = 1),0)
    passrushers <- 11 - coverplayers
    
    routesrunprobs <- predict(route_count_model, modeldat, type = "probs")
    num_RB <- as.numeric(substr(personnel, 1, 1))
    num_TE <- as.numeric(substr(personnel, 2, 2))
    routesrunpossibilities <- c((num_RB+num_TE):6)
    newroutesrunprobs <- routesrunprobs[(num_RB+num_TE):6]
    routesrun <- sample(routesrunpossibilities, size=1, prob=newroutesrunprobs)
    
    anglect <- 0
    cornerct <- 0
    crossct <- 0
    flatct <- 0
    goct <- 0
    hitchct <- 0
    inct <- 0
    outct <- 0
    postct <- 0
    screenct <- 0
    slantct <- 0
    wheelct <- 0
    chosenroutelist <- c()
    for(n in 1:routesrun){
      routedat <- data.frame(
        possessionTeam = posstm,
        QuarterSeconds = quarter_secs,
        quarter = quarter,
        PossTmDiff = posstmdiff,
        down = down,
        yardsToGo = togo,
        YdstoEZBef = YdsBef,
        simple_personnel = personnel,
        off_form = formation,
        ANGLE_count = anglect,
        CORNER_count = cornerct,
        CROSS_count = crossct,
        FLAT_count = flatct,
        GO_count = goct,
        HITCH_count = hitchct,
        IN_count = inct,
        OUT_count = outct,
        POST_count = postct,
        SCREEN_count = screenct,
        SLANT_count = slantct,
        WHEEL_count = wheelct,
        defensiveTeam = deftm,
        simple_def_personnel = def_personnel,
        def_simple_coverage = coverage
      )
      
      angleprobs <- predict(angle_model, routedat, type = "probs")
      cornerprobs <- predict(corner_model, routedat, type = "probs")
      crossprobs <- predict(cross_model, routedat, type = "probs")
      flatprobs <- predict(flat_model, routedat, type = "probs")
      goprobs <- predict(go_model, routedat, type = "probs")
      hitchprobs <- predict(hitch_model, routedat, type = "probs")
      inprobs <- predict(in_model, routedat, type = "probs")
      outprobs <- predict(out_model, routedat, type = "probs")
      postprobs <- predict(post_model, routedat, type = "probs")
      screenprobs <- predict(screen_model, routedat, type = "probs")
      slantprobs <- predict(slant_model, routedat, type = "probs")
      wheelprobs <- predict(wheel_model, routedat, type = "probs")
      allprobs <- c(angleprobs, cornerprobs, crossprobs, flatprobs, goprobs, hitchprobs,
                    inprobs, outprobs, postprobs, screenprobs, slantprobs, wheelprobs)
      probsdf <- as.data.frame(t(allprobs))
      colnames(probsdf) <- c("a0", "a1", "a2", "co0", "co1", "co2", "co3", "cr0",
                             "cr1", "cr2", "cr3", "cr4", "f0", "f1", "f2", "f3",
                             "f4", "g0", "g1", "g2", "g3", "g4", "h0", "h1", "h2",
                             "h3", "h4", "h5", "i0", "i1", "i2", "i3", "i4",
                             "o0", "o1", "o2", "o3", "o4", "p0", "p1", "p2", "p3",
                             "sc0", "sc1", "sc2", "sc3", "sc4", "sl0", "sl1",
                             "sl2", "sl3", "sl4", "w0", "w1", "w2")
      aprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("a", anglect+1))])
      coprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("co", cornerct+1))])
      crprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("cr", crossct+1))])
      fprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("f", flatct+1))])
      gprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("g", goct+1))])
      hprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("h", hitchct+1))])
      iprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("i", inct+1))])
      oprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("o", outct+1))])
      pprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("p", postct+1))])
      scprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("sc", screenct+1))])
      slprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("sl", slantct+1))])
      wprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("w", wheelct+1))])
      probs <- c(aprob, coprob, crprob, fprob, gprob, hprob, iprob, oprob, pprob,
                 scprob, slprob, wprob)
      route_options <- c("angle", "corner", "cross", "flat", "go", "hitch", "in",
                         "out", "post", "screen", "slant", "wheel")
      chosen_route <- sample(route_options, 1, prob = probs)
      varname <- paste0(chosen_route, "ct")
      value <- get(varname)
      chosenroutelist <- append(chosenroutelist, chosen_route)
      assign(varname, get(varname) + 1)
    }
    
    #### GETS SPECIFIC OFFENSIVE AND DEFENSIVE PLAYERS ON THE FIELD
    mydf <- data.frame(
      RB = as.numeric(substr(personnel, 1, 1)),
      WR = length(chosenroutelist) - as.numeric(substr(personnel, 1, 1)) -
        as.numeric(substr(personnel, 2,2)),
      TE = as.numeric(substr(personnel, 2, 2))
    )
    lengthdf <- offdf |> filter(possessionTeam==posstm)
    plays <- nrow(lengthdf)
    quarterbacks <- passing |> filter(teamAbbr==posstm & position=="QB")
    quarterbacks <- quarterbacks |> filter(!is.na(attempts))
    onfieldQB <- quarterbacks$nflId[quarterbacks$attempts==
                                       max(quarterbacks$attempts)]
    runningbacks <- rushrecblock |> filter(teamAbbr==posstm & 
                                             (position=="RB" | position=="FB"))
    runningbacks <- runningbacks |> filter(!is.na(attempts))
    runningbacks <- runningbacks |> filter(routes >= 10)
    rbprobs <- runningbacks$routes/plays
    onfieldRBS <- sample(runningbacks$nflId, mydf$RB, prob = rbprobs/sum(rbprobs))
    widereceivers <- rushrecblock |> filter(teamAbbr==posstm & position=="WR")
    widereceivers <- widereceivers |> filter(!is.na(routes))
    widereceivers <- widereceivers |> filter(routes >= 10)
    wrprobs <- widereceivers$routes/plays
    onfieldWRS <- sample(widereceivers$nflId, mydf$WR, prob = wrprobs/sum(wrprobs))
    tightends <- rushrecblock |> filter(teamAbbr==posstm & position=="TE")
    tightends <- tightends |> filter(!is.na(routes))
    tightends <- tightends |> filter(routes >= 10)
    teprobs <- tightends$routes/plays
    onfieldTES <- sample(tightends$nflId, mydf$TE, prob = teprobs/sum(teprobs))
    oline <- rushrecblock |> filter(teamAbbr==posstm & position=="OL")
    oline <- oline |> filter(!is.na(blocksnaps))
    oline <- oline |> filter(blocksnaps>=100)
    olineprobs <- oline$blocksnaps/plays
    onfieldOL <- sample(oline$nflId, 10-sum(mydf[1,]), prob = olineprobs/sum(olineprobs))
    
    defplaysdf <- offdf |> filter(defensiveTeam==deftm)
    defplays <- nrow(defplaysdf)
    
    if(def_personnel=="Nickel"){
      frontsevenamount <- 6
    } else if(def_personnel=="Heavy"){
      frontsevenamount <- 8
    } else if(def_personnel=="Goalline"){
      frontsevenamount <- 9
    } else if (def_personnel=="Dime"){
      frontsevenamount <- 5
    } else if (def_personnel=="Quarter"){
      frontsevenamount <- 4
    } else{
      frontsevenamount <- sum(as.numeric(str_split_fixed(def_personnel, "-", 2)))
    }
    # frontsevenamount <- case_when(
    #   def_personnel=="Nickel" ~ 6,
    #   def_personnel=="Heavy" ~ 8,
    #   def_personnel=="Goalline" ~ 9,
    #   def_personnel=="Dime" ~ 5,
    #   def_personnel=="Quarter" ~ 4,
    #   TRUE ~ sum(as.numeric(str_split_fixed(def_personnel, "-", 2)))
    # )
    secondarycount <- 11 - frontsevenamount
    if(coverplayers > secondarycount){
      frontsevencoverplayers <- coverplayers - secondarycount
      secondaryrushers <- 0 
    } else{
      frontsevencoverplayers <- 0
      secondaryrushers <- secondarycount - coverplayers
    }
    frontseven <- defense |> filter(teamAbbr==deftm & position=="F7")
    frontseven <- frontseven |> filter(!is.na(defensesnaps))
    frontseven <- frontseven |> filter(defensesnaps>=25)
    f7coverprobs <- frontseven$coveragesnaps/defplays
    if(frontsevencoverplayers>0){
      onfieldF7cover <- sample(frontseven$nflId, frontsevencoverplayers, 
                               prob = f7coverprobs/sum(f7coverprobs))
    } else{
      onfieldF7cover <- c()
    }
    # onfieldF7cover <- as.numeric(onfieldF7cover)
    f7passrushers <- frontseven |> filter(!nflId %in% onfieldF7cover)
    f7passrushprobs <- f7passrushers$passrushsnaps/defplays
    if((frontsevenamount - frontsevencoverplayers) > 0){
      onfieldF7passrushers <- sample(f7passrushers$nflId, 
                                     frontsevenamount - frontsevencoverplayers,
                                     prob = f7passrushprobs/sum(f7passrushprobs))
    } else{
      onfieldF7passrushers <- c()
    }
    
    secondary <- defense |> filter(teamAbbr==deftm & position=="DB")
    secondary <- secondary |> filter(!is.na(defensesnaps))
    secondary <- secondary |> filter(defensesnaps>=25)
    
    secondaryrushprobs <- secondary$passrushsnaps/defplays
    if(secondaryrushers>0){
      onfieldsecondaryrushers <- sample(secondary$nflId, secondaryrushers,
                                        prob = secondaryrushprobs/sum(secondaryrushprobs))
    } else{
      onfieldsecondaryrushers <- c()
    }
    secondarycoverplayers <- secondary |> filter(!nflId %in% onfieldsecondaryrushers)
    secondarycoverprobs <- secondarycoverplayers$coveragesnaps/defplays
    if((secondarycount-secondaryrushers) > 0){
      onfieldsecondarycover <- sample(secondarycoverplayers$nflId, 
                                      secondarycount - secondaryrushers, 
                                      prob = secondarycoverprobs/sum(secondarycoverprobs))
    } else{
      onfieldsecondarycover <- c()
    }
    allcoverplayers <- c(onfieldF7cover, onfieldsecondarycover)
    covertgtplyrcnt <- sample(c(0,1,2), 1, prob = c(.075, .85, .075))
    potentialtgtcovers <- defense |> filter(nflId %in% allcoverplayers)
    potentialtgtcovers <- potentialtgtcovers |> 
      filter(!is.na(avg_man_grades_coverage_defense & !is.na(avg_zone_grades_coverage_defense)))
    potentialtgtcovs <- potentialtgtcovers$nflId
    covertargetplayers <- sample(potentialtgtcovs, covertgtplyrcnt,
                                prob = rep(1/length(potentialtgtcovs), 
                                           length(potentialtgtcovs)))
    if(!is_empty(covertargetplayers)){
      noncovertgtplayers <- allcoverplayers[allcoverplayers!=covertargetplayers]
    } else{
      noncovertgtplayers <- allcoverplayers
    }
    ####
    
    oline_blocking <- rushrecblock |> filter(nflId %in% onfieldOL)
    oline_means <- oline_blocking$avg_grades_pass_block
    oline_sd <- oline_blocking$sd_grades_pass_block
    samples <- mapply(rnorm, n = 1, mean = oline_means, sd = oline_sd)
    final_blocking_grade <- sum(samples)/4.6
    
    targetdf <- data.frame(
      possessionTeam = posstm,
      QuarterSeconds = quarter_secs,
      quarter = quarter,
      PossTmDiff = posstmdiff,
      down = down,
      yardsToGo = togo,
      YdstoEZBef = YdsBef,
      simple_personnel = personnel,
      off_form = formation,
      ANGLE_count = anglect,
      CORNER_count = cornerct,
      CROSS_count = crossct,
      FLAT_count = flatct,
      GO_count = goct,
      HITCH_count = hitchct,
      IN_count = inct,
      OUT_count = outct,
      POST_count = postct,
      SCREEN_count = screenct,
      SLANT_count = slantct,
      WHEEL_count = wheelct,
      defensiveTeam = deftm,
      simple_def_personnel = def_personnel,
      def_simple_coverage = coverage,
      oline_brocking_grade_mean = final_blocking_grade,
      passrushers = passrushers
    )
    ttt_pred_dist <- posterior_predict(timeToThrowmodel, newdata = targetdf)
    timetoThrow <- round(apply(ttt_pred_dist, 2, sample, size = 1),3)
    targetdf$timeToThrow <- timetoThrow

    tgtrouteprobs <- data.frame(t(predict(targetted_route_model, targetdf, type = "probs")))
    chosenroutelist <- toupper(chosenroutelist)
    trimmedroutes <- unique(chosenroutelist)
    tgtprobs <- c()
    for(route in trimmedroutes){
      colkey <- which(colnames(tgtrouteprobs)==route)
      prob <- as.numeric(tgtrouteprobs[colkey])
      tgtprobs <- append(tgtprobs, prob)
    }
    tgtprobs1 <- tgtprobs/sum(tgtprobs)
    targetted_route <- sample(trimmedroutes, 1, prob = tgtprobs1)
  
    
    routeposdat <- data.frame(
      possessionTeam = posstm,
      QuarterSeconds = quarter_secs,
      quarter = quarter,
      PossTmDiff = posstmdiff,
      down = down,
      yardsToGo = togo,
      YdstoEZBef = YdsBef,
      simple_personnel = personnel,
      off_form = formation,
      ANGLE_count = anglect,
      CORNER_count = cornerct,
      CROSS_count = crossct,
      FLAT_count = flatct,
      GO_count = goct,
      HITCH_count = hitchct,
      IN_count = inct,
      OUT_count = outct,
      POST_count = postct,
      SCREEN_count = screenct,
      SLANT_count = slantct,
      WHEEL_count = wheelct
    )
    
    personneldf <- data.frame(
      RB = as.numeric(substr(personnel, 1, 1)),
      WR = length(chosenroutelist) - as.numeric(substr(personnel, 1, 1)) -
        as.numeric(substr(personnel, 2, 2)),
      TE = as.numeric(substr(personnel, 2, 2))
    )
    
    poschosenroutelist <- sort(chosenroutelist)
    
    if(personneldf$RB!=0){
      rbroutedf <- posroutedf |> filter(position=="RB" & route %in% poschosenroutelist)
      RBproblist <- c()
      for(route in poschosenroutelist){
        RBprob <- rbroutedf$prob[rbroutedf$route==route]
        RBproblist <- append(RBproblist, RBprob)
      }
      RBroutes <- sample(poschosenroutelist, personneldf$RB, prob = RBproblist/sum(RBproblist))
      rbroutelist <- c()
      for(r in 1:personneldf$RB){
        RBindex <- match(RBroutes[r], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-RBindex]
        rbroutelist <- append(rbroutelist, paste0("RB: ", RBroutes[r]))
      }
    } else{
      rbroutelist <- c()
    }
    
    if(personneldf$TE!=0){
      teroutedf <- posroutedf |> filter(position=="TE" & route %in% poschosenroutelist)
      TEproblist <- c()
      for(route in poschosenroutelist){
        TEprob <- teroutedf$prob[teroutedf$route==route]
        TEproblist <- append(TEproblist, TEprob)
      }
      TEroutes <- sample(poschosenroutelist, personneldf$TE, prob = TEproblist/sum(TEproblist))
      teroutelist <- c()
      for(t in 1:personneldf$TE){
        TEindex <- match(TEroutes[t], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-TEindex]
        teroutelist <- append(teroutelist, paste0("TE: ", TEroutes[t]))
      }
    } else{
      teroutelist <- c()
    }
    
    if(personneldf$WR!=0){
      wrroutedf <- posroutedf |> filter(position=="WR" & route %in% poschosenroutelist)
      WRproblist <- c()
      for(route in poschosenroutelist){
        WRprob <- wrroutedf$prob[wrroutedf$route==route]
        WRproblist <- append(WRproblist, WRprob)
      }
      WRroutes <- sample(poschosenroutelist, personneldf$WR, prob = WRproblist/sum(WRproblist))
      wrroutelist <- c()
      for(w in 1:personneldf$WR){
        WRindex <- match(WRroutes[w], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-WRindex]
        wrroutelist <- append(wrroutelist, paste0("WR: ", WRroutes[w]))
      }
    } else{
      wrroutelist <- c()
    }
    
    finalvec <- c(rbroutelist, wrroutelist, teroutelist)
    
    tgtposoptions <- finalvec[grepl(targetted_route, finalvec)]
    tgtposselection <- sample(tgtposoptions, 1, 
                             prob = rep(1/length(tgtposoptions), length(tgtposoptions)))
    tgtpos <- str_split_fixed(tgtposselection, ":", 2)[,1]
    if(tgtpos=="RB"){
      newRBs <- runningbacks |> filter(nflId %in% onfieldRBS)
      tgtprobs <- newRBs$routes/plays
      tgtplayer <- sample(newRBs$nflId, 1, prob = tgtprobs/sum(tgtprobs))
    } else if(tgtpos=="WR"){
      newWRs <- widereceivers |> filter(nflId %in% onfieldWRS)
      tgtprobs <- newWRs$routes/plays
      tgtplayer <- sample(newWRs$nflId, 1, prob = tgtprobs/sum(tgtprobs))
    } else if(tgtpos=="TE"){
      newTEs <- tightends |> filter(nflId %in% onfieldTES)
      tgtprobs <- newTEs$routes/plays
      tgtplayer <- sample(newTEs$nflId, 1, prob = tgtprobs/sum(tgtprobs))
    } else{
      tgtplayer <- NA
    }
    
    otherroutes <- c(onfieldRBS, onfieldWRS, onfieldTES)
    otherroutes <- otherroutes[otherroutes!=tgtplayer]
    
    list(pers = personnel, form = formation, runorpass = "Pass",
         def_pers = def_personnel, coverage = coverage,
         routes = finalvec, tgt_route = targetted_route, 
         quarterback = onfieldQB, otherroutes = otherroutes,
         oline = onfieldOL,
         passrushers = c(onfieldF7passrushers, onfieldsecondaryrushers),
         coveringtgtplayer = covertargetplayers,
         othercoverplayers = noncovertgtplayers,
         tgt_pos = tgtpos, tgt_player = tgtplayer)
    
  } else{
    RBs <- as.numeric(substr(personnel, 1, 1))
    TEs <- as.numeric(substr(personnel, 2, 2))
    WRs <- 5 - RBs - TEs
    onfieldpositionlist <- c("QB")
    if(RBs!=0){
      onfieldpositionlist <- append(onfieldpositionlist, "RB")
    }
    if(WRs!=0){
      onfieldpositionlist <- append(onfieldpositionlist, "WR")
    }
    if(TEs!=0){
      onfieldpositionlist <- append(onfieldpositionlist, "TE")
    }
    lengthdf <- offdf |> filter(possessionTeam==posstm)
    plays <- nrow(lengthdf)
    quarterbacks <- passing |> filter(teamAbbr==posstm & position=="QB")
    onfieldQB <- quarterbacks$nflId[quarterbacks$attempts==
                                      max(quarterbacks$attempts)]
    runningbacks <- rushrecblock |> filter(teamAbbr==posstm & 
                                             (position=="RB" | position=="FB"))
    runningbacks <- runningbacks |> filter(attempts >= 10)
    rbprobs <- runningbacks$attempts/plays
    onfieldRBS <- sample(runningbacks$nflId, RBs, prob = rbprobs/sum(rbprobs))
    widereceivers <- rushrecblock |> filter(teamAbbr==posstm & position=="WR")
    widereceivers <- widereceivers |> filter(blocksnaps >= 25)
    wrprobs <- widereceivers$blocksnaps/plays
    onfieldWRS <- sample(widereceivers$nflId, WRs, prob = wrprobs/sum(wrprobs))
    tightends <- rushrecblock |> filter(teamAbbr==posstm & position=="TE")
    tightends <- tightends |> filter(blocksnaps >= 25)
    teprobs <- tightends$blocksnaps/plays
    onfieldTES <- sample(tightends$nflId, TEs, prob = teprobs/sum(teprobs))
    oline <- rushrecblock |> filter(teamAbbr==posstm & position=="OL")
    oline <- oline |> filter(blocksnaps>=50)
    olineprobs <- oline$blocksnaps/plays
    onfieldOL <- sample(oline$nflId, 5, prob = olineprobs/sum(olineprobs))
    
    tmrushposdf <- teamrushposdf |> filter(possessionTeam==posstm)
    tmrushposdf$prob[tmrushposdf$rushingposition=="RB"] <-
      tmrushposdf$prob[tmrushposdf$rushingposition=="RB"] +
      tmrushposdf$prob[tmrushposdf$rushingposition=="FB"]
    tmrushposdf <- tmrushposdf |> filter(rushingposition!="FB")
    tmrushposdf <- tmrushposdf |> filter(rushingposition %in% onfieldpositionlist)
    rushpos <- sample(tmrushposdf$rushingposition, 1, 
                      prob = tmrushposdf$prob)
    if(formation=="WILDCAT"){
      rushpos <- "RB"
    }
    if(rushpos=="QB" & is.na(quarterbacks$sd_grades_run[quarterbacks$nflId==onfieldQB])){
      rushpos <- "RB"
    }
    if(rushpos=="WR" & max(widereceivers$attempts)<10){
      rushpos <- "RB"
    }
    if(rushpos=="RB"){
      newRBs <- runningbacks |> filter(nflId %in% onfieldRBS)
      newRBs <- newRBs |> filter(attempts>=10)
      rushprobs <- newRBs$attempts/plays
      rushplayer <- sample(newRBs$nflId, 1, prob = rushprobs/sum(rushprobs))
    } else if(rushpos=="QB"){
      rushplayer <- onfieldQB
    } else if(rushpos=="WR"){
      newWRs <- widereceivers |> filter(nflId %in% onfieldWRS)
      rushprobs <- newWRs$attempts/plays
      rushplayer <- sample(as.character(newWRs$nflId), 1, prob = rushprobs/sum(rushprobs))
    } else{
      rushplayer <- NA
    }
    rest_of_offense <- c(onfieldQB, onfieldRBS, onfieldWRS, onfieldTES)
    rest_of_offense <- rest_of_offense[-which(rest_of_offense==rushplayer)]
    if(rushpos!="QB"){
      rest_of_offense <- rest_of_offense[-which(rest_of_offense==onfieldQB)]
    }
    
    deflengthdf <- offdf |> filter(defensiveTeam==deftm)
    deflength <- nrow(deflengthdf)
    frontsevenamount <- case_when(
      def_personnel=="Nickel" ~ 6,
      def_personnel=="Heavy" ~ 8,
      def_personnel=="Goalline" ~ 9,
      def_personnel=="Dime" ~ 5,
      def_personnel=="Quarter" ~ 4,
      TRUE ~ sum(as.numeric(str_split_fixed(def_personnel, "-", 2)))
    )
    f7df <- defense |> filter(teamAbbr==deftm & position=="F7")
    f7df <- f7df |> filter(rundefensesnaps>=25)
    f7probs <- f7df$rundefensesnaps/deflength
    f7players <- sample(f7df$nflId, frontsevenamount,
                        prob = f7probs/sum(f7probs))
    secondaryamount <- 11-frontsevenamount
    secondarydf <- defense |> filter(teamAbbr==deftm & position=="DB")
    secondarydf <-secondarydf |> filter(rundefensesnaps>=25)
    secondaryprobs <- secondarydf$rundefensesnaps/deflength
    secondaryplayers<- sample(secondarydf$nflId, secondaryamount,
                             prob = secondaryprobs/sum(secondaryprobs))
    
    list(pers = personnel, form = formation, runorpass = "Run",
         def_pers = def_personnel, coverage = coverage,
         rushpos = rushpos, rushplayer = rushplayer,
         quarterback = onfieldQB,
         rest_of_offense = rest_of_offense,
         oline = onfieldOL,
         defense = c(f7players, secondaryplayers))
  }
}

saferoutefun <- safely(routeselection)
saferouteselection <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter){
  out <- suppressWarnings(saferoutefun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter))
  while(!is.null(out$error) | any(is.na(out$result))){
    out <- suppressWarnings(saferoutefun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter))
  }
  out$result
}
suppressWarnings(saferouteselection("DEN", "SEA", 3, 10, 70, 0, 600, 1))
# replicate(1000, suppressWarnings(saferouteselection("DEN", "SEA", 3, 10, 70, 0, 600, 1)),
          # simplify = TRUE)


# results <- vector("list", 1000)
# i <- 1
# while (i <= 1000) {
#   out <- suppressWarnings(safe_fun("DEN", "SEA", 3, 10, 70, 0, 600, 1))
#   
#   if (is.null(out$error)) {
#     results[[i]] <- out$result
#     i <- i + 1
#     message(i, ": Success")
#   } else {
#     message("Error on trial ", i, ", retrying...")
#   }
# }


yardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter){
  # output <- suppressWarnings(saferouteselection(posstm, deftm, down, togo, 
  #                                           YdsBef, posstmdiff, quarter_secs, quarter))
  # while(any(is.na(output))) {
  #   output <- suppressWarnings(saferouteselection(posstm, deftm, down, togo, 
  #                                                 YdsBef, posstmdiff, quarter_secs, quarter))
  # }
  
  # max_attempts <- 10
  # attempts <- 0
  # repeat {
  #   output <- suppressWarnings(saferouteselection(posstm, deftm, down, togo, 
  #                                                 YdsBef, posstmdiff, quarter_secs, quarter))
  #   attempts <- attempts + 1
  #   if(any(is.na(output))){
  #     print(paste0("Potential Yards Error: ", attempts))
  #     # print(game_state)
  #     # print(play)
  #   }
  #   if(all(!is.na(output)) || attempts >= max_attempts) break
  # }
  # if(attempts >= max_attempts) {
  #   print("Using Play Default")
  #   play <- list(pers = "11", form = "SINGLEBACK", runorpass = "Run", 
  #                def_pers = "Nickel", coverage = "Cover_1", rushpos = "RB",
  #                rushplayer = "40129", quarterback = "38605",
  #                rest_of_offense = c("52423", "46109", "52660", "54545"),
  #                oline = c("41369", "47824", "53527", "44832", "43384"),
  #                defense = c("46117", "47871", "41464", "46487", "52435",
  #                            "38545", "44873", "42543", "46711", "47941", "54618"))
  # }
  max_attempts <- 10
  attempts <- 0
  timeout_seconds <- 20  # Set your desired timeout (e.g., 5 seconds)
  
  repeat {
    output <- tryCatch({
      # Set time limit for this operation
      setTimeLimit(elapsed = timeout_seconds, transient = TRUE)
      result <- suppressWarnings(saferouteselection(posstm, deftm, down, togo, 
                                                    YdsBef, posstmdiff, quarter_secs, quarter))
      # Reset time limit
      setTimeLimit(elapsed = Inf, transient = TRUE)
      result
    }, error = function(e) {
      if(grepl("reached elapsed time limit", e$message)) {
        message(paste0("Attempt ", attempts, " timed out after ", timeout_seconds, " seconds"))
      } else {
        message(paste0("Error in saferouteselection(): ", e$message))
      }
      NULL  # Return NULL on timeout/error
    })
    
    attempts <- attempts + 1
    
    if(is.null(output)) {
      # Timeout occurred, try again
      next
    }
    
    if(any(is.na(output))) {
      print(paste0("Potential Yards Error: ", attempts))
    }
    
    if(all(!is.na(output)) || attempts >= max_attempts) break
  }
  
  if(attempts >= max_attempts || is.null(output)) {
    print("Using Play Default (timeout or max attempts reached)")
    play <- list(pers = "11", form = "SINGLEBACK", runorpass = "Run", 
                 def_pers = "Nickel", coverage = "Cover_1", rushpos = "RB",
                 rushplayer = "40129", quarterback = "38605",
                 rest_of_offense = c("52423", "46109", "52660", "54545"),
                 oline = c("41369", "47824", "53527", "44832", "43384"),
                 defense = c("46117", "47871", "41464", "46487", "52435",
                             "38545", "44873", "42543", "46711", "47941", "54618"))
  }
  if(output$runorpass=="Pass"){
    if(output$coverage %in% c("Cover_1", "RedZone", "GoalLine", "Cover_0",
                              "Cover2Man", "Bracket")){
      manzone <- "Man"
    } else{manzone <- "Zone"}
    tgtroute <- output$tgt_route
    qbdf <- passing |> filter(nflId==output$quarterback)
    olinedf <- rushrecblock |> filter(nflId %in% output$oline)
    olineblockgrade <- mean(mapply(rnorm, n=1, mean = olinedf$avg_grades_pass_block, sd = olinedf$sd_grades_pass_block))
    passrushdf <- defense |> filter(nflId %in% output$passrushers)
    passrushgrade <- mean(mapply(rnorm, n=1, mean = passrushdf$avg_grades_pass_rush_defense, sd = passrushdf$sd_grades_pass_rush_defense))
    pffpassrush <- olineblockgrade - passrushgrade
    dummydf <- data.frame(
      down = down,
      simple_personnel = output$pers,
      simple_def_personnel = output$def_pers,
      def_simple_coverage = output$coverage,
      off_form = output$form,
      YdstoEZBef = YdsBef,
      yardsToGo = togo,
      possessionTeam = posstm,
      defensiveTeam = deftm,
      runorpass = output$runorpass,
      targettedroute = tgtroute,
      targettedposition = output$tgt_pos,
      oline_brocking_grade_mean = olineblockgrade,
      passrushers = length(output$passrushers),
      pffpassrush = pffpassrush
    )
    pressureplayers <- sample(c(0:4), 1,
                              prob = predict(pressureplayersmodel, newdata = dummydf, type = "probs"))
    dummydf$pressureplayers <- pressureplayers
    sackprob <- predict(sack_model, dummydf, type = "response")
    sack <- sample(c("Yes", "No"), 1, prob = c(sackprob, 1-sackprob))
    if(sack=="Yes"){
      random_yards <- round(apply(posterior_predict(sack_yards_model, newdata = dummydf), 2, sample, size = 1),0)
      list(runpass = "Pass", result = "Sack", yards = random_yards)
    } else{
      ttt <- round(apply(posterior_predict(timeToThrowmodel, newdata = dummydf), 2, sample, size = 1),3)
      pressure_component <- if_else(pressureplayers>=1, "pressure", "no_pressure")
      ttt_component <- if_else(ttt<2.5, "less", "more")
      if(tgtroute %in% c("ANGLE", "FLAT", "IN", "OUT", "SLANT")){
        distance_component <- "short"
      } else if(tgtroute %in% c("CORNER", "CROSS", "HITCH")){
        distance_component <- "medium"
      } else if(tgtroute %in% c("GO", "POST", "WHEEL")){
        distance_component <- "deep"
      } else{
        distance_component <- "behind_los"
      }
      screen_component <- if_else(tgtroute=="SCREEN", "screen", "no_screen")
      pressurepart <- rnorm(1, mean = as.numeric(qbdf[which(colnames(qbdf)==paste0("avg_", pressure_component, "_grades_pass"))]),
                            sd = as.numeric(qbdf[which(colnames(qbdf)==paste0("sd_", pressure_component, "_grades_pass"))]))
      tttpart <- rnorm(1, mean = as.numeric(qbdf[which(colnames(qbdf)==paste0("avg_", ttt_component, "_grades_pass"))]),
                       sd = as.numeric(qbdf[which(colnames(qbdf)==paste0("sd_", ttt_component, "_grades_pass"))]))
      distancepart <- rnorm(1, mean = as.numeric(qbdf[which(colnames(qbdf)==paste0("avg_", distance_component, "_grades_pass"))]),
                            sd = as.numeric(qbdf[which(colnames(qbdf)==paste0("sd_", distance_component, "_grades_pass"))]))
      screenpart <- rnorm(1, mean = as.numeric(qbdf[which(colnames(qbdf)==paste0("avg_", screen_component, "_grades_pass"))]),
                          sd = as.numeric(qbdf[which(colnames(qbdf)==paste0("sd_", screen_component, "_grades_pass"))]))
      dummydf$pass_grade_mean <- mean(c(pressurepart, tttpart, distancepart, screenpart))
      
      tgtrecdf <- (rushrecblock |> filter(nflId==output$tgt_player))[1,]
      tgtrecdistance_routepart <- rnorm(1, mean = as.numeric(tgtrecdf[which(colnames(tgtrecdf)==paste0("avg_grades_pass_route"))]),
                                        sd = as.numeric(tgtrecdf[which(colnames(tgtrecdf)==paste0("sd_grades_pass_route"))]))
      tgtrecdistance_handspart <- rnorm(1, mean = as.numeric(tgtrecdf[which(colnames(tgtrecdf)==paste0("avg_grades_hands_drop"))]),
                                        sd = as.numeric(tgtrecdf[which(colnames(tgtrecdf)==paste0("sd_grades_hands_drop"))]))
      recgrademean <- (tgtrecdistance_handspart + tgtrecdistance_routepart)/2
      tgtcoverdf <- defense |> filter(nflId %in% output$coveringtgtplayer)
      if(nrow(tgtcoverdf)!=0){
        if(manzone=="Man"){
          meancol <- which(colnames(tgtcoverdf)==paste0("avg_man_grades_coverage_defense"))
          sdcol <- which(colnames(tgtcoverdf)==paste0("sd_man_grades_coverage_defense"))
          tgtcover <- mapply(rnorm, 1, mean = tgtcoverdf[meancol], sd = tgtcoverdf[sdcol])
        } else{
          meancol <- which(colnames(tgtcoverdf)==paste0("avg_zone_grades_coverage_defense"))
          sdcol <- which(colnames(tgtcoverdf)==paste0("sd_zone_grades_coverage_defense"))
          tgtcover <- mapply(rnorm, 1, mean = tgtcoverdf[meancol], sd = tgtcoverdf[sdcol])
        }
      } else{
        tgtcover <- 20
      }
      dummydf$coveragefactor <- recgrademean - tgtcover
      passresult <- sample(c("Complete", "Fumble", "Incomplete", "Interception"), 1,
                           prob = predict(passresultmodel, dummydf, type = "probs"))
      if(passresult=="Complete"){
        pass_yards <- round(apply(posterior_predict(passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        list(runpass = "Pass", result = "Complete", yards = pass_yards)
      } else if(passresult=="Interception"){
        pass_yards <- round(apply(posterior_predict(passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        int_yards <- round(apply(posterior_predict(int_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        list(runpass = "Pass", result = "Interception", yards = pass_yards - int_yards)
      } else if(passresult=="Incomplete"){
        list(runpass = "Pass", result = "Incomplete", yards = 0)
      } else{
        fumblepos <- sample(c("QB", output$tgt_pos), 1, prob = c(.6, .4))
        if(fumblepos=="QB"){
          fumbleprob <- fumblostdf$lostprob[fumblostdf$fumbleposition=="QB"]
          fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
          if(fumblost=="Yes"){
            fumbresult <- "Sack_Fumble_Retained"
          } else{
            fumbresult <- "Sack_Fumble_Lost"
          }
          fumbleyards <- -5
        } else{
          fumbleprob <- fumblostdf$lostprob[fumblostdf$fumbleposition==output$tgt_pos]
          fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
          if(fumblost=="Yes"){
            fumbresult <- "Complete_Fumble_Retained"
          } else{
            fumbresult <- "Complete_Fumble_Lost"
          }
          fumbleyards <- round(apply(posterior_predict(passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        }
        list(runpass = "Pass", result = fumbresult, yards = fumbleyards)
      }
    }
  } else{
    rushteamdf <- teamrushposdf |> filter(possessionTeam==posstm)
    rushteamdf$prob[rushteamdf$rushingposition=="RB"] <- 
      rushteamdf$prob[rushteamdf$rushingposition=="RB"] + 
      rushteamdf$prob[rushteamdf$rushingposition=="FB"]
    rushteamdf <- rushteamdf[-which(rushteamdf$rushingposition=='FB'),]
    rushingpos <- output$rushpos
    olinedf <- rushrecblock |> filter(nflId %in% output$oline)
    olineblockgrade <- mean(mapply(rnorm, n=1, mean = olinedf$avg_grades_run_block, sd = olinedf$sd_grades_run_block))
    dummydf <- data.frame(
      down = down,
      simple_personnel = output$pers,
      simple_def_personnel = output$def_pers,
      def_simple_coverage = output$coverage,
      off_form = output$form,
      YdstoEZBef = YdsBef,
      yardsToGo = togo,
      possessionTeam = posstm,
      defensiveTeam = deftm,
      runorpass = output$runorpass,
      oline_brocking_grade_mean = olineblockgrade,
      rushingposition = rushingpos
    )
    oline <- rushrecblock |> filter(nflId %in% output$oline)
    olinegrade <- mean(mapply(rnorm, 1, mean = na.omit(oline$avg_grades_run_block),
                              sd = na.omit(oline$sd_grades_run_block)))
    runningplayerdf <- rushrecblock |> filter(nflId==output$rushplayer)
    runninggrade <- rnorm(1, mean = runningplayerdf$avg_grades_run,
                          sd = runningplayerdf$sd_grades_run)
    othersdf <- rushrecblock |> filter(nflId %in% output$rest_of_offense)
    othersgrade <- mean(mapply(rnorm, 1, mean = na.omit(othersdf$avg_grades_run_block),
                               sd = na.omit(othersdf$sd_grades_run_block)))
    totaloffensegrade <- (4*olinegrade + 3*runninggrade + 2*othersgrade)/9
    defensedf <- defense |> filter(nflId %in% output$defense)
    rundefgrade <- mean(mapply(rnorm, 1, mean = na.omit(defensedf$avg_grades_run_defense),
                               sd = na.omit(defensedf$sd_grades_run_defense)))
    tacklegrade <- mean(mapply(rnorm, 1, mean = na.omit(defensedf$avg_grades_tackle),
                               sd = na.omit(defensedf$sd_grades_tackle)))
    totaldefensegrade <- (rundefgrade + tacklegrade)/2
    dummydf$pff_player_grade_component <- totaloffensegrade - totaldefensegrade
    rush_yards <- round(apply(posterior_predict(rushing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
    fumbleprob <- predict(rushfumble_model, newdata = dummydf, type = "response")
    fumble <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
    if(fumble=="No"){
      result <- "No_Fumble"
    } else{
      fldf <- fumblostdf |> filter(fumbleposition==rushingpos)
      fumblelost <- sample(c("Yes", "No"), 1, prob = c(fldf$lostprob, 1-fldf$lostprob))
      if(fumblelost=="No"){
        result <- "Fumble_Retained"
      } else{
        result <- "Fumble_Lost"
      }
    }
    list(runpass = "Run", result = result, yards = rush_yards)
  }
}

safeydsfun <- safely(yardsgained)
safeyardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter){
  out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter))
  while(!is.null(out$error) | is.null(out$result) | any(is.na(out$result))){
    out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter))
  }
  out$result
}
data.frame(safeyardsgained("DEN", "SEA", 1, 10, 70, 0, 600, 1))
# replicate(1000, suppressWarnings(safeyardsgained("DEN", "SEA", 1, 10, 70, 0, 600, 1)),
#     simplify = TRUE)

# tmlist <- c("SEA", "DEN", "DAL", "TB", "NYG", "TEN", "MIN", "GB", "LAC", "LV", "KC",
#              "ARI", "JAX", "WAS", "NYJ", "BAL", "NE", "MIA", "IND", "HOU",
#              "DET", "PHI", "PIT", "CIN", "CHI", "SF", "CAR", "CLE", "ATL", "NO",
#              "BUF", "LA")
# i <- 1
# # dflist <- list()
# while(i<=10001){
#   teams <- sample(tmlist, 2, prob = rep(1/32, 32))
#   offtm <- teams[1]
#   deftm <- teams[2]
#   down <- sample(c(1:4), 1, prob = rep(.25, 4))
#   ydsbef <- sample(c(1:99), 1, prob = rep(1/99, 99))
#   if(down==1 & ydsbef>=10){
#     togo <- 10
#   } else{
#     vec <- c(1:ydsbef)
#     togo <- sample(vec, 1, prob = rep(1/length(vec), length(vec)))
#   }
#   if(togo > 40){
#     togo <- 40
#   }
#   posstmmargin <- sample(c(-40:40), 1, prob = rep(1/81, 81))
#   secondsleft <- sample(c(1:900), 1, prob = rep(1/900, 900))
#   quarter <- sample(c(1:4), 1, prob = rep(.25, 4))
#   df <- data.frame(safeyardsgained(offtm, deftm, down, togo, ydsbef, posstmmargin, 
#                                    secondsleft, quarter))
#   finaldf <- df |> 
#     mutate(posstm = offtm,
#            deftm = deftm,
#            down = down,
#            togo = togo,
#            ydsbef = ydsbef,
#            posstmmargin = posstmmargin,
#            secondsleft = secondsleft,
#            quarter = quarter)
#   dflist <- list.append(dflist, finaldf)
#   print(i)
#   i <- i + 1
# }
# basefile <- dflist[[1]]
# for(j in 2:length(dflist)){
#   basefile <- rbind(basefile, dflist[[j]])
# }
# 
# check <- basefile |> filter(result=="No_Fumble")
# 
# write_csv(basefile, "Ydsfunctioncheck.csv")




simulator <- function(team1, team2) {
  play <- list(yards = 0, runpass = "Run", result = "Default")
  # Initialize all game state variables
  game_state <- list(
    quarter = 1,
    secondsleft = 3600,
    quartersecondsleft = 900,
    down = 1,
    togo = 10,
    ydsbef = 70,
    teams = c(team1, team2),
    scoredf = data.frame(x1 = 0, x2 = 0),
    posstm = NULL,
    deftm = NULL,
    posstmmargin = 0,
    playtime = NULL,
    afterplaytime = NULL,
    option = NULL
  )
  
  colnames(game_state$scoredf) <- c(team1, team2)
  game_state$posstm <- sample(game_state$teams, 1, prob = c(.5, .5))
  game_state$deftm <- game_state$teams[which(c(team1, team2) != game_state$posstm)]
  game_state$startoffteam <- game_state$posstm
  game_state$startdefteam <- game_state$deftm
  
  # Helper functions (all take and return game_state)
  timeupdater <- function(game_state, playtime, afterplaytime) {
    game_state$secondsleft <- round(game_state$secondsleft - playtime - afterplaytime)
    game_state$quartersecondsleft <- round(game_state$quartersecondsleft - playtime - afterplaytime)
    return(game_state)
  }
  
  playtimecorrector <- function(game_state) {
    if(is.null(game_state$playtime) || game_state$playtime < 3) {
      game_state$playtime <- 3
    }
    return(game_state)
  }
  
  togocorrector <- function(game_state) {
    if(game_state$togo > game_state$ydsbef) {
      game_state$togo <- game_state$ydsbef
    }
    return(game_state)
  }
  
  posschange <- function(game_state) {
    temp <- game_state$posstm
    game_state$posstm <- game_state$deftm
    game_state$deftm <- temp
    return(game_state)
  }
  
  touchdown <- function(game_state) {
    if(game_state$posstmmargin %in% c(-1, -5, -8, -11, -15)) {
      mypoints <- sample(c(6,8), 1, prob = c(.49, .51))
    } else {
      mypoints <- sample(c(6,7), 1, prob = c(.025, .975))
    }
    return(mypoints)
  }
  
  afterplaytimegenerator <- function(game_state) {
    if(game_state$posstmmargin < 0 & game_state$secondsleft <= 240) {
      game_state$afterplaytime <- round(rnorm(1, 10, 1))
    } else {
      game_state$afterplaytime <- round(rnorm(1, 35, 3.5))
    }
    return(game_state)
  }
  
  posstmmargin_updater <- function(game_state) {
    game_state$posstmmargin <- as.numeric(game_state$scoredf[[game_state$posstm]] - 
                                            game_state$scoredf[[game_state$deftm]])
    return(game_state)
  }
  
  quartercheck <- function(game_state) {
    if(game_state$quartersecondsleft <= 0) {
      game_state$quartersecondsleft <- 900
      game_state$secondsleft <- 3600 - (game_state$quarter * 900)
      game_state$quarter <- game_state$quarter + 1
      if(game_state$quarter == 3) {
        game_state$posstm <- game_state$startdefteam
        game_state$deftm <- game_state$startoffteam
        game_state$down <- 1
        game_state$togo <- 10
        game_state$ydsbef <- 70
        game_state <- posstmmargin_updater(game_state)
      }
    }
    return(game_state)
  }
  
  deftd <- function(game_state, playtimemean, playtimesd) {
    game_state$scoredf[[game_state$deftm]] <- 
      as.numeric(game_state$scoredf[[game_state$deftm]]) + touchdown(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  touchback <- function(game_state, playtimemean, playtimesd) {
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 80
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  reg_turnover <- function(game_state, playtimemean, playtimesd){
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 100 - as.numeric(game_state$ydsbef - play$yards)
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  safety <- function(game_state, playtimemean, playtimesd){
    game_state$scoredf[[game_state$deftm]] <- 
      as.numeric(game_state$scoredf[[game_state$deftm]]) + 2
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 65
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  offtd <- function(game_state, playtimemean, playtimesd){
    game_state$scoredf[[game_state$posstm]] <- 
      as.numeric(game_state$scoredf[[game_state$posstm]]) + touchdown(game_state)
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  firstdown <- function(game_state, playtimemean, playtimesd){
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- game_state$ydsbef - play$yards
    game_state <- togocorrector(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state <- afterplaytimegenerator(game_state)
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  turnover_on_downs <- function(game_state, playtimemean, playtimesd){
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 100 - (game_state$ydsbef - play$yards)
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  regular_gain <- function(game_state, playtimemean, playtimesd){
    game_state$down <- game_state$down + 1
    game_state$togo <- game_state$togo - play$yards
    game_state$ydsbef <- game_state$ydsbef - play$yards
    game_state <- togocorrector(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state <- afterplaytimegenerator(game_state)
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  made_fg <- function(game_state){
    game_state$option <- "field goal attempt"
    game_state$scoredf[[game_state$posstm]] <- 
      as.numeric(game_state$scoredf[[game_state$posstm]]) + 3
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 5, 1))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  missed_fg <- function(game_state){
    game_state$option <- "field goal attempt"
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- (100-game_state$ydsbef) - 5
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 5, 1))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  punt <- function(game_state){
    game_state$option <- "punt"
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- (100-game_state$ydsbef) + round(rnorm(1, 45, 5))
    ### TOUCHBACK
    if(game_state$ydsbef>=100){
      game_state$ydsbef <- 80
    }
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 9, 1.5))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }
  
  game_state <- posstmmargin_updater(game_state)
  
  # Main game loop
  while(game_state$secondsleft > 0) {
    game_state$scoredf$Quarter <- game_state$quarter
    game_state$scoredf$SecondsLeft <- game_state$quartersecondsleft
    game_state$scoredf$Possession <- game_state$posstm
    game_state$scoredf$Down <- game_state$down
    game_state$scoredf$ToGo <- game_state$togo
    game_state$scoredf$YdstoEZ <- game_state$ydsbef
    
    
    if(game_state$down==4){
      if(game_state$ydsbef>40){
        ### GO FOR IT ON FOURTH
        if(game_state$quartersecondsleft<=240 & game_state$posstmmargin<0 
           & game_state$togo<=10){
          game_state$option <- "goforit"
        } else{ ### PUNT
          game_state <- punt(game_state)
        }
      } else{
        ### GO FOR IT ON FOURTH
        if((game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=240 & game_state$
            posstmmargin<0 & game_state$togo<=3) |
           (game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=120 & 
            game_state$posstmmargin<0)){
          game_state$option <- "goforit"
        } else{ ### FIELD GOAL
          game_state$option <- "field goal attempt"
          fgmakedf <- fgdf |> group_by(FGDist) |> 
            summarise(make_prob = mean(FGMakeProb))
          fgpredvardf <- data.frame(
            FGDist = game_state$ydsbef + 17,
            Quarter = game_state$quarter,
            Time2 = game_state$quartersecondsleft,
            PossTmMargin = game_state$posstmmargin,
            Down = game_state$down,
            ToGo = game_state$togo
          )
          fgattprob <- predict(fgmodf, newdata = fgpredvardf, type="response")
          fgattprob <- sqrt(sqrt(fgattprob))
          fgattselection <- sample(c("Yes", "No"), 1, prob = c(fgattprob, 1-fgattprob))
          ### FGATT
          if(fgattselection=="Yes"){
            game_state$option <- "field goal attempt"
            fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
            fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
            ### MADE FIELD GOAL
            if(fgmake=="Yes"){
              game_state <- made_fg(game_state)
              ### MISSED FIELD GOAL
            } else{
              game_state <- missed_fg(game_state)
            }
            ### GO FOR IT ON FOURTH
          } else{
            game_state$option <- "goforit"
          }
        }
      }
    } else{
      game_state$option <- "regular"
    }
    ### OTHER field goal scenario
    if((game_state$secondsleft<=10 & game_state$posstmmargin>=-3 & 
        game_state$posstmmargin<=0) |
       (game_state$quarter==2 & game_state$quartersecondsleft<=10)){
      game_state$option <- "field goal attempt"
      fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
      fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
      ### MADE FIELD GOAL
      if(fgmake=="Yes"){
        game_state <- made_fg(game_state)
        ### MISSED FIELD GOAL
      } else{
        game_state <- missed_fg(game_state)
      }
    }
    
    
    ### REGULAR PLAYS
    
    # if((game_state$down!=4 | game_state$option=="goforit") & 
    #    game_state$option!="field goal attempt"){
    #   max_attempts <- 10
    #   attempts <- 0
    #   repeat {
    #     play <- safeyardsgained(game_state$posstm, game_state$deftm, game_state$down,
    #                             game_state$togo, game_state$ydsbef, game_state$posstmmargin,
    #                             game_state$quartersecondsleft, game_state$quarter)
    #     attempts <- attempts + 1
    #     if(is.na(play$yards)){
    #       print(paste0("Potential Error: ", attempts))
    #       # print(game_state)
    #       # print(play)
    #     }
    #     if(!is.na(play$yards) || attempts >= max_attempts) break
    #   }
    #   if(attempts >= max_attempts) {
    #     print("Using Default")
    #     play <- list(runpass = "Run", result = "No_Fumble", yards = 0)
    #   }
    # }
    if((game_state$down!=4 | game_state$option=="goforit") & 
       game_state$option!="field goal attempt"){
      max_attempts <- 10
      attempts <- 0
      play <- NULL  # Initialize play as NULL
      
      while (attempts < max_attempts) {
        # Set up timeout handling
        play <- tryCatch({
          setTimeLimit(elapsed = 30, transient = TRUE)  # 30-second timeout
          result <- safeyardsgained(game_state$posstm, game_state$deftm, game_state$down,
                                    game_state$togo, game_state$ydsbef, game_state$posstmmargin,
                                    game_state$quartersecondsleft, game_state$quarter)
          setTimeLimit(elapsed = Inf, transient = TRUE)  # Reset timeout
          result
        }, error = function(e) {
          if(grepl("reached elapsed time limit", e$message)) {
            warning("safeyardsgained() timed out on attempt ", attempts + 1)
          } else {
            warning("Error in safeyardsgained() on attempt ", attempts + 1, ": ", e$message)
          }
          NULL
        })
        
        attempts <- attempts + 1
        
        # Check if we got a valid play
        if (!is.null(play) && 
            is.list(play) && 
            all(c("runpass", "result", "yards") %in% names(play)) && 
            !is.na(play$yards)) {
          break
        }
        
        # Small delay between attempts to avoid tight loops
        if (attempts < max_attempts) {
          Sys.sleep(0.1)  # 100ms delay
        }
      }
      
      # Fallback to default play if all attempts failed
      if (is.null(play) || 
          !is.list(play) || 
          !all(c("runpass", "result", "yards") %in% names(play)) || 
          is.na(play$yards)) {
        warning("Failed to generate valid play after ", max_attempts, " attempts - using default")
        play <- list(runpass = "Run", result = "No_Fumble", yards = 0)
      }
    }
    if(play$result=="Sack" & play$yards > 0){
      play$yards <- -5
    }
    if(play$yards < -100){
      play$yards <- -100
    }
    if(play$yards > 100){
      play$yards <- 100
    }
    newyardsbef <- game_state$ydsbef - play$yards
    ################ PASS
    if(play$runpass=="Pass"){
      # INTERCEPTION
      if(play$result=="Interception"){
        ### PICK SIX
        if(newyardsbef>=100){
          game_state <- deftd(game_state, 8.5, .85)
          ### INTERCEPTION TOUCHBACK
        } else if(newyardsbef <= 0){
          game_state <- touchback(game_state, 5.5, .55)
          ### REGULAR INTERCEPTION
        } else{
          game_state <- reg_turnover(game_state, 6.5, .65)
        } 
        # FUMBLE
      } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained",
                                   "Sack_Fumble_Lost", "Complete_Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result %in% c("Sack_Fumble_Lost", "Complete_Fumble_Lost")){
          ### DEF TD
          if(newyardsbef>=100){
            game_state <- deftd(game_state, 5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            game_state <- touchback(game_state, 5, .5)
            ### REGULAR TURNOVER
          } else{
            game_state <- reg_turnover(game_state, 5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained")){
          ### SAFETY
          if(newyardsbef>=100){
            game_state <- safety(game_state, 5.5, .5)
            ### OFF TD
          } else if(newyardsbef<=0){
            game_state <- offtd(game_state, 7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
            game_state <- firstdown(game_state, 7.5, .75)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo) 
                    & game_state$down==4){
            game_state <- turnover_on_downs(game_state, 6, .6)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo) 
                    & game_state$down!=4){
            game_state <- regular_gain(game_state, 6, .6)
          } else{
            print("FUMBLE RETAINED SCENARIO NOT CAPTURED")
          }
        }
        # SACK
      } else if(play$result=="Sack"){
        ### SAFETY
        if(newyardsbef >= 100){
          game_state <- safety(game_state, 4.5, .45)
          ### TURNOVER ON DOWNS
        } else if(game_state$down==4 & (newyardsbef < 100)){
          game_state <- turnover_on_downs(game_state, 4.5, .45)
          ### REGULAR SACK
        } else if(game_state$down!=4 & (newyardsbef < 100)){
          game_state <- regular_gain(game_state, 4.5, .45)
        } else{
          print("SACK SCENARIO NOT CAPTURED")
        }
        # COMPLETE PASSES
      } else if(play$result=="Complete"){
        ### SAFETY
        if(newyardsbef>=100){
          game_state <- safety(game_state, 5, .5)
          ### OFF TOUCHDOWN
        } else if(newyardsbef<=0){
          game_state <- offtd(game_state, 7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
          game_state <- firstdown(game_state, 7.5, .75)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo) 
                  & game_state$down==4){
          game_state <- turnover_on_downs(game_state, 6.5, .65)
          ### REGULAR NON FOURTH DOWN NO FIRST DOWN NO TOUCHDOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo) 
                  & game_state$down!=4){
          game_state <- regular_gain(game_state, 6.5, .65)
        } else{
          print("COMPLETE PASSES SCENARIO NOT CAPTURED")
        }
        # INCOMPLETE PASSES
      } else if(play$result=="Incomplete"){
        ### TURNOVER ON DOWNS
        if(game_state$down==4){
          game_state <- turnover_on_downs(game_state, 6, .6)
          ### REGULAR INCOMPLETION
        } else if(game_state$down!=4){
          game_state$down <- game_state$down + 1
          game_state$playtime <- round(rnorm(1, 6, .6))
          game_state <- playtimecorrector(game_state)
          game_state$afterplaytime <- 0
          game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
          game_state <- quartercheck(game_state)
        } else{
          print("INCOMPLETE PASSES SCENARIO NOT CAPTURED")
        }
      } else{
        print("PASS SCENARIO NOT CAPTURED")
      }
      ################ RUN
    } else if(play$runpass=="Run"){
      # FUMBLE
      if(play$result %in% c("Fumble_Retained", "Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result=="Fumble_Lost"){
          ### DEF TD
          if(newyardsbef>=100){
            game_state <- deftd(game_state, 5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            game_state <- touchback(game_state, 5, .5)
            ### REGULAR TURNOVER
          } else{
            game_state <- reg_turnover(game_state, 5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result=="Fumble_Retained"){
          ### SAFETY
          if(newyardsbef>=100){
            game_state <- safety(game_state, 4.5, .45)
            ### OFF TD
          } else if(newyardsbef<=0){
            game_state <- offtd(game_state, 7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
            game_state <- firstdown(game_state, 6.5, .65)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo) 
                    & game_state$down==4){
            game_state <- turnover_on_downs(game_state, 4.5, .45)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo) 
                    & game_state$down!=4){
            game_state <- regular_gain(game_state, 5, .5)
          } else{
            print("RUN FUMBLE RETAINED SCENARIO NOT CAPTURED")
          }
        }
        # NON FUMBLE RUNS
      } else if(play$result=="No_Fumble"){
        ### SAFETY
        if(newyardsbef>=100){
          game_state <- safety(game_state, 4.5, .45)
          ### OFF TD
        } else if(newyardsbef<=0){
          game_state <- offtd(game_state, 7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
          game_state <- firstdown(game_state, 6, .6)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down==4){
          game_state <- turnover_on_downs(game_state, 4.5, .45)
          ### NON FIRST DOWN REGULAR PLAY
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo) 
                  & game_state$down!=4){
          game_state <- regular_gain(game_state, 5, .5)
        } else{
          print("NON FUMBLE RUN SCENARIO NOT CAPTURED")
        }
      }
      else{
        print("RUN SCENARIO NOT CAPTURED")
      }
    }
    else{
      print("RUNPASS IS NOT A RUN OR PASS")
    }
    game_state$scoredf$Detail <- paste0(play$result, "; Yards: ", play$yards)
    game_state$scoredf <- game_state$scoredf |> relocate((Quarter:YdstoEZ), .before = JAX)
    print(game_state$scoredf)
  }
  return(game_state$scoredf)
}
# count <- 10
simulator("JAX", "KC")
# count <- count + 1


# multiple_simulations <- function(n, team1, team2){
#   resultlist <- vector("list", n) # Pre-allocate
#   for(i in 1:n){
#     start <- Sys.time()
#     print(paste0(i, " Start Time: ", start))
#     res <- try(simulator(team1, team2))
#     if(inherits(res, "try-error")) {
#       message(sprintf("Simulation %d failed at %s", i, Sys.time()))
#       next
#     }
#     end <- Sys.time()
#     print(paste0(i, " End Time: ", end))
# 
#     resultlist[[i]] <- res
#     elapsed <- difftime(Sys.time(), start, units = "secs")
#   }
#   basefile <- resultlist[[1]]
#   for(j in 2:length(resultlist)){
#     basefile <- rbind(basefile, resultlist[[j]])
#   }
#   newfile <- data.frame(
#     samplesize = n,
#     team1 = team1,
#     team1wins = sum(basefile[[team1]] > basefile[[team2]]),
#     team1mean = mean(as.numeric(basefile[[team1]])),
#     team1sd = sd(as.numeric(basefile[[team1]])),
#     team2 = team2,
#     team2wins = sum(basefile[[team2]] > basefile[[team1]]),
#     team2mean = mean(as.numeric(basefile[[team2]])),
#     team2sd = sd(as.numeric(basefile[[team2]]))
#   )
#   newfile
# }


multiple_simulations <- function(n, team1, team2, max_attempts = 3) {
  resultlist <- vector("list", n)
  successful_runs <- 0
  attempts <- 0
  
  while(successful_runs < n && attempts < n * max_attempts) {
    attempts <- attempts + 1
    start <- Sys.time()
    message(paste0("Attempt ", attempts, " Start Time: ", start))
    
    # Initialize res as NULL before the try block
    res <- NULL
    timed_out <- FALSE
    
    # Try with time limit
    try_result <- try({
      setTimeLimit(elapsed = 240, transient = TRUE)  # 4 minutes = 240 seconds
      res <- simulator(team1, team2)
      setTimeLimit(elapsed = Inf, transient = TRUE)  # Reset time limit
    }, silent = TRUE)
    
    if(inherits(res, "try-error")) {
      # Check if the error was due to a timeout
      if(grepl("reached elapsed time limit", try_result[1])) {
        timed_out <- TRUE
        message("Simulation timed out after 4 minutes - retrying...")
      } else {
        message(sprintf("Simulation failed with error: %s", try_result[1]))
      }
      next
    }
    
    successful_runs <- successful_runs + 1
    resultlist[[successful_runs]] <- res
    message(paste0("Completed ", successful_runs, "/", n, " in ", 
                   difftime(Sys.time(), start, units = "secs"), " secs"))
  }
  
  if(successful_runs == 0) {
    warning("All simulations failed")
    return(NULL)
  }
  
  # Combine only successful runs
  combined_results <- do.call(rbind, resultlist[1:successful_runs])
  
  data.frame(
    samplesize = successful_runs,
    team1 = team1,
    team1wins = sum(combined_results[[team1]] > combined_results[[team2]]),
    team1mean = mean(as.numeric(combined_results[[team1]])),
    team1sd = sd(as.numeric(combined_results[[team1]])),
    team2 = team2,
    team2wins = sum(combined_results[[team2]] > combined_results[[team1]]),
    team2mean = mean(as.numeric(combined_results[[team2]])),
    team2sd = sd(as.numeric(combined_results[[team2]]))
  )
}

# xtrial <- multiple_simulations(100, "SEA", "TB")


### LOADING IN GAME DATA
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
games2022 <- read_csv("Games_for_Simulator.csv")
###

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Simulated Games")
# finalfilelist <- list()
all_teams_loop_simulator <- function(){
  team1vec <- c(games2022$Team1)
  team2vec <- c(games2022$Team2)
  for(b in 1:length(team1vec)){
    team1 <- team1vec[b]
    team2 <- team2vec[b]
    starttime <- Sys.time()
    print(paste0(team1, " vs. ", team2))
    mysim <- multiple_simulations(100, team1, team2)
    # finalfilelist <<- list.append(finalfilelist, mysim)
    write_csv(mysim, paste0(team1, " vs. ", team2, ".csv"))
    endtime <- Sys.time()
    print(paste0(team1, " vs. ", team2, " timeelapsed: ", endtime - starttime))
    Sys.sleep(10)
  }
}

all_teams_loop_simulator()

for(file in finalfilelist){
  write_csv(file, paste0(file$team1, " vs. ", file$team2, ".csv"))
}





 #######




























# Special Teams -------------------------------------------------------------

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
fgdf <- readRDS("FirstDownLogRegResults.rds")

forfgatt <- fgdf |> select(FGAtt, FGDist, Quarter, Time2, PossTmMargin, Down, ToGo)
forfgatt$FGAtt <- as.factor(forfgatt$FGAtt)

fgmodf <- glm(FGAtt ~ FGDist + Quarter + Time2 + PossTmMargin + Down + ToGo, 
              data = forfgatt, family = "binomial")
m <- tidy(fgmodf)

forfgatt$FGAttProb <- predict(fgmodf, forfgatt, type = "response")

fg_att_func <- function(fgdist, quarter, secsleft, margin, down, togo){
  fg_att_prob <- 1 / (1 + exp(-(m$estimate[1] + m$estimate[2]*fgdist + 
                                  m$estimate[3]*quarter + m$estimate[4]*secsleft +
                                  m$estimate[5]*margin + m$estimate[6]*down + 
                                  m$estimate[7]*togo)))
  fg_att_prob
}

punt <- function(){
  round(rnorm(1, 42.5, 5))
}


# Penalties ---------------------------------------------------------------
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
pendf <- read_csv("Penalties.csv")

penalty_func <- function(runpass){
  if(runpass=="Pass"){
    offoptions <- c("Yes", "No")
    offprobs <- pendf$Probability[pendf$OffDef=="Offense"]
    finoffprob <- sum(offprobs)
    offresult <- sample(offoptions, 1, prob = c(finoffprob, (1-finoffprob)))
    if(offresult=="Yes"){
      igprob <- pendf$Probability[pendf$Penalty=="Intentional_Grounding"] / finoffprob
      fiveydprob <- pendf$Probability[pendf$Penalty=="OffFiveYdPenalty"] / finoffprob
      tenydprob <- pendf$Probability[pendf$Penalty=="OffTenYdPenalty"] / finoffprob
      offunrough <- pendf$Probability[pendf$Penalty=="Offensive_Unnecessary_Roughness"] / finoffprob
      offpenoptions <- c("Intentional_Grounding", "OffFiveYdPenalty",
                         "OffTenYdPenalty", "Offensive_Unnecessary_Roughness")
      offpenprobs <- c(igprob, fiveydprob, tenydprob, offunrough)
      offresult <- sample(offpenoptions, 1, prob = offpenprobs)
    }
    defoptions <- c("Yes", "No")
    defprobs <- pendf$Probability[pendf$OffDef=="Defense"]
    findefprob <- sum(defprobs)
    defresult <- sample(defoptions, 1, prob = c(findefprob, (1-findefprob)))
    if(defresult=="Yes"){
      fiveydprob <- pendf$Probability[pendf$Penalty=="DefFiveYdPenalty"] / findefprob
      defhold <- pendf$Probability[pendf$Penalty=="Defensive_Holding"] / findefprob
      defpi <- pendf$Probability[pendf$Penalty=="Defensive_Pass_Interference"] / findefprob
      defunrough <- pendf$Probability[pendf$Penalty=="Defensive_Unnecessary_Roughness"] / findefprob
      defpenoptions <- c("DefFiveYdPenalty", "Defensive_Holding",
                         "Defensive_Pass_Interference", "Defensive_Unnecessary_Roughness")
      defpenprobs <- c(fiveydprob, defhold, defpi, defunrough)
      defresult <- sample(defpenoptions, 1, prob = defpenprobs)
    }
  }
  if(runpass=="Run"){
    offoptions <- c("Yes", "No")
    offprobs <- pendf$Probability[pendf$OffDef=="Offense" & 
                                    pendf$Penalty!="Intentional_Grounding"]
    finoffprob <- sum(offprobs)
    offresult <- sample(offoptions, 1, prob = c(finoffprob, (1-finoffprob)))
    if(offresult=="Yes"){
      fiveydprob <- pendf$Probability[pendf$Penalty=="OffFiveYdPenalty"] / finoffprob
      tenydprob <- pendf$Probability[pendf$Penalty=="OffTenYdPenalty"] / finoffprob
      offunrough <- pendf$Probability[pendf$Penalty=="Offensive_Unnecessary_Roughness"] / finoffprob
      offpenoptions <- c("OffFiveYdPenalty",
                         "OffTenYdPenalty", "Offensive_Unnecessary_Roughness")
      offpenprobs <- c(fiveydprob, tenydprob, offunrough)
      offresult <- sample(offpenoptions, 1, prob = offpenprobs)
    }
    defoptions <- c("Yes", "No")
    defprobs <- pendf$Probability[pendf$OffDef=="Defense" & 
                                    pendf$Penalty!="Defensive_Pass_Interference"]
    findefprob <- sum(defprobs)
    defresult <- sample(defoptions, 1, prob = c(findefprob, (1-findefprob)))
    if(defresult=="Yes"){
      fiveydprob <- pendf$Probability[pendf$Penalty=="DefFiveYdPenalty"] / findefprob
      defhold <- pendf$Probability[pendf$Penalty=="Defensive_Holding"] / findefprob
      defunrough <- pendf$Probability[pendf$Penalty=="Defensive_Unnecessary_Roughness"] / findefprob
      defpenoptions <- c("DefFiveYdPenalty", "Defensive_Holding",
                        "Defensive_Unnecessary_Roughness")
      defpenprobs <- c(fiveydprob, defhold, defunrough)
      defresult <- sample(defpenoptions, 1, prob = defpenprobs)
    }
  }
  finaldf <- tibble("Off_Pen" = offresult, "Def_Pen" = defresult)
  finaldf
}


# Game Simulator ----------------------------------------------------------

gamesimulator <- function(tm1, tm2){
  possession <- sample(c(tm1, tm2), size = 1)
  quarter <- 1
  time <- 900
  ydstoEZ <- 70
}









# Archives ----------------------------------------------------------------

# Creating Penalty Dataframe ----------------------------------------------

setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
pendf <- readRDS("FirstDownLogRegResults.rds")

penalties <- pendf |> filter(grepl("enalty", Detail))
penalties <- penalties |> select(Detail)

grounding <- penalties |> filter(grepl("Intentional Grounding", Detail))
probgrounding <- nrow(grounding) / nrow(pendf)

offside <- penalties |> filter(grepl("Neutral Zone", Detail) | grepl("Defensive Offside", Detail))
proboffside <- nrow(offside) / nrow(pendf)

falsestart <- penalties |> filter(grepl("Illegal Shift", Detail) | 
                                    grepl("Illegal Motion", Detail) |
                                    grepl("Illegal Formation", Detail) |
                                    grepl("False Start", Detail))
probfalsestart <- nrow(falsestart) / nrow(pendf)

offten <- penalties |> filter(grepl("Offensive Holding", Detail) |
                                grepl("Offensive Pass Interfere", Detail))
probofften <- nrow(offten) / nrow(pendf)

defhold <- penalties |> filter(grepl("Defensive Holding", Detail))
probdefhold <- nrow(defhold) / nrow(pendf)

defpi <- penalties |> filter(grepl("Defensive Pass Interfere", Detail))
probdefpi <- nrow(defpi) / nrow(pendf)

unnecrough <- penalties |> filter(grepl("Unnecessary Roughness", Detail))
probunnecrough <- nrow(unnecrough) / nrow(pendf)

proboffunnecrough <- .4 * probunnecrough
probdefunnecrough <- .6 * probunnecrough

dummydf <- tibble(Penalty = rep(NA, 8), OffDef = rep(NA, 8), Yards = rep(NA, 8),
                  Probability = rep(NA, 8))

dummydf[1,] <- list("Intentional_Grounding", "Offense", 10, probgrounding)
dummydf[2,] <- list("OffFiveYdPenalty", "Offense", 5, probfalsestart)
dummydf[3,] <- list("OffTenYdPenalty", "Offense", 10, probofften)
dummydf[4,] <- list("Offensive_Unnecessary_Roughness", "Offense", 15, proboffunnecrough)
dummydf[5,] <- list("DefFiveYdPenalty", "Defense", 5, proboffside)
dummydf[6,] <- list("Defensive_Holding", "Defense", 5, probdefhold)
### 0 is going to be an rnorm for yards later
dummydf[7,] <- list("Defensive_Pass_Interference", "Defense", 0, probdefpi)
dummydf[8,] <- list("Defensive_Unnecessary_Roughness", "Defense", 15, probdefunnecrough)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
write_csv(dummydf, "Penalties.csv")



#### OLD YARDS GAINED FUNCTION

# yardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter){
#   output <- suppressWarnings(routeselection(posstm, deftm, down, togo, 
#                                             YdsBef, posstmdiff, quarter_secs, quarter))
#   if(output$runorpass=="Pass"){
#     qbdf <- passing |> filter(nflId==output$quarterback)
#     tgtroute <- output$tgt_route
#     if(tgtroute %in% c("ANGLE", "FLAT", "IN", "OUT", "SLANT")){
#       throw_dist <- "short"
#     } else if(tgtroute %in% c("CORNER", "CROSS", "HITCH")){
#       throw_dist <- "medium"
#     } else if(tgtroute %in% c("GO", "POST", "WHEEL")){
#       throw_dist <- "deep"
#     } else{
#       throw_dist <- "behind_los"
#     }
#     
#     dummydf <- data.frame(
#       down = down,
#       simple_personnel = output$pers,
#       off_form = output$form,
#       YdstoEZBef = YdsBef,
#       yardsToGo = togo,
#       possessionTeam = posstm,
#       runorpass = output$runorpass,
#       targettedroute = tgtroute,
#       targettedposition = output$tgt_pos,
#       runorpass = "Pass"
#     )
#     sack_prob <- as.numeric(predict(sack_model, newdata = dummydf, type = "response"))
#     int_prob <- as.numeric(predict(interception_model, newdata = dummydf, type = "response"))
#     pass_result_options <- c("Complete", "Incomplete", "Sack", "Interception")
#     pass_result_probs <- c(final_comp_prob, 1 - final_comp_prob, sack_prob, int_prob)
#     pass_result_probs <- pass_result_probs/sum(pass_result_probs)
#     pass_result <- sample(pass_result_options, 1, prob = pass_result_probs)
#     if(pass_result=="Incomplete"){
#       list(runorpass = output$runorpass, pass_res = "Incomplete", 
#            yards_gained = 0)
#     } else if (pass_result=="Complete"){
#       ### Setting up pff player grade component
#       qbdf <- passing |> filter(nflId==output$quarterback)
#       ###
#       pred_dist <- posterior_predict(passing_yds_model, newdata = dummydf)
#       pred_df <- as.data.frame(pred_dist) %>%
#         pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")
#       pass_yards <- round(apply(pred_dist, 2, sample, size = 1),0)
#       fumb_prob <- as.numeric(predict(fumble_model, newdata = dummydf, type = "response"))
#       fumb_options <- c("Yes", "No")
#       fumbprobs <- c(fumb_prob, 1 - fumb_prob)
#       fumb_res <- sample(fumb_options, 1, prob = fumbprobs)
#       if(fumb_res == "Yes"){
#         fumb_lost_df <- fumblostdf |> filter(fumbleposition==mypos)
#         flprob <- as.numeric(fumb_lost_df$lostprob)
#         fl_options <- c("Yes", "No")
#         fl_probs <- c(flprob, 1 - flprob)
#         fl_res <- sample(fl_options, 1, prob = fl_probs)
#         fumblestring <- paste0("Fumble: ", fumb_res, "; Lost: ", fl_res)
#       } else{
#         fumblestring <- paste0("Fumble: ", fumb_res, "; Lost: NA")
#       }
#       list(runorpass = output$runorpass, pass_res = "Complete", 
#            yards_gained = pass_yards, fumble = fumblestring)
#     } else if (pass_result=="Sack"){
#       pred_dist <- posterior_predict(sack_yards_model, newdata = dummydf)
#       pred_df <- as.data.frame(pred_dist) %>%
#         pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")
#       sack_yards <- round(apply(pred_dist, 2, sample, size = 1),0)
#       fumb_prob <- as.numeric(predict(fumble_model, newdata = dummydf, type = "response"))
#       fumb_options <- c("Yes", "No")
#       fumbprobs <- c(fumb_prob, 1 - fumb_prob)
#       fumb_res <- sample(fumb_options, 1, prob = fumbprobs)
#       if(fumb_res == "Yes"){
#         fumb_lost_df <- fumblostdf |> filter(fumbleposition=="QB")
#         flprob <- as.numeric(fumb_lost_df$lostprob)
#         fl_options <- c("Yes", "No")
#         fl_probs <- c(flprob, 1 - flprob)
#         fl_res <- sample(fl_options, 1, prob = fl_probs)
#         fumblestring <- paste0("Fumble: ", fumb_res, "; Lost: ", fl_res)
#       } else{
#         fumblestring <- paste0("Fumble: ", fumb_res, "; Lost: NA")
#       }
#       list(runorpass = output$runorpass, pass_res = "Sack", sack_yards = sack_yards, 
#            fumble = fumblestring)
#     } else{
#       pred_dist <- posterior_predict(int_yds_model, newdata = dummydf)
#       pred_df <- as.data.frame(pred_dist) %>%
#         pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")
#       int_yards <- round(apply(pred_dist, 2, sample, size = 1),0)
#       list(runorpass = output$runorpass, pass_res = "Int", int_return_yds = int_yards)
#     }
#   } else{
#     teamrushdf <- teamrushposdf |> filter(possessionTeam==posstm)
#     genericrushdf <- genericrushposdf |> filter(rushingposition %in% teamrushdf$rushingposition)
#     teamrushprobs <- teamrushdf$prob
#     genericrushprobs <- genericrushdf$prob/sum(genericrushdf$prob)
#     totalrushprobs <- (teamrushprobs + genericrushprobs) / 2
#     rush_options <- teamrushdf$rushingposition
#     my_rush_pos <- sample(rush_options, 1, prob = totalrushprobs)
#     dummydf <- data.frame(
#       down = down,
#       simple_personnel = output$pers,
#       off_form = output$form,
#       YdstoEZBef = YdsBef,
#       yardsToGo = togo,
#       possessionTeam = posstm,
#       rushingposition = my_rush_pos,
#       runorpass = "Run"
#     )
#     pred_dist <- posterior_predict(rushing_yds_model, newdata = dummydf)
#     pred_df <- as.data.frame(pred_dist) %>%
#       pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")
#     rush_yards <- round(apply(pred_dist, 2, sample, size = 1),0)
#     fumb_prob <- as.numeric(predict(fumble_model, newdata = dummydf, type = "response"))
#     fumb_options <- c("Yes", "No")
#     fumbprobs <- c(fumb_prob, 1 - fumb_prob)
#     fumb_res <- sample(fumb_options, 1, prob = fumbprobs)
#     if(fumb_res == "Yes"){
#       fumb_lost_df <- fumblostdf |> filter(fumbleposition==my_rush_pos)
#       flprob <- as.numeric(fumb_lost_df$lostprob)
#       fl_options <- c("Yes", "No")
#       fl_probs <- c(flprob, 1 - flprob)
#       fl_res <- sample(fl_options, 1, prob = fl_probs)
#       fumblestring <- paste0("Fumble: ", fumb_res, "; Lost: ", fl_res)
#     } else{
#       fumblestring <- paste0("Fumble: ", fumb_res, "; Lost: NA")
#     }
#     list(runorpass = output$runorpass, rushyards = rush_yards, fumble = fumblestring)
#   }
# }


## OLD SIMULATORS
simulator <- function(team1, team2){
  quarter <- 1
  secondsleft <- 3600
  quartersecondsleft <- 900
  down <- 1
  togo <- 10
  ydsbef <- 70
  teams <- c(team1, team2)
  scoredf <- data.frame(x1 = 0, x2 = 0)
  colnames(scoredf) <- c(team1, team2)
  startoffteam <- sample(teams, 1, prob = c(.5, .5))
  startdefteam <- teams[which(c(team1, team2)!=startoffteam)]
  posstm <- startoffteam
  deftm <- startdefteam
  
  timeupdater <- function(){
    secondsleft <<- secondsleft - playtime - afterplaytime
    quartersecondsleft <<- quartersecondsleft - playtime - afterplaytime
  }
  playtimecorrector <- function(){
    if(playtime < 3){
      playtime <<- 3
    }
  }
  togocorrector <- function(){
    if(togo > ydsbef){
      togo <<- ydsbef
    }
  }
  posschange <- function(){
    dummytm <<- posstm
    posstm <<- deftm
    deftm <<- dummytm
  }
  touchdown <- function(){
    if(posstmmargin %in% c(5, 8, 11, 15)){
      mypoints <<- sample(c(6,8), 1, prob = c(.49, .51))
    } else{
      mypoints <<- sample(c(6,7), 1, prob = c(.025, .975))
    }
    mypoints
  }
  afterplaytimegenerator <- function(){
    if(posstmmargin<0 & secondsleft<=240){
      afterplaytime <<- round(rnorm(1, 9, 1))
    } else{
      afterplaytime <<- round(rnorm(1, 25, 3.5))
    }
  }
  posstmmargin_updater <- function(){
    posstmmargin <<- as.numeric(scoredf[[posstm]] - scoredf[[deftm]])
  }
  quartercheck <- function(){
    if(quartersecondsleft<=0){
      quarter <<- quarter + 1
      quartersecondsleft <<- 900 - (((5-quarter)*900) - secondsleft)
      ### HALFTIME SWITCH
      if(quarter==3){
        posstm <<- startdefteam
        deftm <<- startoffteam
        down <<- 1
        togo <<- 10
        ydsbef <<- 70
        posstmmargin_updater()
      }
    } else{
      quarter <<- quarter
    }
  }
  posstmmargin_updater()
  while(secondsleft > 0){
    ### SPECIAL TEAMS & Fourth down going for it
    if(down==4){
      if(ydsbef>40){
        ### GO FOR IT ON FOURTH
        if(quartersecondsleft<=240 & posstmmargin<0 & togo<=10){
          option <- "goforit"
        } else{ ### PUNT
          option <- "punt"
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- (100-ydsbef) + round(rnorm(1, 42.5, 5))
          ### TOUCHBACK
          if(ydsbef>=100){
            ydsbef <- 80
          }
          togocorrector()
          playtime <- round(rnorm(1, 9, 1.5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
        }
      } else{
        ### GO FOR IT ON FOURTH
        if((quarter %in% c(2,4) & quartersecondsleft<=240 & posstmmargin<0 & togo<=3) |
           (quarter %in% c(2,4) & quartersecondsleft<=120 & posstmmargin<0)){
          option <- "goforit"
        } else{ ### FIELD GOAL
          option <- "field goal attempt"
          setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
          fgdf <- readRDS("FirstDownLogRegResults.rds")
          fgdf$FGAtt <- as.factor(fgdf$FGAtt)
          fgmodf <- glm(FGAtt ~ FGDist + Quarter + Time2 + PossTmMargin + Down + ToGo, 
                        data = fgdf, family = "binomial")
          fgmakedf <- fgdf |> group_by(FGDist) |> 
            summarise(make_prob = mean(FGMakeProb))
          fgpredvardf <- data.frame(
            FGDist = ydsbef + 17,
            Quarter = quarter,
            Time2 = quartersecondsleft,
            PossTmMargin = posstmmargin,
            Down = down,
            ToGo = togo
          )
          fgattprob <- predict(fgmodf, newdata = fgpredvardf, type="response")
          fgattprob <- sqrt(sqrt(fgattprob))
          fgattselection <- sample(c("Yes", "No"), 1, prob = c(fgattprob, 1-fgattprob))
          ### FGATT
          if(fgattselection=="Yes"){
            option <- "field goal attempt"
            fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==ydsbef+17]
            fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
            ### MADE FIELD GOAL
            if(fgmake=="Yes"){
              scoredf[[posstm]] <- as.numeric(scoredf[[posstm]]) + 3
              down <- 1
              togo <- 10
              ydsbef <- 70
              togocorrector()
              posschange()
              posstmmargin_updater()
              playtime <- round(rnorm(1, 5, 1))
              playtimecorrector()
              afterplaytime <- 0
              timeupdater()
              quartercheck()
              ### MISSED FIELD GOAL
            } else{
              down <- 1
              togo <- 10
              ydsbef <- (100-ydsbef) - 5
              togocorrector()
              posschange()
              posstmmargin_updater()
              playtime <- round(rnorm(1, 5, 1))
              playtimecorrector()
              afterplaytime <- 0
              timeupdater()
              quartercheck()
            }
            ### GO FOR IT ON FOURTH
          } else{
            option <- "goforit"
          }
        }
      }
    } else{
      option <- "regular"
    }
    ### OTHER field goal scenario
    if((secondsleft<=10 & posstmmargin>=-3 & posstmmargin<=0) |
       (quarter==2 & quartersecondsleft<=10)){
      option <- "field goal attempt"
      fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==ydsbef+17]
      fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
      ### MADE FIELD GOAL
      if(fgmake=="Yes"){
        scoredf[[posstm]] <- as.numeric(scoredf[[posstm]]) + 3
        down <- 1
        togo <- 10
        ydsbef <- 70
        togocorrector()
        posschange()
        posstmmargin_updater()
        playtime <- round(rnorm(1, 5, 1))
        playtimecorrector()
        afterplaytime <- 0
        timeupdater()
        quartercheck()
        ### MISSED FIELD GOAL
      } else{
        down <- 1
        togo <- 10
        ydsbef <- (100-ydsbef) - 5
        togocorrector()
        posschange()
        posstmmargin_updater()
        playtime <- round(rnorm(1, 5, 1))
        playtimecorrector()
        afterplaytime <- 0
        timeupdater()
        quartercheck()
      }
    }
    
    ### REGULAR PLAYS
    if((down!=4 | option=="goforit") & option!="field goal attempt"){
      play <- safeyardsgained(posstm, deftm, down, togo, ydsbef, posstmmargin, 
                              quartersecondsleft, quarter)
      while(is.na(play$yards)){
        play <- safeyardsgained(posstm, deftm, down, togo, ydsbef, posstmmargin, 
                                quartersecondsleft, quarter)
      }
    }
    
    if(play$runpass=="Pass"){
      # INTERCEPTION
      if(play$result=="Interception"){
        ydsbef <- (100-ydsbef) + play$yards
        ### PICK SIX
        if(ydsbef<=0){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + touchdown()
          down <- 1
          togo <- 10
          ydsbef <- 70
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 8.5, 1.5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### INTERCEPTION TOUCHBACK
        } else if(ydsbef >= 100){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 80
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 5.5, 1))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
        } else{
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- ydsbef
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 8.5, 1.5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
        } 
        # FUMBLE
      } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained",
                                   "Sack_Fumble_Lost", "Complete_Fumble_Lost")){
        ### SAFETY
        if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained") &
           (ydsbef - play$yards)>=100){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + 2
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 65
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 5.5, .5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### DEF TD
        } else if(play$result %in% c("Sack_Fumble_Lost", "Complete_Fumble_Lost") &
                  (ydsbef - play$yards)>=100){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + touchdown()
          down <- 1
          togo <- 10
          ydsbef <- 70
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 5.5, .5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### 4TH DOWN
        } else if(down==4 & play$result %in% 
                  c("Sack_Fumble_Retained", "Complete_Fumble_Retained")){
          ### TURNOVER ON DOWNS
          if(play$yards < togo){
            posschange()
            down <- 1
            togo <- 10
            ydsbef <- 100 - (ydsbef - play$yards)
            togocorrector()
            posstmmargin_updater()
            playtime <- round(rnorm(1, 5.5, .5))
            playtimecorrector()
            afterplaytime <- 0
            timeupdater()
            quartercheck()
            ### FIRST DOWN ON 4TH DOWN
          } else{
            ### TOUCHDOWN
            if(play$yards >= ydsbef){
              scoredf[[posstm]] <- as.numeric(scoredf[[posstm]]) + touchdown()
              posschange
              down <- 1
              togo <- 10
              ydsbef <- 70
              togocorrector()
              posstmmargin_updater()
              playtime <- round(rnorm(1, 7.5, .75))
              playtimecorrector()
              afterplaytime <- 0
              timeupdater()
              quartercheck()
              ### NO TOUCHDOWN FIRSTDOWN ON FOURTH DOWN
            } else{
              down <- 1
              togo <- 10
              ydsbef <- ydsbef - play$yards
              togocorrector()
              playtime <- round(rnorm(1, 7.5, .75))
              playtimecorrector()
              afterplaytimegenerator()
              timeupdater()
              quartercheck()
            }
          }
        } else if(down!=4 & play$result %in% 
                  c("Sack_Fumble_Retained", "Complete_Fumble_Retained")){
          ### TOUCHDOWN
          if(play$yards >= ydsbef){
            scoredf[[posstm]] <- as.numeric(scoredf[[posstm]]) + touchdown()
            posschange()
            down <- 1
            togo <- 10
            ydsbef <- 70
            togocorrector()
            posstmmargin_updater()
            playtime <- round(rnorm(1, 7.5, .75))
            playtimecorrector()
            afterplaytime <- 0
            timeupdater()
            quartercheck()
          } else{
            ### FIRST DOWN
            if(play$yards >= togo){
              down <- 1
              togo <- 10
              ydsbef <- ydsbef - play$yards
              togocorrector()
              playtime <- round(rnorm(1, 7.5, .75))
              playtimecorrector()
              afterplaytimegenerator()
              timeupdater()
              quartercheck()
              ### NO FIRST DOWN
            } else{
              down <- down + 1
              togo <- togo - play$yards
              ydsbef <- ydsbef - play$yards
              togocorrector()
              playtime <- round(rnorm(1, 7.5, .75))
              playtimecorrector()
              afterplaytimegenerator()
              timeupdater()
              quartercheck()
            }
          }
        } else if(play$result %in% c("Sack_Fumble_Lost", "Complete_Fumble_Lost") &
                  (ydsbef - play$yards)<100){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 100 - (ydsbef-play$yards)
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 7.5, .75))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
        }
        # SACK
      } else if(play$result=="Sack"){
        ### SAFETY
        if((ydsbef - play$yards) >= 100){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + 2
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 65
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### TURNOVER ON DOWNS
        } else if(down==4 & (ydsbef - play$yards) < 100){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 100 - (ydsbef - play$yards)
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### REGULAR SACK
        } else if(down!=4 & (ydsbef - play$yards) < 100){
          down <- down + 1
          togo <- togo - play$yards
          ydsbef <- ydsbef - play$yards
          togocorrector()
          playtime <- round(rnorm(1, 4.5, .5))
          playtimecorrector()
          afterplaytimegenerator()
          timeupdater()
          quartercheck()
        }
        # COMPLETE PASSES
      } else if(play$result=="Complete"){
        ### SAFETY
        if((ydsbef - play$yards)>=100){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + 2
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 65
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 5.5, .5))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### TURNOVER ON DOWNS
        } else if(down==4 & (ydsbef - play$yards) < 100 & (play$yards < togo)){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 100 - (ydsbef - play$yards)
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 6.5, .65))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### TOUCHDOWN
        } else if(play$yards > ydsbef){
          scoredf[[posstm]] <- as.numeric(scoredf[[posstm]]) + touchdown()
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 70
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 7.5, .75))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### FIRST DOWN CONVERSION (NO TOUCHDOWN)
        } else if((ydsbef - play$yards) < 100 & (play$yards >= togo) &
                  (play$yards < ydsbef)){
          down <- 1
          togo <- 10
          ydsbef <- ydsbef - play$yards
          togocorrector()
          playtime <- round(rnorm(1, 7.5, .75))
          playtimecorrector()
          afterplaytimegenerator()
          timeupdater()
          quartercheck()
          ### REGULAR NON FOURTH DOWN NO FIRST DOWN NO TOUCHDOWN
        } else if(down!=4 & (ydsbef - play$yards) < 100 & (play$yards < togo) &
                  (play$yards < ydsbef)){
          down <- down + 1
          togo <- togo - play$yards
          ydsbef <- ydsbef - play$yards
          togocorrector()
          playtime <- round(rnorm(1, 7.5, .75))
          playtimecorrector()
          afterplaytimegenerator()
          timeupdater()
          quartercheck()
        }
      } else if(play$result=="Incomplete"){
        ### TURNOVER ON DOWNS
        if(down==4){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 100 - ydsbef
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 6.5, .65))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### REGULAR INCOMPLETION
        } else{
          down <- down + 1
          playtime <- round(rnorm(1, 6.5, .65))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
        }
      }
    } else if(play$runpass=="Run"){
      if(play$result %in% c("Fumble_Retained", "Fumble_Lost")){
        ### SAFETY
        if(play$result=="Fumble_Retained" & (ydsbef - play$yards) >= 100){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + 2
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 65
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### DEF TD
        } else if(play$result=="Fumble_Lost" & (ydsbef - play$yards) >= 100){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + touchdown()
          down <- 1
          togo <- 10
          ydsbef <- 70
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### REGULAR FUMBLE LOST
        } else if(play$result=="Fumble_Lost" & (ydsbef - play$yards) < 100){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 100 - (ydsbef - play$yards)
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### TOUCHDOWN
        } else if(play$result=="Fumble_Retained" & play$yards>ydsbef){
          scoredf[[posstm]] <- as.numeric(scoredf[[posstm]]) + touchdown()
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 70
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### TURNOVER ON DOWNS
        } else if(play$result=="Fumble_Retained" & down==4 & play$yards<togo){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 100 - (ydsbef - play$yards)
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### FIRST DOWN NO TOUCHDOWN
        } else if(play$result=="Fumble_Retained" & play$yards<ydsbef & play$yards>=togo){
          down <- 1
          togo <- 10
          ydsbef <- ydsbef - play$yards
          togocorrector()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytimegenerator()
          timeupdater()
          quartercheck()
          ### NO FIRST DOWN
        } else if(play$result=="Fumble_Retained" & down!=4 & play$yards<togo){
          down <- down + 1
          togo <- togo - play$yards
          ydsbef <- ydsbef - play$yards
          togocorrector()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytimegenerator()
          timeupdater()
          quartercheck()
        }
        # NON FUMBLE RUNS
      } else{
        ### SAFETY
        if((ydsbef - play$yards)>=100){
          scoredf[[deftm]] <- as.numeric(scoredf[[deftm]]) + 2
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 65
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### TOUCHDOWN
        } else if(play$yards >= ydsbef){
          scoredf[[posstm]] <- as.numeric(scoredf[[posstm]]) + touchdown()
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 70
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### FIRST DOWN
        } else if((play$yards >= togo) & (play$yards < ydsbef)){
          down <- 1
          togo <- 10
          ydsbef <- ydsbef - play$yards
          togocorrector()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytimegenerator()
          timeupdater()
          quartercheck()
          ### TURNOVER ON DOWNS
        } else if(down==4 & (play$yards < togo)){
          posschange()
          down <- 1
          togo <- 10
          ydsbef <- 100 - (ydsbef - play$yards)
          togocorrector()
          posstmmargin_updater()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
          ### NO FIRST DOWN 
        } else if(down!=4 & (play$yards < togo)){
          down <- down + 1
          togo <- togo - play$yards
          ydsbef <- ydsbef - play$yards
          togocorrector()
          playtime <- round(rnorm(1, 4.5, .45))
          playtimecorrector()
          afterplaytimegenerator()
          timeupdater()
          quartercheck()
        }
      }
    }
    # scoredf$Quarter <- quarter
    # scoredf$SecondsLeft <- quartersecondsleft
    # scoredf$Possession <- posstm
    # scoredf$Down <- down
    # scoredf$ToGo <- togo
    # scoredf$YdstoEZ <- ydsbef
    # print(scoredf)
  }
  scoredf
}
simulator <- function(team1, team2){
  quarter <- 1
  secondsleft <- 3600
  quartersecondsleft <- 900
  down <- 1
  togo <- 10
  ydsbef <- 70
  teams <- c(team1, team2)
  scoredf <- data.frame(x1 = 0, x2 = 0)
  colnames(scoredf) <- c(team1, team2)
  startoffteam <- sample(teams, 1, prob = c(.5, .5))
  startdefteam <- teams[which(c(team1, team2)!=startoffteam)]
  posstm <- startoffteam
  deftm <- startdefteam
  
  ### VARIABLE UPDATING FUNCTIONS
  timeupdater <- function(){
    secondsleft <<- secondsleft - playtime - afterplaytime
    quartersecondsleft <<- quartersecondsleft - playtime - afterplaytime
  }
  playtimecorrector <- function(){
    if(playtime < 3){
      playtime <<- 3
    }
  }
  togocorrector <- function(){
    if(togo > ydsbef){
      togo <<- ydsbef
    }
  }
  posschange <- function(){
    dummytm <<- posstm
    posstm <<- deftm
    deftm <<- dummytm
  }
  touchdown <- function(){
    if(posstmmargin %in% c(5, 8, 11, 15)){
      mypoints <<- sample(c(6,8), 1, prob = c(.49, .51))
    } else{
      mypoints <<- sample(c(6,7), 1, prob = c(.025, .975))
    }
    mypoints
  }
  afterplaytimegenerator <- function(){
    if(posstmmargin<0 & secondsleft<=240){
      afterplaytime <<- round(rnorm(1, 9, 1))
    } else{
      afterplaytime <<- round(rnorm(1, 25, 3.5))
    }
  }
  posstmmargin_updater <- function(){
    posstmmargin <<- as.numeric(scoredf[[posstm]] - scoredf[[deftm]])
  }
  quartercheck <- function(){
    if(quartersecondsleft<=0){
      quarter <<- quarter + 1
      quartersecondsleft <<- 900 - (((5-quarter)*900) - secondsleft)
      ### HALFTIME SWITCH
      if(quarter==3){
        posstm <<- startdefteam
        deftm <<- startoffteam
        down <<- 1
        togo <<- 10
        ydsbef <<- 70
        posstmmargin_updater()
      }
    } else{
      quarter <<- quarter
    }
  }
  ###
  
  
  #### SCENARIO FUNCTIONS
  deftd <- function(playtimemean, playtimesd){
    down <<- 1
    togo <<- 10
    ydsbef <<- 70
    togocorrector()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  touchback <- function(playtimemean, playtimesd){
    posschange()
    down <<- 1
    togo <<- 10
    ydsbef <<- 80
    togocorrector()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  reg_turnover <- function(playtimemean, playtimesd){
    posschange()
    down <<- 1
    togo <<- 10
    ydsbef <<- ydsbef
    togocorrector()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  safety <- function(playtimemean, playtimesd){
    scoredf[[deftm]] <<- as.numeric(scoredf[[deftm]]) + 2
    posschange()
    down <<- 1
    togo <<- 10
    ydsbef <<- 65
    togocorrector()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  offtd <- function(playtimemean, playtimesd){
    scoredf[[posstm]] <<- as.numeric(scoredf[[posstm]]) + touchdown()
    posschange()
    down <<- 1
    togo <<- 10
    ydsbef <<- 70
    togocorrector()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  firstdown <- function(playtimemean, playtimesd){
    down <<- 1
    togo <<- 10
    ydsbef <<- ydsbef - play$yards
    togocorrector()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytimegenerator()
    timeupdater()
    quartercheck()
  }
  
  turnover_on_downs <- function(playtimemean, playtimesd){
    posschange()
    down <<- 1
    togo <<- 10
    ydsbef <<- 100 - (ydsbef - play$yards)
    togocorrector()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  regular_gain <- function(playtimemean, playtimesd){
    down <<- down + 1
    togo <<- togo - play$yards
    ydsbef <<- ydsbef - play$yards
    togocorrector()
    playtime <<- round(rnorm(1, playtimemean, playtimesd))
    playtimecorrector()
    afterplaytimegenerator()
    timeupdater()
    quartercheck()
  }
  
  made_fg <- function(){
    scoredf[[posstm]] <<- as.numeric(scoredf[[posstm]]) + 3
    down <<- 1
    togo <<- 10
    ydsbef <<- 70
    togocorrector()
    posschange()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, 5, 1))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  missed_fg <- function(){
    down <<- 1
    togo <<- 10
    ydsbef <<- (100-ydsbef) - 5
    togocorrector()
    posschange()
    posstmmargin_updater()
    playtime <<- round(rnorm(1, 5, 1))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  punt <- function(){
    option <<- "punt"
    posschange()
    down <<- 1
    togo <<- 10
    ydsbef <<- (100-ydsbef) + round(rnorm(1, 45, 5))
    ### TOUCHBACK
    if(ydsbef>=100){
      ydsbef <<- 80
    }
    togocorrector()
    playtime <<- round(rnorm(1, 9, 1.5))
    playtimecorrector()
    afterplaytime <<- 0
    timeupdater()
    quartercheck()
  }
  
  ####
  
  posstmmargin_updater()
  while(secondsleft > 0){
    scoredf$Quarter <- quarter
    scoredf$SecondsLeft <- quartersecondsleft
    scoredf$Possession <- posstm
    scoredf$Down <- down
    scoredf$ToGo <- togo
    scoredf$YdstoEZ <- ydsbef
    
    ### SPECIAL TEAMS & Fourth down going for it
    if(down==4){
      if(ydsbef>40){
        ### GO FOR IT ON FOURTH
        if(quartersecondsleft<=240 & posstmmargin<0 & togo<=10){
          option <- "goforit"
        } else{ ### PUNT
          punt()
        }
      } else{
        ### GO FOR IT ON FOURTH
        if((quarter %in% c(2,4) & quartersecondsleft<=240 & posstmmargin<0 & togo<=3) |
           (quarter %in% c(2,4) & quartersecondsleft<=120 & posstmmargin<0)){
          option <- "goforit"
        } else{ ### FIELD GOAL
          option <- "field goal attempt"
          setwd("C:/Users/Aaron Friedlander/Desktop/Cleaned Thesis QB Project/RDS Files/RDS Files From Model Building")
          fgdf <- readRDS("FirstDownLogRegResults.rds")
          fgdf$FGAtt <- as.factor(fgdf$FGAtt)
          fgmodf <- glm(FGAtt ~ FGDist + Quarter + Time2 + PossTmMargin + Down + ToGo, 
                        data = fgdf, family = "binomial")
          fgmakedf <- fgdf |> group_by(FGDist) |> 
            summarise(make_prob = mean(FGMakeProb))
          fgpredvardf <- data.frame(
            FGDist = ydsbef + 17,
            Quarter = quarter,
            Time2 = quartersecondsleft,
            PossTmMargin = posstmmargin,
            Down = down,
            ToGo = togo
          )
          fgattprob <- predict(fgmodf, newdata = fgpredvardf, type="response")
          fgattprob <- sqrt(sqrt(fgattprob))
          fgattselection <- sample(c("Yes", "No"), 1, prob = c(fgattprob, 1-fgattprob))
          ### FGATT
          if(fgattselection=="Yes"){
            option <- "field goal attempt"
            fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==ydsbef+17]
            fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
            ### MADE FIELD GOAL
            if(fgmake=="Yes"){
              made_fg()
              ### MISSED FIELD GOAL
            } else{
              missed_fg()
            }
            ### GO FOR IT ON FOURTH
          } else{
            option <- "goforit"
          }
        }
      }
    } else{
      option <- "regular"
    }
    ### OTHER field goal scenario
    if((secondsleft<=10 & posstmmargin>=-3 & posstmmargin<=0) |
       (quarter==2 & quartersecondsleft<=10)){
      option <- "field goal attempt"
      fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==ydsbef+17]
      fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
      ### MADE FIELD GOAL
      if(fgmake=="Yes"){
        made_fg()
        ### MISSED FIELD GOAL
      } else{
        missed_fg()
      }
    }
    
    ### REGULAR PLAYS
    if((down!=4 | option=="goforit") & option!="field goal attempt"){
      play <- safeyardsgained(posstm, deftm, down, togo, ydsbef, posstmmargin, 
                              quartersecondsleft, quarter)
      while(is.na(play$yards)){
        play <- safeyardsgained(posstm, deftm, down, togo, ydsbef, posstmmargin, 
                                quartersecondsleft, quarter)
      }
    }
    newyardsbef <- ydsbef - play$yards
    
    ################ PASS
    if(play$runpass=="Pass"){
      # INTERCEPTION
      if(play$result=="Interception"){
        ### PICK SIX
        if(newyardsbef>=100){
          deftd(8.5, .85)
          ### INTERCEPTION TOUCHBACK
        } else if(newyardsbef <= 0){
          touchback(5.5, .55)
          ### REGULAR INTERCEPTION
        } else{
          reg_turnover(6.5, .65)
        } 
        # FUMBLE
      } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained",
                                   "Sack_Fumble_Lost", "Complete_Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result %in% c("Sack_Fumble_Lost", "Complete_Fumble_Lost")){
          ### DEF TD
          if(newyardsbef>=100){
            deftd(5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            touchback(5, .5)
            ### REGULAR TURNOVER
          } else{
            reg_turnover(5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained")){
          ### SAFETY
          if(newyardsbef>=100){
            safety(5.5, .5)
            ### OFF TD
          } else if(newyardsbef<=0){
            offtd(7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=togo)){
            firstdown(7.5, .75)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down==4){
            turnover_on_downs(6, .6)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down!=4){
            regular_gain(6, .6)
          }
        }
        # SACK
      } else if(play$result=="Sack"){
        ### SAFETY
        if(newyardsbef >= 100){
          safety(4.5, .45)
          ### TURNOVER ON DOWNS
        } else if(down==4 & (newyardsbef < 100)){
          turnover_on_downs(4.5, .45)
          ### REGULAR SACK
        } else if(down!=4 & (newyardsbef < 100)){
          regular_gain(4.5, .45)
        }
        # COMPLETE PASSES
      } else if(play$result=="Complete"){
        ### SAFETY
        if(newyardsbef>=100){
          safety(5, .5)
          ### OFF TOUCHDOWN
        } else if(newyardsbef<=0){
          offtd(7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=togo)){
          firstdown(7.5, .75)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down==4){
          turnover_on_downs(6.5, .65)
          ### REGULAR NON FOURTH DOWN NO FIRST DOWN NO TOUCHDOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down!=4){
          regular_gain(6.5, .65)
        }
        # INCOMPLETE PASSES
      } else if(play$result=="Incomplete"){
        ### TURNOVER ON DOWNS
        if(down==4){
          turnover_on_downs(6, .6)
          ### REGULAR INCOMPLETION
        } else if(down!=4){
          down <- down + 1
          playtime <- round(rnorm(1, 6, .6))
          playtimecorrector()
          afterplaytime <- 0
          timeupdater()
          quartercheck()
        }
      }
      ################ RUN
    } else if(play$runpass=="Run"){
      # FUMBLE
      if(play$result %in% c("Fumble_Retained", "Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result=="Fumble_Lost"){
          ### DEF TD
          if(newyardsbef>=100){
            deftd(5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            touchback(5, .5)
            ### REGULAR TURNOVER
          } else{
            reg_turnover(5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result=="Fumble_Retained"){
          ### SAFETY
          if(newyardsbef>=100){
            safety(4.5, .45)
            ### OFF TD
          } else if(newyardsbef<=0){
            offtd(7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=togo)){
            firstdown(6.5, .65)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down==4){
            turnover_on_downs(4.5, .45)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down!=4){
            regular_gain(5, .5)
          }
        }
        # NON FUMBLE RUNS
      } else{
        ### SAFETY
        if(newyardsbef>=100){
          safety(4.5, .45)
          ### OFF TD
        } else if(newyardsbef<=0){
          offtd(7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=togo)){
          firstdown(6, .6)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down==4){
          turnover_on_downs(4.5, .45)
          ### NON FIRST DOWN REGULAR PLAY
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<togo) & down!=4){
          regular_gain(5, .5)
        }
      }
    }
    scoredf$Detail <- paste0(play$result, "; Yards: ", play$yards)
    scoredf <- scoredf |> relocate((Quarter:YdstoEZ), .before = ATL)
    print(scoredf)
  }
  scoredf
}
