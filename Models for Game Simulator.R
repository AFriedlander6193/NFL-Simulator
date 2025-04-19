library(tidyverse)
library(rlist)
library(brms)
library(cmdstanr)
library(rvest)
library(dplyr)
library(stringr)
library(installr)
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
library(xgboost)


# Setting up Data ---------------------------------------------------------

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

### Fixes
offdf$YdstoEZBef <- as.numeric(offdf$YdstoEZBef)
offdf <- offdf |> 
  mutate(YdstoEZBef = if_else(is.na(YdstoEZBef), 50, YdstoEZBef))
offdf <- offdf |> select(-time_to_throw)
###

offdf <- offdf |> filter(!is.na(coverage))
offdf <- offdf |> filter(coverage!="Miscellaneous")

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

#### Turning Categorical Variables into Factors
offdf$possessionTeam <- as.factor(offdf$possessionTeam)
offdf$off_form <- as.factor(offdf$off_form)
offdf$Off_Personnel <- as.factor(offdf$Off_Personnel)


### Newer Personnel with less options
offdf <- offdf |> 
  mutate(simple_personnel = paste0(RBcount, TEcount))
offdf <- offdf |> relocate(simple_personnel, .after = Off_Personnel)


### Combined PFF Grades for Each Play
offdf <- offdf |> 
  mutate(pff_player_grade_component = case_when(
    runorpass=="Pass" & is.na(receiving_grade_mean) & is.na(cover_target_route_grade_mean) ~ 
      ((pass_grade_mean + allother_offensive_grade_mean)/2 - allother_cover_players_grade_mean) +
      (oline_brocking_grade_mean - passrush_players_grade_mean),
    runorpass=="Pass" & !is.na(receiving_grade_mean) & is.na(cover_target_route_grade_mean) ~
      ((pass_grade_mean + receiving_grade_mean + allother_offensive_grade_mean)/3 -
      allother_cover_players_grade_mean) +
      (oline_brocking_grade_mean - passrush_players_grade_mean),
    runorpass=="Pass" & !is.na(receiving_grade_mean) ~ ((pass_grade_mean + receiving_grade_mean)/2 - cover_target_route_grade_mean) +
      (oline_brocking_grade_mean - passrush_players_grade_mean) +
      (allother_offensive_grade_mean - allother_cover_players_grade_mean),
    runorpass=="Pass" & is.na(receiving_grade_mean) & !is.na(cover_target_route_grade_mean) ~
      ((pass_grade_mean + allother_offensive_grade_mean)/2 - 
      (cover_target_route_grade_mean + allother_cover_players_grade_mean)/2) +
      (oline_brocking_grade_mean - passrush_players_grade_mean),
    runorpass=="Run" ~ (rush_grade_mean + oline_brocking_grade_mean + allother_offensive_grade_mean)/3 -
      allother_cover_players_grade_mean
  ),
  targetted_pff_comp = (pass_grade_mean+receiving_grade_mean)/2 - cover_target_route_grade_mean)

offdf <- offdf |> filter(!is.na(allother_offensive_grade_mean))

offdf <- offdf |> 
  mutate(completion = case_when(
    runorpass=="Run" ~ NA,
    runorpass=="Pass" & is.na(targettedplayer) ~ NA,
    runorpass=="Pass" & !is.na(targettedplayer) & is.na(receptionplayer) ~ "incomplete",
    runorpass=="Pass" & !is.na(targettedplayer) & !is.na(receptionplayer) ~ "complete"
  ))
offdf <- offdf |> relocate(completion, .before = targettedplayer)

passdf <- offdf |> filter(!is.na(targettedplayer))
ggplot(data=passdf, aes(x = targetted_pff_comp, y = factor(completion))) +
  geom_boxplot()


#
### Additional necessary packages
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(sure)

# Personnel and Coverage Models -------------------------------------------


offdf <- offdf |> filter(!is.na(off_form))
offdf$simple_personnel <- as.factor(offdf$simple_personnel)
offdf$possessionTeam <- as.factor(offdf$possessionTeam)
offdf$defensiveTeam <- as.factor(offdf$defensiveTeam)


### Creates the multinom model for offensive personnel
personnelmod <- multinom(simple_personnel ~  possessionTeam + QuarterSeconds + quarter + 
                  PossTmDiff + down + yardsToGo + YdstoEZBef , data = offdf)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(personnelmod, "Personnel_Multinom_Reg_Model.rds")

### Creates the ordinal model for defensive personnel
offdf <- offdf |> 
  mutate(simple_def_personnel = case_when(
    linebackerstotal + dlinetotal == 8 ~ "Heavy",
    linebackerstotal + dlinetotal >= 9 ~ "Goalline",
    ### I ADDED THE BOTTOM TWO AFTER WRITING REPORT
    simple_def_personnel %in% c("4-2", "2-4", "1-5", "5-1", "3-3") ~ "Nickel",
    simple_def_personnel=="1-4" ~ "Dime",
    TRUE ~ Def_Pers_Name
  ))



defpersonnelmod <- multinom(simple_def_personnel ~  simple_personnel + 
                              QuarterSeconds + quarter + 
                              PossTmDiff + down + yardsToGo + YdstoEZBef + defensiveTeam
                              , data = offdf)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(defpersonnelmod, "DefPersonnel_Multinom_Reg_Model.rds")



### Creates the ordinal model for formation
offdf$off_form <- as.factor(offdf$off_form)
formmod <- multinom(off_form ~  possessionTeam +QuarterSeconds + quarter + 
                  PossTmDiff + down + yardsToGo + YdstoEZBef + simple_personnel +
                    simple_def_personnel, 
                data = offdf)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(formmod, "Formation_Ord_Reg_Model.rds")


### Model for Defensive Coverage
offdf <- offdf |> 
  mutate(def_simple_coverage = case_when(
    coverage=="Cover-3" | coverage=="Cover-3 Seam" | coverage=="Cover-3 Cloud Left" |
      coverage=="Cover-3 Cloud Right" | coverage=="Cover-3 Double Cloud" ~ "Cover_3",
    coverage=="Cover-6 Right" | coverage=="Cover 6-Left" ~ "Cover_6",
    coverage=="Cover-1" | coverage=="Cover-1 Double" ~ "Cover_1",
    coverage=="Quarters" ~ "Cover_4",
    coverage=="Red Zone" ~ "RedZone",
    coverage=="Goal Line" ~ "GoalLine",
    coverage=="Cover-0" ~ "Cover_0",
    coverage=="2-Man" ~ "Cover2Man",
    coverage=="Cover-2" ~ "Cover_2",
    coverage=="Prevent" ~ "Prevent",
    coverage=="Bracket" ~ "Bracket"
  ))
offdf <- offdf |> relocate(def_simple_coverage, .after = coverage)
defcovmod <- multinom(def_simple_coverage ~  defensiveTeam +QuarterSeconds + quarter + 
                      PossTmDiff + down + yardsToGo + YdstoEZBef + simple_personnel +
                      simple_def_personnel + off_form, 
                    data = offdf)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(defcovmod, "Def_Coverage_Ord_Reg_Model.rds")



### RunPass Model
train_index <- sample(seq_len(nrow(offdf)), size = 0.8 * nrow(offdf))

train_data <- offdf[train_index, ]  # 80% training data
test_data <- offdf[-train_index, ]  # 20% testing data

train_data <- train_data |> 
  mutate(runorpass = if_else(runorpass=="Run", 0, 1))
train_data$runorpass <- as.factor(train_data$runorpass)

rpmodel <- glm(runorpass ~ possessionTeam +QuarterSeconds + quarter + 
                    PossTmDiff + down + yardsToGo + YdstoEZBef + simple_personnel + off_form +
                 simple_def_personnel + def_simple_coverage
                 , family = "binomial", data = train_data)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(rpmodel, "RunPass_Log_Reg_Model.rds")

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
write_csv(offdf, "AllOffensivePersonnel.csv")


# Route Models ------------------------------------------------------------
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

route_model_generator <- function(route){
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
  offdf <- suppressMessages(read_csv("AllOffensivePersonnel.csv"))
  routelist <- unique(na.omit(offdf$route1))
  routelist <- routelist[-which(routelist==route)]
  routelist <- paste0(routelist, "_count")
  eqlist <- c("possessionTeam", "QuarterSeconds", "quarter", "PossTmDiff", "down",
              "yardsToGo", "YdstoEZBef", "simple_personnel", "off_form", 
              "defensiveTeam", "simple_def_personnel", "def_simple_coverage", routelist)
  formula_str <- paste(paste0(route, "_count ~"), paste(eqlist, collapse = " + "))
  formula <- as.formula(formula_str)
  factor_route <- which(colnames(offdf)==paste0(route, "_count"))
  offdf[[factor_route]] <- as.factor(offdf[[factor_route]])
  model <- multinom(formula, data = offdf)
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models/Route Models")
  saveRDS(model, paste0(route,"Route_Ord_Reg_Model.rds"))
}

for(route in sort(unique(na.omit(offdf$route1)))){
  route_model_generator(route)
  print(route)
}


### Model for routes ran
offdf$routescount <- as.factor(offdf$routescount)
routecountmod <- polr(routescount ~  possessionTeam +QuarterSeconds + quarter + 
                  PossTmDiff + down + yardsToGo + YdstoEZBef + simple_personnel + 
                    off_form + defensiveTeam + simple_def_personnel + 
                    def_simple_coverage+ runorpass, 
                data = offdf, Hess = TRUE)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models/Route Models")
saveRDS(routecountmod, "Route_Count_Ord_Reg_Model.rds")



#### Creating targetted route

offdf$targettedroute <- ""
offdf <- offdf |> relocate(targettedroute, .after = targettedplayer)
for(m in 1:nrow(offdf)){
  if(offdf$runorpass[m]=="Pass" & !is.na(offdf$targettedplayer[m]) &
     isTRUE(grepl(offdf$targettedplayer[m], offdf$route_1[m]) |
            grepl(offdf$targettedplayer[m], offdf$route_2[m]) |
            grepl(offdf$targettedplayer[m], offdf$route_3[m]) |
            grepl(offdf$targettedplayer[m], offdf$route_4[m]) |
            grepl(offdf$targettedplayer[m], offdf$route_5[m])) ){
    r1check <- grepl(offdf$targettedplayer[m], offdf$route_1[m])
    r2check <- grepl(offdf$targettedplayer[m], offdf$route_2[m])
    r3check <- grepl(offdf$targettedplayer[m], offdf$route_3[m])
    r4check <- grepl(offdf$targettedplayer[m], offdf$route_4[m])
    r5check <- grepl(offdf$targettedplayer[m], offdf$route_5[m])
    checks <- c(r1check, r2check, r3check, r4check, r5check)
    trueroute <- which(checks)
    pulled_val <- as.character(offdf[(which(colnames(offdf)=="route_1")-1) + trueroute][m,])
    targettedroute <- sub("^\\d+ ", "", pulled_val)
    offdf$targettedroute[m] <- targettedroute
  } else{
    offdf$targettedroute[m] <- NA
  }
  print(m)
}

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
write_csv(offdf, "AllOffensivePersonnel.csv")



# Time To Throw Model -----------------------------------------------------
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")
offdf <- offdf |> 
  mutate(passrushers = if_else(runorpass=="Pass", 11-inCoverage, NA))

passdf <- offdf |> filter(!is.na(timeToThrow))
############### Time to Throw Model
timtothrowmod <- brm(timeToThrow ~ 
                        simple_personnel + 
                        off_form +
                        simple_def_personnel +
                        def_simple_coverage +
                        oline_brocking_grade_mean +
                        passrushers
                      ,
                      family = skew_normal(),
                      data = passdf,
                      chains = 4, iter = 2000, warmup = 1000)

new_tttdf <- data.frame(
  simple_personnel = "11",
  off_form = "EMPTY",
  simple_def_personnel = "4-3",
  def_simple_coverage = "Cover_0",
  oline_brocking_grade_mean = 50,
  passrushers = 7
)

pred_dist <- posterior_predict(timtothrowmod, newdata = new_tttdf)
dim(pred_dist)

pred_df <- as.data.frame(pred_dist) %>%
  pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")

ggplot(pred_df, aes(x = YardsGained, fill = Scenario)) +
  geom_density(alpha = 0.5) +  # Density plot of predicted yards gained
  facet_wrap(~Scenario, scales = "free_y") + 
  labs(title = "Predicted Probability Distribution of Time To Throw",
       x = "Time To Throw",
       y = "Density") +
  theme_minimal()

random_yards <- round(apply(pred_dist, 2, sample, size = 1),3)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(timtothrowmod, "TimetoThrow_Model.rds")
###############





# Number of Players in Coverage Model -------------------------------------
passdf <- offdf |> filter(!is.na(timeToThrow))
coverplayersmod <- brm(inCoverage ~ 
                       simple_personnel + 
                       off_form +
                       simple_def_personnel +
                       def_simple_coverage
                     ,
                     family = skew_normal(),
                     data = passdf,
                     chains = 4, iter = 2000, warmup = 1000)

new_coverdf <- data.frame(
  simple_personnel = "11",
  off_form = "EMPTY",
  simple_def_personnel = "3-4",
  def_simple_coverage = "Cover_3"
)

pred_dist <- posterior_predict(coverplayersmod, newdata = new_coverdf)
dim(pred_dist)

pred_df <- as.data.frame(pred_dist) %>%
  pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")

ggplot(pred_df, aes(x = YardsGained, fill = Scenario)) +
  geom_density(alpha = 0.5) +  # Density plot of predicted yards gained
  facet_wrap(~Scenario, scales = "free_y") + 
  labs(title = "Predicted Probability Distribution of Time To Throw",
       x = "Time To Throw",
       y = "Density") +
  theme_minimal()

random_yards <- round(apply(pred_dist, 2, sample, size = 1),0)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(coverplayersmod, "Cover_Players_Model.rds")

#######


routedf$targettedroute <- as.factor(routedf$targettedroute)
routedf$possessionTeam <- as.factor(routedf$possessionTeam)


targettedroutemod <- multinom(targettedroute ~QuarterSeconds + quarter + 
                                 PossTmDiff + down + yardsToGo + YdstoEZBef + 
                                simple_personnel + off_form + simple_def_personnel +
                                def_simple_coverage + timeToThrow +
                                ANGLE_count + CORNER_count + 
                                CROSS_count + FLAT_count + GO_count + HITCH_count + IN_count +
                                OUT_count + POST_count + SCREEN_count + SLANT_count + WHEEL_count +
                                simple_def_personnel + def_simple_coverage 
                              , data = routedf)
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(targettedroutemod, "Targetted_Route_Multinom_Model.rds")


#### Which Player Ran Which Route
# Fixes
offdf$RB3[2221] <- "53625"


routelist <- sort(unique(na.omit(offdf$route1)))
### This was becuase the code didn't think dummyposition was a colname for some reason
for(route in routelist){
  offdf <- offdf |> 
    mutate(dummycol = if_else(
      grepl(route, route_1) |
        grepl(route, route_2) |
        grepl(route, route_3) |
        grepl(route, route_4) |
        grepl(route, route_5),
      "Yes",
      "No"
    ))
  remove_string <- paste0(" ", route)
  offdf <- offdf |> 
    mutate(dummyplayer = case_when(
      grepl(route, route_1) ~ sub(remove_string, "", route_1),
      grepl(route, route_2) ~ sub(remove_string, "", route_2),
      grepl(route, route_3) ~ sub(remove_string, "", route_3),
      grepl(route, route_4) ~ sub(remove_string, "", route_4),
      grepl(route, route_5) ~ sub(remove_string, "", route_5)
    ))
  offdf$dummyposition <- ""
  key <- which(colnames(offdf)=="dummyposition")
  lowkey <- which(colnames(offdf)=="QB1")
  upkey <- which(colnames(offdf)=="C1")
  for(i in 1:nrow(offdf)){
    if(is.na(offdf$dummyplayer[i])){
      offdf$dummyposition[i] <- NA
    } else{
      col_indicator <- which(offdf[lowkey:upkey][i,]==offdf$dummyplayer[i])
      mycol <- colnames(offdf[col_indicator+(lowkey-1)])
      finalpos <- gsub("[0-9]+", "", mycol)
      if(finalpos=="RB" | finalpos=="FB" | finalpos=="WR" | finalpos=="TE"){
        offdf$dummyposition[i] <- finalpos
      } else{
        offdf$dummyposition[i] <- NA
      }
    }
    print(i)
  }
  colnames(offdf)[key] <- paste0(route, "ran")
  offdf <- offdf |> select(-dummycol)
  offdf <- offdf |> select(-dummyplayer)
}

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
write_csv(offdf, "AllOffensivePersonnel.csv")

### Players running routes models (This didn't work so I'm just gonna do group by tables)
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

position_route_model_generator <- function(route){
  colkey <- which(colnames(offdf)==paste0(route, "ran"))
  dummydf <- offdf[!is.na(offdf[colkey]),]
  
  routelist <- sort(unique(na.omit(offdf$route1)))
  routelist <- routelist[-which(routelist==route)]
  routelist <- paste0(routelist, "_count")
  eqlist <- c("possessionTeam", "QuarterSeconds", "quarter", "PossTmDiff", "down",
              "yardsToGo", "YdstoEZBef", "simple_personnel", "off_form", 
              "defensiveTeam", "simple_def_personnel", "def_simple_coverage", routelist)
  formula_str <- paste(paste0(route, "ran ~"), paste(eqlist, collapse = " + "))
  formula <- as.formula(formula_str)
  dummydf[[colkey]] <- as.factor(dummydf[[colkey]])
  model <- multinom(formula, data = dummydf)
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models/Routes by Positions Models")
  saveRDS(model, paste0(route,"Route_Pos_Multinom_Model.rds"))
}

for(route in sort(unique(na.omit(offdf$route1)))){
  position_route_model_generator(route)
  print(route)
}

dflist <- list()
routelist <- sort(unique(na.omit(offdf$route1)))
for(route in routelist){
  route_col <- which(colnames(offdf)==paste0(route, "ran"))
  checkdf <- offdf |> 
    group_by(across(all_of(route_col))) |>  # Dynamically select the column
    summarise(count = n(), .groups = "drop")
  checkdf <- subset(checkdf, !is.na(checkdf[[1]]))
  checkdf <- checkdf |> 
    mutate(prob = count / sum(count))
  checkdf$route <- route
  checkdf$count[2] <- checkdf$count[1] + checkdf$count[2]
  checkdf$prob[2] <- checkdf$prob[1] + checkdf$prob[2]
  checkdf <- checkdf[-1,]
  colnames(checkdf) <- c("position", "count", "prob", "route")
  dflist <- list.append(dflist, checkdf)
  print(route)
}

basefile <- dflist[[1]]
for(c in 2:12){
  basefile <- rbind(basefile, dflist[[c]])
}

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
write_csv(basefile, "PosRoutesRan.csv")



### Adding targetted route
lowkey <- which(colnames(offdf)=="QB1")
upkey <- which(colnames(offdf)=="C1")
offdf$targettedposition <- ""
offdf <- offdf |> relocate(targettedposition, .after = targettedplayer)
for(i in 1:nrow(offdf)){
  if(is.na(offdf$targettedplayer[i])){
    offdf$targettedposition[i] <- NA
  } else{
    col_indicator <- which(offdf[lowkey:upkey][i,]==offdf$targettedplayer[i])
    mycol <- colnames(offdf[col_indicator+(lowkey-1)])
    mycol <- sub("[0-9]+", "", mycol)
    offdf$targettedposition[i] <- mycol
  }
  print(i)
}

offdf <- offdf |> mutate(
  targettedposition = case_when(
    targettedposition == "FB" ~ "RB",
    targettedposition == "QB" | targettedposition == "T" ~ NA,
    TRUE ~ targettedposition
  )
)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
write_csv(offdf, "AllOffensivePersonnel.csv")


#### Model for YardsGained (Completed Passes only)
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

offdf$down <- as.factor(offdf$down)
offdf$runorpass <- as.factor(offdf$runorpass)
offdf$targettedroute <- as.factor(offdf$targettedroute)
offdf$targettedposition <- as.factor(offdf$targettedposition)
offdf$possessionTeam <- as.factor(offdf$possessionTeam)

recdf <- offdf |> filter(!is.na(receptionplayer))
recdf <- recdf |> 
  mutate(cover_target_route_grade_mean = 
           if_else(is.na(cover_target_route_grade_mean), 40, cover_target_route_grade_mean))
recdf <- recdf |> 
  mutate(rec_component = receiving_grade_mean - cover_target_route_grade_mean,
         block_component = oline_brocking_grade_mean - passrush_players_grade_mean,
         other_component = allother_offensive_grade_mean - allother_cover_players_grade_mean)


ydsgainedmodel <- brm(yardsGained ~ 
                      down + 
                      simple_personnel + 
                      off_form +
                      simple_def_personnel + 
                      def_simple_coverage + 
                      targettedroute + 
                      targettedposition + 
                      YdstoEZBef + 
                      yardsToGo +
                      # pass_grade_mean +
                      # receiving_grade_mean +
                      # cover_target_route_grade_mean
                      # +
                      (1 | possessionTeam) +
                      (1 | defensiveTeam)
                     ,
                   family = skew_normal(),
                   data = recdf,
                   chains = 4, iter = 2000, warmup = 1000, cores = 4)

new_passdf <- data.frame(
  down = c(1),  
  simple_personnel = c("11"),
  simple_def_personnel = c("3-4"),
  off_form = c("SHOTGUN"),
  def_simple_coverage = c("Cover_3"),
  targettedroute = c("GO"),
  targettedposition = c("WR"),
  YdstoEZBef = c(50),
  yardsToGo = c(10),
  possessionTeam = c("KC"),
  defensiveTeam = c("SEA"),
  pass_grade_mean = c(60),
  rec_component = c(0),
  block_component = c(5),
  other_component = c(5)
)

# Get expected (posterior mean) yards gained
pred_dist <- posterior_predict(ydsgainedmodel, newdata = new_passdf)
dim(pred_dist)

pred_df <- as.data.frame(pred_dist) %>%
  pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")

check <- pred_df |> group_by(Scenario) |> 
  summarise(mean = mean(YardsGained))

ggplot(pred_df, aes(x = YardsGained, fill = Scenario)) +
  geom_density(alpha = 0.5) +  # Density plot of predicted yards gained
  facet_wrap(~Scenario, scales = "free_y") + 
  labs(title = "Predicted Probability Distribution of Yards Gained",
       x = "Yards Gained",
       y = "Density") +
  theme_minimal()

random_yards <- round(apply(pred_dist, 2, sample, size = 1),0)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(ydsgainedmodel, "PassingYdsModel.rds")

### Creating model for pressure players
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(sure)
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")
offdf <- offdf |> 
  mutate(passrushers = 11 -inCoverage,
         pffpassrush = oline_brocking_grade_mean - passrush_players_grade_mean)
passdf <- offdf |> filter(runorpass=="Pass")
passdf <- passdf |> 
  mutate(pressureplayers = if_else(pressureplayers>=4, 4, pressureplayers))
passdf$pressureplayers <- factor(passdf$pressureplayers, ordered = TRUE)
# passdf$simple_personnel <- as.factor(passdf$simple_personnel)
# passdf$simple_def_personnel <- as.factor(passdf$simple_def_personnel)
# passdf$off_form <- as.factor(passdf$off_form)
# passdf$def_simple_coverage <- as.factor(passdf$def_simple_coverage)
pressureplayersmod <- polr(pressureplayers ~ down + yardsToGo + YdstoEZBef + 
                             simple_personnel + passrushers + pffpassrush +
                          off_form + simple_def_personnel +
                          def_simple_coverage,
                        data = passdf, Hess = TRUE)
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(pressureplayersmod, "PressurePlayersModel.rds")

predict(pressureplayersmod, dummydf, type = "probs")

### Probability of sack
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")
offdf <- offdf |> 
  mutate(passrushers = 11 -inCoverage,
         pffpassrush = oline_brocking_grade_mean - passrush_players_grade_mean)
passdf <- offdf |> filter(runorpass == "Pass")
passdf <- passdf |> 
  mutate(sackonplay = if_else(!is.na(sackdefender), 1, 0))
passdf$sackonplay <- as.factor(passdf$sackonplay)

sackmodel <- glm(sackonplay ~ down + yardsToGo + YdstoEZBef + 
                   simple_personnel + off_form + pressureplayers +
                   simple_def_personnel + def_simple_coverage + passrushers +
                   pffpassrush, data = passdf, family = "binomial")

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(sackmodel, "SackModel.rds")
###

#### Probability of incompletion on each play
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")
offdf <- offdf |> 
  mutate(passrushers = 11 -inCoverage,
         pffpassrush = oline_brocking_grade_mean - passrush_players_grade_mean)

passdf <- offdf |> filter(runorpass == "Pass")
# passdf <- passdf |> filter(!is.na(targettedroute))
# 
# probdf <- passdf |> group_by(targettedroute) |> 
#   summarise(comp_pct = sum(!is.na(receptionplayer)) / length(receptionplayer))
# 
# teamprobdf <- passdf |> group_by(possessionTeam, targettedroute) |> 
#   summarise(comp_pct = sum(!is.na(receptionplayer)) / length(receptionplayer))
# 
# setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
# write_csv(probdf, "generic_route_comp_probs.csv")
# write_csv(teamprobdf, "team_route_comp_probs.csv")

passattemptdf <- passdf |> filter(!is.na(targettedplayer))
passattemptdf <- passattemptdf |> 
  mutate(passresult = if_else(is.na(receptionplayer), "INCOMPLETE", "COMPLETE"))
passattemptdf <- passattemptdf |> 
  mutate(passresult = if_else(!is.na(fumbleplayer), "FUMBLE", passresult),
         passresult = if_else(!is.na(interceptiondefender), "INTERCEPTION", passresult))
passattemptdf$passresult <- as.factor(passattemptdf$passresult)
### THIS IS IMPORTANT
passattemptdf <- passattemptdf |> 
  mutate(coveragefactor = receiving_grade_mean - cover_target_route_grade_mean)

passresultmod <- multinom(passresult ~  targettedroute + pass_grade_mean +
                            coveragefactor + yardsToGo + simple_personnel +
                            simple_def_personnel + off_form + def_simple_coverage +
                            possessionTeam + defensiveTeam + targettedposition +
                            passrushers + pressureplayers + pffpassrush
                            , data = passattemptdf)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(passresultmod, "PassResult_Multinom_Reg_Model.rds")



### Finding which player ran the ball on a given play
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")
offdf$RB3[2038] <- "53625"
offdf$RB3[14776] <- "44879"

offdf$rushingposition <- ""
lowkey <- which(colnames(offdf)=="QB1")
upkey <- which(colnames(offdf)=="C1")
for(n in 1:nrow(offdf)){
  if(offdf$runorpass[n]=="Pass"){
    offdf$rushingposition[n] <- NA
  } else{
    col <- which(offdf[lowkey:upkey][n,]==offdf$rushingplayer[n])
    finalcol <- which(colnames(offdf)=="QB1") - 1 + col
    mycol <- colnames(offdf[finalcol])
    mycol <- sub("[0-9]+", "", mycol)
    offdf$rushingposition[n] <- mycol
  }
  print(n)
}

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
write_csv(offdf, "AllOffensivePersonnel.csv")

rushdf <- offdf |> filter(runorpass=="Run")

genericrushposdf <- rushdf |> group_by(rushingposition) |> 
  summarise(count = n(),
            prob = n()/nrow(rushdf)) |> 
  ungroup()

teamrushposdf <- rushdf |> group_by(possessionTeam, rushingposition) |> 
  summarise(count = n(), .groups = "drop_last") |> 
  mutate(prob = count / sum(count, na.rm = TRUE)) |> 
  ungroup()

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
write_csv(genericrushposdf, "generic_rush_pos.csv")
write_csv(teamrushposdf, "team_rush_pos.csv")

### Model for rush yards
rushmodel <- brm(yardsGained ~ 
                        down + 
                        rushingposition +
                        simple_personnel +
                        simple_def_personnel +
                        def_simple_coverage +
                        off_form +
                        YdstoEZBef + 
                        yardsToGo + 
                        pff_player_grade_component + 
                        (1 | possessionTeam) +
                        (1 | defensiveTeam),  
                      family = skew_normal(),
                      data = rushdf,
                      chains = 4, iter = 2000, warmup = 1000, cores = 4)

new_rushdf <- data.frame(
  down = c(1, 1, 1),  
  rushingposition = c("RB", "RB", "RB"),
  simple_personnel = c("11", "11", "11"),
  simple_def_personnel = c("3-4", "3-4","3-4"),
  def_simple_coverage = c("Cover_3", "Cover_3", "Cover_3"),
  off_form = c("SHOTGUN", "SHOTGUN", "SHOTGUN"),
  YdstoEZBef = c(50, 50, 50),
  yardsToGo = c(10, 10, 10),
  possessionTeam = c("DEN", "DEN", "DEN"),
  defensiveTeam = c("SEA", "SEA", "SEA"),
  pff_player_grade_component = c(0, 5, 10)
)

# Get expected (posterior mean) yards gained
pred_dist <- posterior_predict(rushmodel, newdata = new_rushdf)
dim(pred_dist)

pred_df <- as.data.frame(pred_dist) %>%
  pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")

ggplot(pred_df, aes(x = YardsGained, fill = Scenario)) +
  geom_density(alpha = 0.5) +  # Density plot of predicted yards gained
  facet_wrap(~Scenario, scales = "free_y") + 
  labs(title = "Predicted Probability Distribution of Yards Gained",
       x = "Yards Gained",
       y = "Density") +
  theme_minimal()

check <- pred_df |> group_by(Scenario) |> 
  summar

random_yards <- round(apply(pred_dist, 2, sample, size = 1),0)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(rushmodel, "RushingYdsModel.rds")


## Model for Interceptions
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

passdf <- offdf |> filter(runorpass=="Pass")
passdf <- passdf |>
  mutate(intonplay = if_else(is.na(interceptiondefender), 0, 1))
passdf$intonplay <- as.factor(passdf$intonplay)
intmodel <- glm(intonplay ~ down + yardsToGo + YdstoEZBef + targettedroute +
                  possessionTeam + off_form + simple_personnel, data = passdf,
                family = "binomial")

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(intmodel, "InterceptionModel.rds")

intdf <- passdf |> filter(intonplay==1)

intydsmodel <- brm(interceptionyards ~ 
                   down + 
                   targettedroute +
                   targettedposition +
                   simple_personnel +
                   simple_def_personnel +
                   off_form +
                   def_simple_coverage + 
                   YdstoEZBef + 
                   yardsToGo + 
                   (1 | possessionTeam) +
                   (1 | defensiveTeam),  
                 family = skew_normal(),
                 data = intdf,
                 chains = 4, iter = 2000, warmup = 1000, cores = 4)

new_intdf <- data.frame(
  down = c(1, 1, 1),  
  simple_personnel = c("11", "11", "11"),
  simple_def_personnel = c("3-4", "3-4", "3-4"),
  def_simple_coverage = c("Cover_3", "Cover_3", "Cover_3"),
  off_form = c("SHOTGUN", "SHOTGUN", "SHOTGUN"),
  targettedroute = c("IN", "OUT", "GO"),
  targettedposition = c("WR", "WR", "WR"),
  YdstoEZBef = c(50, 50, 50),
  yardsToGo = c(10, 10, 10),
  possessionTeam = c("DEN", "DEN", "DEN"),
  defensiveTeam = c("SEA", "SEA", "SEA")
)

# Get expected (posterior mean) yards gained
pred_dist <- posterior_predict(intydsmodel, newdata = new_intdf)
dim(pred_dist)

pred_df <- as.data.frame(pred_dist) %>%
  pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")

ggplot(pred_df, aes(x = YardsGained, fill = Scenario)) +
  geom_density(alpha = 0.5) +  # Density plot of predicted yards gained
  facet_wrap(~Scenario, scales = "free_y") + 
  labs(title = "Predicted Probability Distribution of Yards Gained",
       x = "Yards Gained",
       y = "Density") +
  theme_minimal()

random_yards <- round(apply(pred_dist, 2, sample, size = 1),0)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(intydsmodel, "IntYdsModel.rds")


### Finding which player fumbled the ball on a given play
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")

offdf$fumbleposition <- ""
lowkey <- which(colnames(offdf)=="QB1")
upkey <- which(colnames(offdf)=="C1")
for(n in 1:nrow(offdf)){
  if(is.na(offdf$fumbleplayer[n])){
    offdf$fumbleposition[n] <- NA
  } else{
    col <- which(offdf[lowkey:upkey][n,]==offdf$fumbleplayer[n])
    finalcol <- which(colnames(offdf)=="QB1") - 1 + col
    mycol <- colnames(offdf[finalcol])
    mycol <- sub("[0-9]+", "", mycol)
    offdf$fumbleposition[n] <- mycol
  }
  print(n)
}

### Fumblelost dataframe
fumbdf <- offdf |> filter(!is.na(fumbleplayer))
fumbdf <- fumbdf |> filter(fumbleposition!="C")
posfumbdf <- fumbdf |> group_by(fumbleposition) |> 
  summarise(count = length(fumbleposition),
            lostcount = sum(fumblelost==1),
            lostprob = sum(fumblelost==1)/length(fumbleposition))

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
write_csv(posfumbdf, "FumbleLostProb.csv")

# NOTE FROM AARON ON 4/9/25 (CHANGE THIS MODEL TO JUST RUN PLAYS)
### Fumble Model
rushdf <- offdf |> filter(runorpass=="Run")
rushdf <- rushdf |> 
  mutate(fumbleonplay = if_else(is.na(fumbleplayer), 0, 1))

rushdf$fumbleonplay <- as.factor(rushdf$fumbleonplay)
fumblemodel <- glm(fumbleonplay ~ down + yardsToGo + YdstoEZBef +
                  possessionTeam + off_form + simple_personnel + rushingposition +
                  simple_def_personnel + def_simple_coverage + defensiveTeam, 
                  data = rushdf,
                family = "binomial")
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(fumblemodel, "RushFumbleModel.rds")


## (OLD) Sack Model
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
offdf <- read_csv("AllOffensivePersonnel.csv")
passdf <- offdf |> filter(runorpass=="Pass")
passdf <- passdf |>
  mutate(sackonplay = if_else(is.na(sackdefender), 0, 1))
sackmodel <- glm(sackonplay ~ down + yardsToGo + YdstoEZBef +
                   possessionTeam + off_form + simple_personnel
                 , data = passdf,
                 family = "binomial")
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(sackmodel, "SackModel.rds")

sackdf <- passdf |> filter(sackonplay==1)
# sackyardsmodel <- lm(sackyards ~ down + yardsToGo + YdstoEZBef +
#                        possessionTeam + off_form + simple_personnel
#                      , data = sackdf)

sackyardsmodel <- brm(sackyards ~ 
                   down + 
                   simple_personnel +
                   simple_def_personnel +
                   def_simple_coverage +
                   off_form +
                   YdstoEZBef + 
                   yardsToGo + 
                   (1 | possessionTeam) +
                   (1 | defensiveTeam),  
                 family = gaussian(),
                 data = sackdf,
                 chains = 4, iter = 2000, warmup = 1000, cores = 4)

new_sackdf <- data.frame(
  down = c(1, 1, 1),  
  simple_personnel = c("11", "11", "11"),
  simple_def_personnel = c("3-4", "3-4", "3-4"),
  def_simple_coverage = c("Cover_3", "Cover_3", "Cover_3"),
  off_form = c("SINGLEBACK", "SINGLEBACK", "SINGLEBACK"),
  YdstoEZBef = c(50, 50, 50),
  yardsToGo = c(10, 10, 10),
  possessionTeam = c("DEN", "KC", "MIN"),
  defensiveTeam = c("SEA", "SEA", "SEA")
)

# Get expected (posterior mean) yards gained
pred_dist <- posterior_predict(sackyardsmodel, newdata = new_sackdf)
dim(pred_dist)

pred_df <- as.data.frame(pred_dist) %>%
  pivot_longer(cols = everything(), names_to = "Scenario", values_to = "YardsGained")

ggplot(pred_df, aes(x = YardsGained, fill = Scenario)) +
  geom_density(alpha = 0.5) +  # Density plot of predicted yards gained
  facet_wrap(~Scenario, scales = "free_y") + 
  labs(title = "Predicted Probability Distribution of Yards Gained",
       x = "Yards Gained",
       y = "Density") +
  theme_minimal()

random_yards <- round(apply(pred_dist, 2, sample, size = 1),0)

setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Models")
saveRDS(sackyardsmodel, "SackYardsModel.rds")






# ### Creating model for seconds run on play
# setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
# offdf <- read_csv("AllOffensivePersonnel.csv")
# offdf <- offdf |> arrange(gameId, playId)
# 
# offdf <- offdf |> 
#   mutate(secondsonplay = if_else(gameId==lead(gameId), 
#                                  SecondsLeft-lead(SecondsLeft), SecondsLeft-0))
# offdf <- offdf |> relocate(secondsonplay, .after = SecondsLeft)

