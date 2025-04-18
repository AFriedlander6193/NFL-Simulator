library(tidyverse)
library(fuzzyjoin)
library(stringr)

##### Merging nflId and playerId
# Standardizing function
clean_name <- function(name) {
  name %>%
    str_to_lower() %>%            # Convert to lowercase
    str_replace_all("\\.", "") %>% # Remove periods (A.J. -> AJ)
    str_trim()                     # Remove extra spaces
}

### nfldata
setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Kaggle Data")
players <- read_csv("players.csv")
newplayers <- players |> select(displayName, nflId, position)
newplayers <- newplayers |> 
  mutate(position = case_when(
    position=="CB" | position=="DB" | position=="FS" | position=="SS" ~ "DB",
    position=="ILB" | position=="MLB" | position=="LB" | position=="OLB" |
    position=="DE" | position=="DT" | position=="NT" ~ "F7",
    position=="C" | position=="G" | position=="T" ~ "OL",
    TRUE ~ position
  ),
  displayName = clean_name(displayName))

### pffdata
setwd("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats")
passing <- read_csv("AllPassing2015+.csv")
passing <- passing |> filter(Year==2022 & Week<=9)
newpass <- passing |> select(player, player_id, position)
rushrec <- read_csv("AllRushRecBlock2015+.csv")
rushrec <- rushrec |> filter(Year==2022 & Week<=9)
newrushrec <- rushrec |> select(player, player_id, position)
defense <- read_csv("AllDefense2015+.csv")
defense <- defense |> filter(Year==2022 & Week<=9)
newdefense <- defense |> select(player, player_id, position)

totaldf <- rbind(newpass, newrushrec, newdefense)
totaldf <- unique(totaldf)
colnames(totaldf) <- c("displayName", "pffId", "position")
totaldf <- totaldf |> 
  mutate(displayName = clean_name(displayName))

totaldf <- totaldf |> 
  mutate(position = case_when(
    position=="C" | position=="G" | position=="T" ~ "OL",
    position=="LB" | position=="ED" | position=="DI" ~ "F7",
    position=="HB" ~ "RB",
    position=="CB" | position=="S" ~ "DB",
    TRUE ~ position
  ))
totaldf$displayName <- gsub(" iii", "", totaldf$displayName)
totaldf$displayName <- gsub(" ii", "", totaldf$displayName)
totaldf$displayName <- gsub(" jr", "", totaldf$displayName)
totaldf$displayName <- gsub(" sr", "", totaldf$displayName)

finaldf <- left_join(newplayers, totaldf)
finaldf <- unique(finaldf)


### MANUAL FIXES
finaldf$pffId[finaldf$displayName=="matt slater"] <- 4467
finaldf$pffId[finaldf$displayName=="nicholas williams"] <- 8006
finaldf$pffId[finaldf$displayName=="nickell robey"] <- 8483
finaldf$pffId[finaldf$displayName=="cameron fleming"] <- 8775
finaldf$pffId[finaldf$displayName=="willie snead"] <- 8999
finaldf$pffId[finaldf$displayName=="shaquille mason"] <- 9564
finaldf$pffId[finaldf$displayName=="trenton brown"] <- 9677
finaldf$pffId[finaldf$displayName=="matt judon"] <- 10780
finaldf$pffId[finaldf$displayName=="mitchell trubisky"] <- 11757
finaldf$pffId[finaldf$displayName=="takkarist mckinley"] <- 11781
finaldf$pffId[finaldf$displayName=="sidney jones"] <- 11798
finaldf$pffId[finaldf$displayName=="alex armah"] <- 11948
finaldf$pffId[finaldf$displayName=="pat o'connor"] <- 12005
finaldf$pffId[finaldf$displayName=="joseph jones"] <- 12321
finaldf$pffId[finaldf$displayName=="joseph noteboom"] <- 46249
finaldf$pffId[finaldf$displayName=="ogbonnia okoronkwo"] <- 50130
finaldf$pffId[finaldf$displayName=="jake martin"] <- 50152
finaldf$pffId[finaldf$displayName=="sebastian joseph"] <- 49935
finaldf$pffId[finaldf$displayName=="jeffery wilson"] <- 29413
finaldf$pffId[finaldf$displayName=="trenton scott"] <- 24481
finaldf$pffId[finaldf$displayName=="chauncey gardner-johnson"] <- 49371
finaldf$pffId[finaldf$displayName=="ugochukwu amadi"] <- 49352
finaldf$pffId[finaldf$displayName=="michael jackson"] <- 50437
finaldf$pffId[finaldf$displayName=="yosuah nijman"] <- 60822
finaldf$pffId[finaldf$displayName=="cameron lewis"] <- 49395
finaldf$pffId[finaldf$displayName=="david sills"] <- 34579
finaldf$pffId[finaldf$displayName=="iosua opeta"] <- 45110
finaldf$pffId[finaldf$displayName=="joseph fortson"] <- 94384
finaldf$pffId[finaldf$displayName=="josh uche"] <- 27379
finaldf$pffId[finaldf$displayName=="justin madubuike"] <- 44304
finaldf$pffId[finaldf$displayName=="michael danna"] <- 48415
finaldf$pffId[finaldf$displayName=="michael onwenu"] <- 27357
finaldf$pffId[finaldf$displayName=="ladarius hamilton"] <- 50192
finaldf$pffId[finaldf$displayName=="patrick surtain"] <- 83119
finaldf$pffId[finaldf$displayName=="gregory rousseau"] <- 76889
finaldf$pffId[finaldf$displayName=="joe tryon"] <- 58272
finaldf$pffId[finaldf$displayName=="trevon moehrig"] <- 77847
finaldf$pffId[finaldf$displayName=="samuel cosmi"] <- 60114
finaldf$pffId[finaldf$displayName=="carlos basham"] <- 44320
finaldf$pffId[finaldf$displayName=="josh palmer"] <- 62979
finaldf$pffId[finaldf$displayName=="patrick jones"] <- 43974
finaldf$pffId[finaldf$displayName=="tedarrell slaton"] <- 56597
finaldf$pffId[finaldf$displayName=="demetric felton"] <- 40568
finaldf$pffId[finaldf$displayName=="william bradley-king"] <- 44184
finaldf$pffId[finaldf$displayName=="ahmad gardner"] <- 100903
finaldf$pffId[finaldf$displayName=="cordale flott"] <- 98834
finaldf$pffId[finaldf$displayName=="thomas booker"] <- 82168
finaldf$pffId[finaldf$displayName=="mike woods"] <- 84117
finaldf$pffId[finaldf$displayName=="zander horvath"] <- 57384
finaldf$pffId[finaldf$displayName=="joshua onujiogu"] <- 156131
finaldf$pffId[finaldf$displayName=="tanner conner"] <- 22777
finaldf$pffId[finaldf$displayName=="ethan fernea"] <- 61765
finaldf$pffId[finaldf$displayName=="stephen jones"] <- 55785
finaldf$pffId[finaldf$displayName=="nick bellore"] <- 6515
finaldf$pffId[finaldf$displayName=="ty montgomery"] <- 9527
finaldf$pffId[finaldf$displayName=="miles killebrew"] <- 10745
finaldf$pffId[finaldf$displayName=="jordan howard"] <- 10784
finaldf$pffId[finaldf$displayName=="taysom hill"] <- 12112
finaldf$pffId[finaldf$displayName=="keke coutee"] <- 47698
finaldf$pffId[finaldf$displayName=="javon wims"] <- 41406
finaldf$pffId[finaldf$displayName=="isaiah simmons"] <- 42451
finaldf$pffId[finaldf$displayName=="patrick taylor"] <- 40448
finaldf$pffId[finaldf$displayName=="feleipe franks"] <- 40217
finaldf$pffId[finaldf$displayName=="lance mccutcheon"] <- 61478
finaldf$pffId[finaldf$displayName=="tay martin"] <- 61811




savedf <- finaldf 
colnames(passing)[colnames(passing)=="player_id"] <- "pffId"
colnames(rushrec)[colnames(rushrec)=="player_id"] <- "pffId"
colnames(defense)[colnames(defense)=="player_id"] <- "pffId"

passing <- passing |> relocate(attempts, .after = team_name)
rushrec <- rushrec |> relocate(c(attempts, routes, snap_counts_offense), 
                               .after = team_name)
defense <- defense |> relocate(c(snap_counts_pass_rush, snap_counts_run_defense,
                               snap_counts_coverage), .after = team_name)
defense <- defense |> 
  mutate(defensesnaps = snap_counts_pass_rush + snap_counts_run_defense +
           snap_counts_coverage)
defense <- defense |> relocate(defensesnaps, .after = team_name)

groupedpassing <- passing |> group_by(pffId) |> 
  summarise(attempts = sum(na.omit(attempts)),
    across(no_screen_grades_hands_fumble:more_grades_run,
                   ~mean(. , na.rm=TRUE), .names = "avg_{.col}"),
            across(no_screen_grades_hands_fumble:more_grades_run,
                   ~sd(. , na.rm=TRUE), .names = "sd_{.col}"))

groupedrushrec <- rushrec |> group_by(pffId) |> 
  summarise(displayname = player[1],
            position = position[1],
            attempts = sum(na.omit(attempts)),
            routes = sum(na.omit(routes)),
            blocksnaps = sum(na.omit(snap_counts_offense)),
    across(grades_offense:grades_run,
                   ~mean(. , na.rm=TRUE), .names = "avg_{.col}"),
            across(grades_offense:grades_run,
                   ~sd(. , na.rm=TRUE), .names = "sd_{.col}"))

groupeddefense <- defense |> group_by(pffId) |> 
  summarise(defensesnaps = sum(na.omit(defensesnaps)),
            positon = position[1],
            passrushsnaps = sum(na.omit(snap_counts_pass_rush)),
            rundefensesnaps = sum(na.omit(snap_counts_run_defense)),
            coveragesnaps = sum(na.omit(snap_counts_coverage)),
    across(man_grades_coverage_defense:true_pass_set_grades_pass_rush_defense,
                   ~mean(. , na.rm=TRUE), .names = "avg_{.col}"),
            across(man_grades_coverage_defense:true_pass_set_grades_pass_rush_defense,
                   ~sd(. , na.rm=TRUE), .names = "sd_{.col}"))

groupedrushrec <- groupedrushrec |> select(-position)


finalpassdf <- left_join(savedf, groupedpassing)
finalrushdf <- left_join(savedf, groupedrushrec)
finaldefensedf <- left_join(savedf, groupeddefense)


setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Miscellaneous Data")
write_csv(finalpassdf, "passingnflID+pffID.csv")
write_csv(finalrushdf, "rushrecblocknflID+pffID.csv")
write_csv(finaldefensedf, "defensenflID+pffID.csv")













