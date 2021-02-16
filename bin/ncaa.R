
library(dplyr)

# get ncaaHelper functions
source('src/ncaaHelpers.R')

# download stats
lapply(2002:2021, FUN=scrape_kenpom)

# load stats
stats <- do.call('rbind',
                 lapply(list.files('data/kenpom', '.csv$', full.names=T), FUN=read.csv))

# load scores - from https://data.world/michaelaroy/ncaa-tournament-results
scores <- read.csv('data/michaelaroy-ncaa-tournament-results/data/big_dance_csv.csv') %>% 
  filter(year %in% 2002:2021)

# join
test <- left_join(scores, stats, by=c('team'='Team','year'='year'))
test[which(is.na(test$Conf)),] %>% pull(team) %>% unique()

test <- left_join(scores, stats, by=c('team_2'='Team','year'='year'))
test[which(is.na(test$Conf)),] %>% pull(team_2) %>% unique()

unique(stats$Team) %>% sort()
unique(c(scores$team,scores$team_2)) %>% sort()

stats %>% filter(Team=='Miami OH')
scores %>% filter(team_2=='Miami')

library(xlsx)
# stats
kp19 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/kenpom2019.xlsx', sheetName='Sheet1')
kp18 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/kenpom2018.xlsx', sheetName='Sheet1')
kp17 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/kenpom2017.xlsx', sheetName='Sheet1')
kp16 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/kenpom2016.xlsx', sheetName='Sheet1')
kp15 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/kenpom2015.xlsx', sheetName='Sheet1')
kp14 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/kenpom2014.xlsx', sheetName='Sheet1')

# games
t19 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/teams2019.xlsx', sheetName='Sheet1')
t18 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/teams2018.xlsx', sheetName='Sheet1')
g18 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/games2018.xlsx', sheetName='Sheet1')
g17 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/games2017.xlsx', sheetName='Sheet1')
g16 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/games2016.xlsx', sheetName='Sheet1')
g15 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/games2015.xlsx', sheetName='Sheet1')
g14 <- read.xlsx(file='C:/Users/ryanm/Documents/R/ryan_projects/games2014.xlsx', sheetName='Sheet1')

# check names
unique(c(g14[which(is.na(match(g14$t1_name, kp14$Team))), 't1_name'],
         g14[which(is.na(match(g14$t2_name, kp14$Team))), 't2_name']))
sort(kp14$Team)


game.stats.fx <- function(yr) {
  yr_ch <- as.character(yr)
  kp <- read.xlsx(file=paste0('C:/Users/ryanm/Documents/R/ryan_projects/kenpom',yr_ch,'.xlsx'), sheetName='Sheet1')
  gm <- read.xlsx(file=paste0('C:/Users/ryanm/Documents/R/ryan_projects/games',yr_ch,'.xlsx'), sheetName='Sheet1')
  v <- do.call('rbind', lapply(1:nrow(gm), function(i) {
    stats_t1 <- kp[match(gm[i,2], kp$Team),]
    stats_t2 <- kp[match(gm[i,5], kp$Team),]
    row1 <- cbind(gm[i,c(2,1,3,6)], stats_t1[,c(1,3,4,6:9,11:13)], 
                  gm[i,c(5,4)], stats_t2[,c(1,3,4,6:9,11:13)])
    row2 <- cbind(gm[i,c(5,4,6,3)], stats_t2[,c(1,3,4,6:9,11:13)], 
                  gm[i,c(2,1)], stats_t1[,c(1,3,4,6:9,11:13)])
  }))
  v$yr <- yr
  names(v) <- c('Team', 'TeamSd', 'TeamSc', 'OppSc', 'TeamRk', 'TeamConf', 'TeamWL', 
                'TeamO', 'TeamD', 'TeamT', 'TeamL', 'TeamOO', 'TeamOD', 'TeamNCSOS', 
                'Opp', 'OppSd', 'OppRk', 'OppConf', 'OppWL', 'OppO', 'OppD', 'OppT', 
                'OppL', 'OppOO', 'OppOD', 'OppNCSOS', 'Year')
  return(v)
}


library(dplyr)
library(tidyr)

# gather historical data
master_df <- data.frame(
  do.call('rbind', list(
    game.stats.fx(2018),
    game.stats.fx(2017),
    game.stats.fx(2016),
    game.stats.fx(2015),
    game.stats.fx(2014)
)))

rec.fx <- function(){
  t <- master_df[,c(1,27)]
  o <- master_df[,c(15,27)]
  names(t) <- c('team', 'yr')
  names(o) <- c('team', 'yr')
  to <- rbind(t,o)
  to %>% group_by(team) %>% summarise(ct=n())
}

record <- rec.fx()

reg_df <- master_df %>% 
  mutate(outcome=as.factor(ifelse(as.numeric(TeamSc) > as.numeric(OppSc), 'win', 'lose')),
         TeamSd=as.numeric(TeamSd), TeamRk=as.numeric(TeamRk), TeamConf=as.factor(TeamConf),
         TeamO=as.numeric(TeamO), TeamD=as.numeric(TeamD), TeamT=as.numeric(TeamT),
         TeamL=as.numeric(TeamL), TeamOO=as.numeric(TeamOO), TeamOD=as.numeric(TeamOD),
         TeamNCSOS=as.numeric(TeamNCSOS), OppSd=as.numeric(OppSd), OppRk=as.numeric(OppRk), 
         OppConf=as.factor(OppConf), OppO=as.numeric(OppO), OppD=as.numeric(OppD), 
         OppT=as.numeric(OppT), OppL=as.numeric(OppL), OppOO=as.numeric(OppOO), 
         OppOD=as.numeric(OppOD), OppNCSOS=as.numeric(OppNCSOS),
         upset=as.factor(ifelse((TeamSd > OppSd) & outcome=='win', 'yes', 'no')),
         WinMargin=(as.numeric(TeamSc))/(as.numeric(TeamSc)+as.numeric(OppSc)))
reg_df <- left_join(reg_df, record, by=c('Team'='team'))
reg_df <- left_join(reg_df, record, by=c('Opp'='team'), suffix=c('_team', '_opp'))
reg_df <- reg_df %>% mutate(RecMargin=ct_team/(ct_team+ct_opp))

# model
library(glmnet)
x1 <- as.matrix(select(reg_df, TeamO, TeamD, TeamT, TeamL, TeamOO, TeamOD, TeamNCSOS,  
                      OppO, OppD, OppT, OppL, OppOO, OppOD, OppNCSOS, ct_team, ct_opp, RecMargin))
x2 <- as.matrix(select(reg_df, TeamO, TeamD, TeamT, TeamL, TeamOO, TeamOD, TeamNCSOS,  
                      OppO, OppD, OppT, OppL, OppOO, OppOD, OppNCSOS, ct_team, ct_opp, RecMargin))
y1 <- as.matrix(select(reg_df, outcome))
y2 <- as.matrix(select(reg_df, upset))
y1 <- y1[!is.na(apply(x1, 1, sum)),]
y2 <- y2[!is.na(apply(x2, 1, sum)),]
x1 <- x1[!is.na(apply(x1, 1, sum)),]
x2 <- x2[!is.na(apply(x2, 1, sum)),]

set.seed(1011)
cv_outcome <- cv.glmnet(x1, y1, family="binomial", type.measure="auc", nfolds=10, alpha=1)
set.seed(1011)
cv_upset <- cv.glmnet(x2, y2, family="binomial", type.measure="auc", nfolds=10, alpha=1)

plot(cv_outcome)
plot(cv_upset)
coef(cv_outcome, s = "lambda.min")
coef(cv_outcome, s = "lambda.1se")
coef(cv_upset, s = "lambda.min")
coef(cv_upset, s = "lambda.1se")

# performace
y0 <- ifelse(y1=='lose', 0, 1)
p0 <- predict(cv_outcome, newx=x1, s="lambda.min", type="response")
brierScore <- mean((p0-y0)^2)


# apply model
every_game <- function(yr=2019) {
  require(tidyr)
  require(dplyr)
  yr_ch <- as.character(yr)
  team_stats <- read.xlsx(file=paste0('C:/Users/ryanm/Documents/R/ryan_projects/kenpom',yr_ch,'.xlsx'), sheetName='Sheet1')
  teams <- read.xlsx(file=paste0('C:/Users/ryanm/Documents/R/ryan_projects/teams',yr_ch,'.xlsx'), sheetName='Sheet1')
  # get every possible matchup
  possible_games <- expand.grid(cbind(unite(teams, col='id', sep='_'), unite(teams, col='id', sep='_')))
  names(possible_games) <- c('team1', 'team2')
  possible_games <- possible_games %>% 
    separate(col='team1', into=c('team1', 'region1', 'seed1'), sep='_') %>% 
    separate(col='team2', into=c('team2', 'region2', 'seed2'), sep='_') %>% 
    filter(!(team1==team2))
  # get stats for each team in every matchup
  prd_df <- left_join(possible_games, team_stats, by=c('team1'='Team'))
  prd_df <- left_join(prd_df, team_stats, by=c('team2'='Team'))
  nm <- c('Team', 'TeamRg', 'TeamSd', 'Opp', 'OppRg', 'OppSd', 'TeamRk', 'TeamConf', 'TeamWL',
          'TeamE', 'TeamO', 'TeamD', 'TeamT', 'TeamL', 'TeamSOS', 'TeamOO', 'TeamOD', 'TeamNCSOS',
          'OppRk', 'OppConf', 'OppWL', 'OppE', 'OppO', 'OppD', 'OppT', 'OppL', 'OppSOS', 'OppOO', 
          'OppOD', 'OppNCSOS')
  names(prd_df) <- nm
  # set up matrix for regularized regression prediction
  newdata_df <- prd_df %>% 
    mutate(TeamSd=as.numeric(TeamSd), TeamRk=as.numeric(TeamRk), TeamConf=as.factor(TeamConf),
           TeamO=as.numeric(TeamO), TeamD=as.numeric(TeamD), TeamT=as.numeric(TeamT),
           TeamL=as.numeric(TeamL), TeamOO=as.numeric(TeamOO), TeamOD=as.numeric(TeamOD),
           TeamNCSOS=as.numeric(TeamNCSOS), OppSd=as.numeric(OppSd), OppRk=as.numeric(OppRk), 
           OppConf=as.factor(OppConf), OppO=as.numeric(OppO), OppD=as.numeric(OppD), 
           OppT=as.numeric(OppT), OppL=as.numeric(OppL), OppOO=as.numeric(OppOO), 
           OppOD=as.numeric(OppOD), OppNCSOS=as.numeric(OppNCSOS))
  newdata_df <- left_join(newdata_df, record, by=c('Team'='team'))
  newdata_df <- left_join(newdata_df, record, by=c('Opp'='team'), suffix=c('_team', '_opp'))
  newdata_df$ct_team[which(is.na(newdata_df$ct_team)), ] <- 0
  newdata_df <- newdata_df %>% mutate(RecMargin=ct_team/(ct_team+ct_opp))
  
  newdata_mat_outcome <- as.matrix(
    select(newdata_df, TeamO, TeamD, TeamT, TeamOO, TeamOD, TeamNCSOS, OppO, OppD, 
           OppT, OppOO, OppOD, OppNCSOS, ct_team, ct_opp, RecMargin))
  newdata_mat_upset <- as.matrix(
    select(newdata_df, TeamSd, TeamRk, TeamO, TeamD, TeamT, TeamL, TeamOO, TeamOD, TeamNCSOS, OppSd, 
           OppRk, OppO, OppD, OppT, OppL, OppOO, OppOD, OppNCSOS, ct_team, ct_opp, RecMargin))
  newdata_mat_outcome <- newdata_mat_outcome[!is.na(apply(newdata_mat_outcome, 1, sum)),]
  newdata_mat_upset <- newdata_mat_upset[!is.na(apply(newdata_mat_upset, 1, sum)),]
  # predict from models
  outcome_pred <- cbind(prd_df[,c(1,2,3,15,16,17)],
                        round(predict(cv_outcome, newx=newdata_mat_outcome, s="lambda.min", type="response"), 2))
  upset_pred <- cbind(prd_df[,c(1,2,3,15,16,17)],
                      round(predict(cv_upset, newx=newdata_mat_upset, s="lambda.min", type="response"), 2))
  
  # create output table
  pred <- cbind(as.data.frame(outcome_pred), as.data.frame(upset_pred)[,7])
  names(pred) <- c('Team', 'Team_Region', 'Team_Seed', 'Opponent', 'Opponent_Region', 
                   'Opponent_Seed', 'Win_Prob', 'Upset_Prob')
  pred <- mutate(pred, Adj_Probability=ifelse(as.numeric(Team_Seed) > as.numeric(Opponent_Seed), 
                                              Win_Prob+Upset_Prob, Win_Prob))
  return(pred)
}


pred19 <- every_game(teams=t19, team_stats=kp19)


get_predictions <- function(pred.e.g=pred19) {
  # round 1
  rnd1_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
    pred_rg <- pred.e.g[pred.e.g$Team_Region==rg & pred.e.g$Opponent_Region==rg,]
    rnd1_list <- lapply(1:8, function(i) {
      t1 <- i
      t2 <- 16+1-i
      matchup <- cbind(pred_rg[pred_rg$Team_Seed==t1 & pred_rg$Opponent_Seed==t2, c(1,2,3,7)], 
                       pred_rg[pred_rg$Team_Seed==t2 & pred_rg$Opponent_Seed==t1, c(1,3,8)])
      names(matchup) <- c('Team_A', 'Region', 'Seed_A', 'Win_Prob', 'Team_B', 'Seed_B', 'Upset_Prob')
      matchup <- matchup %>% 
        mutate(Adj_Prob=round((Win_Prob*(1-Upset_Prob))-(1-Win_Prob), 2)) %>% 
        select(Region, Team_A, Seed_A, Team_B, Seed_B, Win_Prob, Upset_Prob, Adj_Prob)
      return(matchup)
    })
    rnd1_region_results <- data.frame(do.call('rbind', rnd1_list))
    return(rnd1_region_results)
  })
  rnd1_results <- data.frame(do.call('rbind', rnd1_results))

  # round 2
  rnd2_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
    pred_rg <- pred.e.g[pred.e.g$Team_Region==rg & pred.e.g$Opponent_Region==rg,]
    rnd1_rg <- filter(rnd1_results, Region==rg)
    rnd1_win <- ifelse(rnd1_rg$Adj_Prob > 0, rnd1_rg$Seed_A, rnd1_rg$Seed_B)
    rnd1_win_pct <- data.frame(seed=rnd1_win, pct=ifelse(rnd1_rg$Adj_Prob > 0, rnd1_rg$Win_Prob, 1-rnd1_rg$Win_Prob))
    rnd2_list <- lapply(
      list(rnd1_win[rnd1_win %in% c('1','8','9','16')], rnd1_win[rnd1_win %in% c('4','5','12','13')],
           rnd1_win[rnd1_win %in% c('3','6','11','14')], rnd1_win[rnd1_win %in% c('2','7','10','15')]), 
      function(i) {
        t1 <- as.character(sort(as.numeric(i))[1])
        t2 <- as.character(sort(as.numeric(i))[2])
        matchup <- cbind(pred_rg[pred_rg$Team_Seed==t1 & pred_rg$Opponent_Seed==t2, c(1,2,3,7)], 
                         rnd1_win_pct[rnd1_win_pct$seed==t1, 2],
                         pred_rg[pred_rg$Team_Seed==t2 & pred_rg$Opponent_Seed==t1, c(1,3,8)],
                         rnd1_win_pct[rnd1_win_pct$seed==t2, 2])
        names(matchup) <- c('Team_A', 'Region', 'Seed_A', 'Win_Prob', 'A_Win_Prob_Rnd1', 
                            'Team_B', 'Seed_B', 'Upset_Prob', 'B_Win_Prob_Rnd1')
        matchup <- matchup %>% 
          mutate(TeamA_Cum_Prob=Win_Prob*A_Win_Prob_Rnd1,
                 TeamB_Cum_Prob=((1-Win_Prob)*B_Win_Prob_Rnd1),
                 Adj_Prob=round(((Win_Prob*(1-Upset_Prob))*TeamA_Cum_Prob)-((1-Win_Prob)*TeamB_Cum_Prob), 2)) %>% 
          select(Region, Team_A, Seed_A, Team_B, Seed_B, Win_Prob, Upset_Prob, Adj_Prob, TeamA_Cum_Prob, TeamB_Cum_Prob)
        return(matchup)
      })
    rnd2_region_results <- data.frame(do.call('rbind', rnd2_list))
    return(rnd2_region_results)
  })
  rnd2_results <- data.frame(do.call('rbind', rnd2_results))

  # round 3
  rnd3_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
    pred_rg <- pred.e.g[pred.e.g$Team_Region==rg & pred.e.g$Opponent_Region==rg,]
    rnd1_rg <- filter(rnd1_results, Region==rg)
    rnd1_win <- ifelse(rnd1_rg$Adj_Prob > 0, rnd1_rg$Seed_A, rnd1_rg$Seed_B)
    rnd1_win_pct <- data.frame(seed=rnd1_win, pct=ifelse(rnd1_rg$Adj_Prob > 0, rnd1_rg$Win_Prob, 1-rnd1_rg$Win_Prob))
    rnd2_rg <- filter(rnd2_results, Region==rg)
    rnd2_win <- ifelse(rnd2_rg$Adj_Prob > 0, rnd2_rg$Seed_A, rnd2_rg$Seed_B)
    rnd2_win_pct <- data.frame(seed=rnd2_win, pct=ifelse(rnd2_rg$Adj_Prob > 0, rnd2_rg$Win_Prob, 1-rnd2_rg$Win_Prob))
    rnd3_list <- lapply(
      list(rnd2_win[rnd2_win %in% c('1','4','5','8','9','12','13','16')],
           rnd2_win[rnd2_win %in% c('2','3','6','7','10','11','14','15')]), 
      function(i) {
        t1 <- as.character(sort(as.numeric(i))[1])
        t2 <- as.character(sort(as.numeric(i))[2])
        matchup <- cbind(pred_rg[pred_rg$Team_Seed==t1 & pred_rg$Opponent_Seed==t2, c(1,2,3,7)],
                         rnd1_win_pct[rnd1_win_pct$seed==t1, 2],
                         rnd2_win_pct[rnd2_win_pct$seed==t1, 2],
                         pred_rg[pred_rg$Team_Seed==t2 & pred_rg$Opponent_Seed==t1, c(1,3,8)],
                         rnd1_win_pct[rnd1_win_pct$seed==t2, 2],
                         rnd2_win_pct[rnd2_win_pct$seed==t2, 2])
        names(matchup) <- c('Team_A', 'Region', 'Seed_A', 'Win_Prob', 'A_Win_Prob_Rnd1', 'A_Win_Prob_Rnd2', 
                            'Team_B', 'Seed_B', 'Upset_Prob', 'B_Win_Prob_Rnd1', 'B_Win_Prob_Rnd2')
        matchup <- matchup %>% 
          mutate(TeamA_Cum_Prob=Win_Prob*A_Win_Prob_Rnd1*A_Win_Prob_Rnd2,
                 TeamB_Cum_Prob=((1-Win_Prob)*B_Win_Prob_Rnd1*B_Win_Prob_Rnd2),
                 Adj_Prob=round(((Win_Prob*(1-Upset_Prob))*TeamA_Cum_Prob)-((1-Win_Prob)*TeamB_Cum_Prob), 2)) %>% 
          select(Region, Team_A, Seed_A, Team_B, Seed_B, Win_Prob, Upset_Prob, Adj_Prob, TeamA_Cum_Prob, TeamB_Cum_Prob)
        return(matchup)
      })
    rnd3_region_results <- data.frame(do.call('rbind', rnd3_list))
    return(rnd3_region_results)
  })
  rnd3_results <- data.frame(do.call('rbind', rnd3_results))
 
  # round 4
  rnd4_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
    pred_rg <- pred.e.g[pred.e.g$Team_Region==rg & pred.e.g$Opponent_Region==rg,]
    rnd1_rg <- filter(rnd1_results, Region==rg)
    rnd1_win <- ifelse(rnd1_rg$Adj_Prob > 0, rnd1_rg$Seed_A, rnd1_rg$Seed_B)
    rnd1_win_pct <- data.frame(seed=rnd1_win, pct=ifelse(rnd1_rg$Adj_Prob > 0, rnd1_rg$Win_Prob, 1-rnd1_rg$Win_Prob))
    rnd2_rg <- filter(rnd2_results, Region==rg)
    rnd2_win <- ifelse(rnd2_rg$Adj_Prob > 0, rnd2_rg$Seed_A, rnd2_rg$Seed_B)
    rnd2_win_pct <- data.frame(seed=rnd2_win, pct=ifelse(rnd2_rg$Adj_Prob > 0, rnd2_rg$Win_Prob, 1-rnd2_rg$Win_Prob))
    rnd3_rg <- filter(rnd3_results, Region==rg)
    rnd3_win <- ifelse(rnd3_rg$Adj_Prob > 0, rnd3_rg$Seed_A, rnd3_rg$Seed_B)
    rnd3_win_pct <- data.frame(seed=rnd3_win, pct=ifelse(rnd3_rg$Adj_Prob > 0, rnd3_rg$Win_Prob, 1-rnd3_rg$Win_Prob))
    rnd4_teams <- rnd3_win[rnd3_win %in% c('1','2','3','4','5','6','7','8',
                                           '9','10','11','12','13','14','15','16')] 
    t1 <- as.character(sort(as.numeric(rnd4_teams))[1])
    t2 <- as.character(sort(as.numeric(rnd4_teams))[2])
    matchup <- cbind(pred_rg[pred_rg$Team_Seed==t1 & pred_rg$Opponent_Seed==t2, c(1,2,3,7)], 
                     rnd1_win_pct[rnd1_win_pct$seed==t1, 2],
                     rnd2_win_pct[rnd2_win_pct$seed==t1, 2],
                     rnd3_win_pct[rnd3_win_pct$seed==t1, 2],
                     pred_rg[pred_rg$Team_Seed==t2 & pred_rg$Opponent_Seed==t1, c(1,3,8)],
                     rnd1_win_pct[rnd1_win_pct$seed==t2, 2],
                     rnd2_win_pct[rnd2_win_pct$seed==t2, 2],
                     rnd3_win_pct[rnd3_win_pct$seed==t2, 2])
    names(matchup) <- c('Team_A', 'Region', 'Seed_A', 'Win_Prob', 'A_Win_Prob_Rnd1', 'A_Win_Prob_Rnd2', 'A_Win_Prob_Rnd3',
                        'Team_B', 'Seed_B', 'Upset_Prob', 'B_Win_Prob_Rnd1', 'B_Win_Prob_Rnd2', 'B_Win_Prob_Rnd3')
    matchup <- matchup %>% 
      mutate(TeamA_Cum_Prob=Win_Prob*A_Win_Prob_Rnd1*A_Win_Prob_Rnd2*A_Win_Prob_Rnd3,
             TeamB_Cum_Prob=((1-Win_Prob)*B_Win_Prob_Rnd1*B_Win_Prob_Rnd2*B_Win_Prob_Rnd3),
             Adj_Prob=round(((Win_Prob*(1-Upset_Prob))*TeamA_Cum_Prob)-((1-Win_Prob)*TeamA_Cum_Prob), 2)) %>%
      select(Region, Team_A, Seed_A, Team_B, Seed_B, Win_Prob, Upset_Prob, Adj_Prob, TeamA_Cum_Prob, TeamB_Cum_Prob)
    return(matchup)
  })
  rnd4_results <- data.frame(do.call('rbind', rnd4_results))
  
  rnd1_results <<- rnd1_results %>% 
    mutate(Adj_Prob=ifelse(Adj_Prob >0, paste0('+',Adj_Prob), paste0(Adj_Prob)))
  rnd2_results <<- rnd2_results %>% 
    mutate(Adj_Prob=ifelse(Adj_Prob >0, paste0('+',Adj_Prob), paste0(Adj_Prob)))
  rnd3_results <<- rnd3_results %>% 
    mutate(Adj_Prob=ifelse(Adj_Prob >0, paste0('+',Adj_Prob), paste0(Adj_Prob)))
  rnd4_results <<- rnd4_results %>% 
    mutate(Adj_Prob=ifelse(Adj_Prob >0, paste0('+',Adj_Prob), paste0(Adj_Prob)))
}
      


# get_predictions(pred.e.g=pred18)
get_predictions(pred.e.g=pred19)




# for (i in 1:nrow(g19)) {
#   t1 <- g19[i,2]
#   t2 <- g19[i,5]
#   w1 <- round(as.numeric(paste(pred[pred$Team==t1, 2])), 2)
#   w2 <- round(as.numeric(paste(pred[pred$Team==t2, 2])), 2)
#   u1 <- round(as.numeric(paste(pred[pred$Team==t1, 3])), 2)
#   u2 <- round(as.numeric(paste(pred[pred$Team==t2, 3])), 2)
#   print(paste(t1,'vs',t2,', probability:',w1,'to',w2,', upset likelihood:',u1,'to',u2))
# }


rnd1 <- get_predictions(
  x=c('Duke', 'VCU', 'Mississippi St.', 'Virginia Tech', 'Maryland', 'LSU', 'Louisville', 'Michigan St.',
      'Gonzaga', 'Syracuse', 'Marquette', 'Florida St.', 'Buffalo', 'Texas Tech', 'Nevada', 'Michigan',
      'Virginia', 'Mississippi', 'Wisconsin', 'Kansas St.', 'Villanova', 'Purdue', 'Cincinnati', 'Tennessee',
      'North Carolina', 'Utah St.', 'Auburn', 'Kansas', 'Iowa St.', 'Houston', 'Wofford', 'Kentucky'), 
  y=c('North Dakota St.', 'UCF', 'Liberty', 'Saint Louis', 'Belmont', 'Yale', 'Minnesota', 'Bradley',
      'Fairleigh Dickinson', 'Baylor', 'Murray St.', 'Vermont', 'Arizona St.', 'Northern Kentucky', 'Florida', 'Montana',
      'Gardner Webb', 'Oklahoma', 'Oregon', 'UC Irvine', "Saint Mary's", 'Old Dominion', 'Iowa', 'Colgate',
      'Iona', 'Washington', 'New Mexico St.', 'Northeastern', 'Ohio St.', 'Georgia St.', 'Seton Hall', 'Abilene Christian'))

rnd2 <- get_predictions(
  x=c('Duke', 'Mississippi St.', 'Maryland', 'Minnesota', 'Gonzaga', 'Murray St.', 'Buffalo', 'Florida',
      'Virginia', 'Oregon', 'Villanova', 'Iowa', 'North Carolina', 'Auburn', 'Ohio St.', 'Wofford'), 
  y=c('UCF', 'Virginia Tech', 'LSU', 'Michigan St.', 'Baylor', 'Florida St.', 'Texas Tech', 'Michigan',
      'Oklahoma', 'UC Irvine', 'Purdue', 'Tennessee', 'Washington', 'Kansas', 'Houston', 'Kentucky'))

rnd3 <- get_predictions(
  x=c('Duke', 'Maryland', 'Gonzaga', 'Texas Tech', 'Virginia', 'Purdue', 'North Carolina', 'Ohio St.'), 
  y=c('Virginia Tech', 'Michigan St.', 'Florida St.', 'Florida', 'Kansas St.', 'Iowa', 'Kansas', 'Kentucky'))

rnd4 <- get_predictions(
  x=c('Duke', 'Gonzaga', 'Virginia', 'North Carolina'), 
  y=c('Michigan St.', 'Texas Tech', 'Iowa', 'Kentucky'))

rnd5 <- get_predictions(
  x=c('Duke', 'Virginia'), 
  y=c('Texas Tech', 'Kentucky'))

final <- get_predictions(
  x=c('Duke'), 
  y=c('Virginia'))



get_predictions(x='Virginia', y='Kentucky')




for (i in 1:8) {
  ind <- c(i, 16+1-i)
  print(ind)
}



ff_chance <- do.call(
  'rbind', lapply(1:nrow(t19), function(i) {
    tot_win <- pred19 %>% 
      filter(Opponent_Region==t19[i,2] & Team==t19[i,1]) %>% 
      group_by(Team, Team_Region) %>% 
      summarise(total_win=sum(Win_Prob)) %>% 
      mutate(adj_ff_win=round(total_win/15,2))
    return(tot_win)
  }))
