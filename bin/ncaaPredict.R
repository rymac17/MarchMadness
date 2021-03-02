
# table with all possible matchups ----
library(xlsx)
library(glmnet)
options(scipen=999)
source('src/ncaaHelpers.R')
cv_outcome <- readRDS('data/models/cv_outcome.rds')
# cv_upset <- readRDS('data/models/cv_upset.rds')
statsTBL <- read.csv('data/statsTBL.csv')
masterTBL <- read.csv('data/masterTBL.csv')

# run once and save
# load teams and make sure names match
teams <- read.xlsx(file='data/teams2019.xlsx', sheetName='Sheet1')
teams[which(!teams$Team %in% c(masterTBL$team,masterTBL$team_2)),]

# run all games
LUT <- expand.grid(team1=teams$Team, team2=teams$Team, stringsAsFactors=F)
LUT[,'winP'] <- lapply(1:nrow(LUT), function(x){
  SIMgame(t1=LUT[x,1], t2=LUT[x,2], yr=2019, pred_wins=T)
  # SIMprob(t1=LUT[x,1], t2=LUT[x,2], yr=2018, pred_wins=T, bootlen=100)
}) %>% do.call('rbind', .)
# LUT[,'upsetP'] <- lapply(1:nrow(LUT), function(x){
#   SIMgame(t1=LUT[x,1], t2=LUT[x,2], yr=2019, pred_wins=F)
#   # SIMprob(t1=LUT[x,1], t2=LUT[x,2], yr=2018, pred_wins=F, bootlen=100)
# }) %>% do.call('rbind', .)
# save
write.csv(LUT, 'data/LUT2019.csv', row.names=F)


# predict 2019 data
rm(list=ls())
library(tidyverse)
library(xlsx)
options(scipen=999)
source('src/ncaaHelpers.R')
# teams <- read.xlsx(file='data/teams2018.xlsx', sheetName='Sheet1')
teams <- read.xlsx(file='data/teams2019.xlsx', sheetName='Sheet1')
cv_outcome <- readRDS('data/models/cv_outcome.rds')
# cv_upset <- readRDS('data/models/cv_upset.rds')
statsTBL <- read.csv('data/statsTBL.csv')
masterTBL <- read.csv('data/masterTBL.csv')
LUT <- read.csv('data/LUT2019.csv')
# LUT <- read.csv('data/LUT2.csv')


# boot <- sapply(1:100, function(x){
#   runRND(teams, 1) %>% 
#     runRND(., 2) %>% 
#     runRND(., 3) %>% 
#     runRND(., 4) %>% 
#     runRND(., 5) %>% 
#     runRND(., 6) %>% 
#     pull(Team)
# })

# sapply(unique(boot), function(x) length(which(boot==x)))


# perfect score 192
32+(16*2)+(8*4)+(4*8)+(2*16)+(1*32)

obsDFT <- masterTBL %>% 
  filter(year==2019) %>% 
  mutate(winner=ifelse(score>score_2, team, team_2)) %>% 
  dplyr::select(round, region_name, winner)

boot <- sapply(1:500, function(x){
  r <- teams
  predDFT <- data.frame()
  for (i in 1:6){
    r <- runRND(r, i)
    predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_name=Region, winner=Team))
  }
  j <- inner_join(obsDFT, predDFT, by=c('round','region_name','winner')) %>% 
    mutate(RNDpts=case_when(round==1~1, round==2~2, round==3~4, round==4~8, 
                            round==5~16, round==6~32)) %>% 
    summarise(pts=sum(RNDpts), pct=pts/192)
  return(j %>% pull(pts))
})

hist(boot)
max(boot)
summary(boot)
#

SIMprob('Baylor', 'Kansas', 2021)
SIMprob('Gonzaga', 'Baylor', 2021)
SIMprob('Baylor', 'Michigan', 2021)
SIMprob('Virginia', 'Duke', 2019)

cv_outcome
#









SIMprob <- function(t1, t2, yr, pred_wins=T, bootlen=100){
  s <- sample(x=c(t1,t2), size=bootlen, replace=T,
              prob=c(SIMgame(t1=t1, t2=t2, yr=yr, pred_wins=pred_wins), 
                     SIMgame(t1=t2, t2=t1, yr=yr, pred_wins=pred_wins)))
  s2 <- sapply(unique(s), function(x) length(which(s==x)))
  out <- ifelse(is.na(s2[t1]), 0, s2[t1]/bootlen) # account for no wins
  return(out)
}


SIMprob(t1='North Dakota St', t2='Duke', yr=2019, pred_wins=T, bootlen=100)
SIMprob(t1='Duke', t2='Michigan St', yr=2019, pred_wins=F, bootlen=500)


str(test2)
rnd=5
rg='West'
i <- teams %>% filter(Region==rg)
rndNM <- sym(paste0('RND',rnd))

lapply(i %>% pull(rndNM) %>% unique(), function(x){
  highSD <- i %>% filter(RND1==x) %>% slice_min(Seed)
  lowSD <- i %>% filter(RND1==x) %>% slice_max(Seed)
  matchup(tbl1=highSD, tbl2=lowSD)
}) %>% 
  do.call('rbind',.)
}  
rnd1_region <- lapply(1:8, function(i) {
  t1 <- teams %>% filter(Region==rg & Seed==i) %>% pull(Team)
  t2 <- teams %>% filter(Region==rg & Seed==(16+1-i)) %>% pull(Team)
  # look at games each way
  match
  matchup <- possibleGames %>% filter(team1==t1, team2==t2) %>% 
    left_join(possibleGames %>% filter(team1==t2, team2==t1),
              by=c('team1'='team2', 'team2'='team1')) %>%
    mutate(AdjwinP=round((winP.x*(1-upsetP.x))-(winP.y), 2),
           Advance=ifelse(AdjwinP>0, 1, 2),
           CumWinPct=ifelse(AdjwinP>0, winP.x, winP.y),
           Region=rg, 
           seed1=i,
           seed2=16+1-i)
  return(matchup)
}) %>% 
  do.call('rbind', .)
return(rnd1_region)
}) %>% 
  do.call('rbind',.)

# round 1
rnd1_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
  i <- teams %>% filter(Region==rg)
  lapply(i %>% pull(RND1) %>% unique(), function(j){
    print(j)
    highSD <- i %>% filter(RND1==j) %>% slice_min(Seed)
    lowSD <- i %>% filter(RND1==j) %>% slice_max(Seed)
    matchup(tbl1=highSD, tbl2=lowSD)
  }) %>% 
    do.call('rbind',.)
  
  # round 2
  rnd2_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
    rnd1_win <- rnd1_results %>% 
      filter(Region==rg) %>% 
      mutate(Team=ifelse(Advance==1, team1, team2),
             Seed=ifelse(Advance==1, seed1, seed2),
             CumWinPct=CumWinPct,
             .keep='none') %>% 
      left_join(., teams, by=c('Team','Seed'))
    rnd2_region <- lapply(unique(rnd1_win$Group2), function(i){
      t1 <- rnd1_win %>% filter(Group2==i) %>% slice_min(Seed)
      t2 <- rnd1_win %>% filter(Group2==i) %>% slice_max(Seed)
      # look at games each way
      matchup <- possibleGames %>% filter(team1==t1[[1]][1], team2==t2[[1]][1]) %>% 
        left_join(possibleGames %>% filter(team1==t2[[1]][1], team2==t1[[1]][1]),
                  by=c('team1'='team2', 'team2'='team1')) %>%
        mutate(AdjwinP=round((winP.x*(1-upsetP.x))-(winP.y), 2),
               Advance=ifelse(AdjwinP>0, 1, 2),
               CumWinPct=ifelse(AdjwinP>0, winP.x, winP.y),
               Region=rg, 
               seed1=t1[[2]][1],
               seed2=t2[[2]][1])
      return(matchup)
    }) %>% 
      do.call('rbind',.)
    return(rnd2_region)
  }) %>% 
    do.call('rbind',.)
  
  # round 3
  rnd3_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
    rnd2_win <- rnd2_results %>% 
      filter(Region==rg) %>% 
      mutate(Team=ifelse(Advance==1, team1, team2),
             Seed=ifelse(Advance==1, seed1, seed2),
             CumWinPct=CumWinPct,
             .keep='none') %>% 
      left_join(., teams, by=c('Team','Seed'))
    rnd3_region <- lapply(unique(rnd2_win$Group3), function(i){
      t1 <- rnd2_win %>% filter(Group3==i) %>% slice_min(Seed)
      t2 <- rnd2_win %>% filter(Group3==i) %>% slice_max(Seed)
      # look at games each way
      matchup <- possibleGames %>% filter(team1==t1[[1]][1], team2==t2[[1]][1]) %>% 
        left_join(possibleGames %>% filter(team1==t2[[1]][1], team2==t1[[1]][1]),
                  by=c('team1'='team2', 'team2'='team1')) %>%
        mutate(AdjwinP=round((winP.x*(1-upsetP.x))-(winP.y), 2),
               Advance=ifelse(AdjwinP>0, 1, 2),
               CumWinPct=ifelse(AdjwinP>0, winP.x, winP.y),
               Region=rg, 
               seed1=t1[[2]][1],
               seed2=t2[[2]][1])
      return(matchup)
    }) %>% 
      do.call('rbind',.)
    return(rnd3_region)
  }) %>% 
    do.call('rbind',.)
  
  # round 3
  rnd4_results <- lapply(c('East', 'West', 'South', 'Midwest'), function(rg) {
    rnd3_win <- rnd3_results %>% 
      filter(Region==rg) %>% 
      mutate(Team=ifelse(Advance==1, team1, team2),
             Seed=ifelse(Advance==1, seed1, seed2),
             CumWinPct=CumWinPct,
             .keep='none') %>% 
      left_join(., teams, by=c('Team','Seed'))
    rnd4_region <- lapply(rg, function(i){
      t1 <- rnd3_win %>% slice_min(Seed)
      t2 <- rnd3_win %>% slice_max(Seed)
      # look at games each way
      matchup <- possibleGames %>% filter(team1==t1[[1]][1], team2==t2[[1]][1]) %>% 
        left_join(possibleGames %>% filter(team1==t2[[1]][1], team2==t1[[1]][1]),
                  by=c('team1'='team2', 'team2'='team1')) %>%
        mutate(AdjwinP=round((winP.x*(1-upsetP.x))-(winP.y), 2),
               Advance=ifelse(AdjwinP>0, 1, 2),
               CumWinPct=ifelse(AdjwinP>0, winP.x, winP.y),
               Region=rg, 
               seed1=t1[[2]][1],
               seed2=t2[[2]][1])
      return(matchup)
    }) %>% 
      do.call('rbind',.)
    return(rnd4_region)
  }) %>% 
    do.call('rbind',.)
  
  
  
  # # test set
  # every_game <- function(yr=2019) {
  #   require(tidyr)
  #   require(dplyr)
  #   library(xlsx)
  #   yr_ch <- as.character(yr)
  #   team_stats <- filter(statsTBL, year==2019)
  #   teams <- read.xlsx(file='data/teams2019.xlsx', sheetName='Sheet1')
  #   # get every possible matchup
  #   possible_games <- expand.grid(cbind(unite(teams, col='id', sep='_'), unite(teams, col='id', sep='_')))
  #   names(possible_games) <- c('team1', 'team2')
  #   possible_games <- possible_games %>% 
  #     separate(col='team1', into=c('team1', 'region1', 'seed1'), sep='_') %>% 
  #     separate(col='team2', into=c('team2', 'region2', 'seed2'), sep='_') %>% 
  #     filter(!(team1==team2))
  #   # get stats for each team in every matchup
  #   prd_df <- left_join(possible_games, team_stats, by=c('team1'='Team'))
  #   prd_df <- left_join(prd_df, team_stats, by=c('team2'='Team'))
  #   nm <- c('Team', 'TeamRg', 'TeamSd', 'Opp', 'OppRg', 'OppSd', 'TeamRk', 'TeamConf', 'TeamWL',
  #           'TeamE', 'TeamO', 'TeamD', 'TeamT', 'TeamL', 'TeamSOS', 'TeamOO', 'TeamOD', 'TeamNCSOS',
  #           'OppRk', 'OppConf', 'OppWL', 'OppE', 'OppO', 'OppD', 'OppT', 'OppL', 'OppSOS', 'OppOO', 
  #           'OppOD', 'OppNCSOS')
  #   names(prd_df) <- nm
  #   # set up matrix for regularized regression prediction
  #   newdata_df <- prd_df %>% 
  #     mutate(TeamSd=as.numeric(TeamSd), TeamRk=as.numeric(TeamRk), TeamConf=as.factor(TeamConf),
  #            TeamO=as.numeric(TeamO), TeamD=as.numeric(TeamD), TeamT=as.numeric(TeamT),
  #            TeamL=as.numeric(TeamL), TeamOO=as.numeric(TeamOO), TeamOD=as.numeric(TeamOD),
  #            TeamNCSOS=as.numeric(TeamNCSOS), OppSd=as.numeric(OppSd), OppRk=as.numeric(OppRk), 
  #            OppConf=as.factor(OppConf), OppO=as.numeric(OppO), OppD=as.numeric(OppD), 
  #            OppT=as.numeric(OppT), OppL=as.numeric(OppL), OppOO=as.numeric(OppOO), 
  #            OppOD=as.numeric(OppOD), OppNCSOS=as.numeric(OppNCSOS))
  #   newdata_df <- left_join(newdata_df, record, by=c('Team'='team'))
  #   newdata_df <- left_join(newdata_df, record, by=c('Opp'='team'), suffix=c('_team', '_opp'))
  #   newdata_df$ct_team[which(is.na(newdata_df$ct_team)), ] <- 0
  #   newdata_df <- newdata_df %>% mutate(RecMargin=ct_team/(ct_team+ct_opp))
  #   
  #   newdata_mat_outcome <- as.matrix(
  #     select(newdata_df, TeamO, TeamD, TeamT, TeamOO, TeamOD, TeamNCSOS, OppO, OppD, 
  #            OppT, OppOO, OppOD, OppNCSOS, ct_team, ct_opp, RecMargin))
  #   newdata_mat_upset <- as.matrix(
  #     select(newdata_df, TeamSd, TeamRk, TeamO, TeamD, TeamT, TeamL, TeamOO, TeamOD, TeamNCSOS, OppSd, 
  #            OppRk, OppO, OppD, OppT, OppL, OppOO, OppOD, OppNCSOS, ct_team, ct_opp, RecMargin))
  #   newdata_mat_outcome <- newdata_mat_outcome[!is.na(apply(newdata_mat_outcome, 1, sum)),]
  #   newdata_mat_upset <- newdata_mat_upset[!is.na(apply(newdata_mat_upset, 1, sum)),]
  #   # predict from models
  #   outcome_pred <- cbind(prd_df[,c(1,2,3,15,16,17)],
  #                         round(predict(cv_outcome, newx=newdata_mat_outcome, s="lambda.min", type="response"), 2))
  #   upset_pred <- cbind(prd_df[,c(1,2,3,15,16,17)],
  #                       round(predict(cv_upset, newx=newdata_mat_upset, s="lambda.min", type="response"), 2))
  #   
  #   # create output table
  #   pred <- cbind(as.data.frame(outcome_pred), as.data.frame(upset_pred)[,7])
  #   names(pred) <- c('Team', 'Team_Region', 'Team_Seed', 'Opponent', 'Opponent_Region', 
  #                    'Opponent_Seed', 'Win_Prob', 'Upset_Prob')
  #   pred <- mutate(pred, Adj_Probability=ifelse(as.numeric(Team_Seed) > as.numeric(Opponent_Seed), 
  #                                               Win_Prob+Upset_Prob, Win_Prob))
  #   return(pred)
  # }
  # 
  # 
  # pred19 <- every_game(teams=t19, team_stats=kp19)
  
  
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
  