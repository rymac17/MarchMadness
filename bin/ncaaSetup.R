
library(dplyr)

# get ncaaHelper functions
source('src/ncaaHelpers.R')

# # download stats
# lapply(2002:2021, FUN=scrape_kenpom)

# update
scrape_kenpom(2022)
scrape_kenpom(2023)
scrape_kenpom(2024)
scrape_kenpom(2025)

# scrape_kenpom(2022, date=032122)


# load stats
stats <- do.call('rbind',
                 lapply(list.files('C:/Users/ryanm/Dropbox/R/MarchMadness_data/kenpom', '.csv$', full.names=T), FUN=read.csv))
stats[which(stats$Team=='Louisiana'), 'Team'] <- 'Louisiana Lafayette' # fix issue with names
write.csv(stats, 'C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv', row.names=F)

# load scores - from https://data.world/michaelaroy/ncaa-tournament-results (1985-2019)
scores <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/past_results/big_dance_csv.csv') %>% 
  filter(year %in% 2002:2024)
scores[which(scores$team_2=='Cal Irvine'), 'team_2'] <- 'UC Irvine' # fix issue with names
# arrange scores so higher seed is first
scores <- rbind(scores %>% filter(seed<=seed_2),
      scores %>% 
        filter(seed>seed_2) %>% 
        rename(seed=seed_2, score=score_2, team=team_2, team_2=team, score_2=score, seed_2=seed))

# test to see if there are any names that didn't match
# if so fix in ncaaHelpers.R
left_join(scores, stats, by=c('team'='Team','year'='year')) %>% 
  filter(is.na(Conf)) %>% pull(team) %>% unique()
left_join(scores, stats, by=c('team_2'='Team','year'='year')) %>% 
  filter(is.na(Conf)) %>% pull(team_2) %>% unique()

# join
masterTBL <- left_join(scores, stats, by=c('team'='Team','year'='year')) %>% 
  left_join(., stats, by=c('team_2'='Team','year'='year'), suffix=c('','_2')) %>% 
  mutate(GameT=AdjT-AdjT_2,
         GameO=AdjO-AdjD_2,
         GameD=AdjD-AdjO_2,
         outcome=ifelse(score>score_2, 1, 0), # 1 win / 0 loss
         upset=ifelse(seed+5<=seed_2 & outcome==0, 1, 0), # 1 upset / 0 no upset (5 seeds lower)
         margin=score/(score+score_2))
write.csv(masterTBL, 'C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv', row.names=F)




### check stats
library(dplyr)
statsTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv')
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv')
teamsTBL <- openxlsx::read.xlsx('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams2024.xlsx', sheet='Sheet1')

# t <- 'Houston'
# left_join(
#   masterTBL %>% 
#     filter(team==t | team_2==t) %>% 
#     mutate(TEAM = t, YEAR = year,
#            SEED = case_when(team==t ~ seed, team_2==t ~ seed_2),
#            WIN = case_when(team==t ~ outcome, team_2==t ~ +!outcome),
#            .keep='none') %>% 
#     group_by(YEAR) %>% 
#     summarise(TEAM = first(TEAM),
#               SEED = first(SEED),
#               WINS = sum(WIN),
#               FF = ifelse(WINS>=4, T, F),
#               NC = ifelse(WINS==6, T, F)),
#   statsTBL %>% filter(Team==t),
#   by=c('YEAR'='year')
# ) %>% 
#   left_join(., avgSeedLUT %>% dplyr::select(SEED, AVG), by='SEED') %>% 
#   mutate(plusMinus=AdjEM-AVG)
# 
# 
# 
# historicalStats <- masterTBL %>% 
#   filter(year!=2023) %>% 
#   group_by(seed, seed_2) %>% 
#   summarise(pctUpsets=round(sum(upset)/n(),2), 
#             events=n(),
#             upsets=sum(upset))
# 
# historicalStats %>% group_by(seed_2) %>% summarise(cumprob=sum(pctUpsets)) %>% arrange(cumprob)
# 
# 
# average AdjEM for seed
avgSeedLUT <- rbind(masterTBL %>% dplyr::select(SEED=seed, KP=AdjEM), masterTBL %>% dplyr::select(SEED=seed_2, KP=AdjEM_2)) %>%
  group_by(SEED) %>%
  summarise(MIN=quantile(KP, probs=0),
            Q1=quantile(KP, probs=.25),
            MED=quantile(KP, probs=.5),
            AVG=mean(KP),
            Q3=quantile(KP, probs=.75),
            MAX=quantile(KP, probs=1))

# which teams are better than an average of that seed?
seedTest <- left_join(teamsTBL, statsTBL %>% filter(year==2024), by='Team') %>%
  left_join(., avgSeedLUT, by=c('Seed'='SEED')) %>%
  mutate(plusMinus=AdjEM-AVG) %>%
  arrange(desc(plusMinus))

df <- rbind(masterTBL %>% dplyr::select(seed, Rk, AdjEM, AdjO, AdjD, AdjT, Luck, AdjEM.1, OppO, OppD, Wins, Losses, OppEM),
            masterTBL %>% dplyr::select(seed=seed_2, Rk=Rk_2, AdjEM=AdjEM_2, AdjO=AdjO_2, AdjD=AdjD_2, AdjT=AdjT_2, Luck=Luck_2,
                                        AdjEM.1=AdjEM.1_2, OppO=OppO_2, OppD=OppD_2, Wins=Wins_2, Losses=Losses_2, OppEM=OppEM_2)) %>%
  mutate(seed=factor(seed))

library(randomForest)
forest <- randomForest(seed ~ ., data=df, importance=T, proximity=T)
print(forest)

seedTest['rf_seed'] <- predict(forest, seedTest)
seedTBL <- seedTest %>%
  dplyr::select(Team, Seed, rf_seed, plusMinus) %>%
  mutate(reseedMargin=as.numeric(Seed)-as.numeric(rf_seed)) %>%
  arrange(desc(plusMinus))
