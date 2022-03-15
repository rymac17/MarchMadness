
library(dplyr)

# get ncaaHelper functions
source('src/ncaaHelpers.R')

# # download stats
# lapply(2002:2021, FUN=scrape_kenpom)

# update
scrape_kenpom(2021)
scrape_kenpom(2022)


# load stats
stats <- do.call('rbind',
                 lapply(list.files('data/kenpom', '.csv$', full.names=T), FUN=read.csv))
write.csv(stats, 'C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv', row.names=F)

# load scores - from https://data.world/michaelaroy/ncaa-tournament-results
scores <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/past_results/big_dance_csv.csv') %>% 
  filter(year %in% 2002:2021)
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
         upset=ifelse(seed<seed_2 & outcome==0, 1, 0), # 1 upset / 0 no upset
         margin=score/(score+score_2))
write.csv(masterTBL, 'C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv', row.names=F)

historicalStats <- masterTBL %>% 
  group_by(seed, seed_2) %>% 
  summarise(pctUpsets=round(sum(upset)/n(),2), 
            events=n(),
            upsets=sum(upset))
