
library(dplyr)

# get ncaaHelper functions
source('src/ncaaHelpers.R')

# # download stats
# lapply(2002:2021, FUN=scrape_kenpom)

# update
scrape_kenpom(2021)


# load stats
stats <- do.call('rbind',
                 lapply(list.files('data/kenpom', '.csv$', full.names=T), FUN=read.csv))
write.csv(stats, 'data/statsTBL.csv', row.names=F)

# load scores - from https://data.world/michaelaroy/ncaa-tournament-results
scores <- read.csv('data/michaelaroy-ncaa-tournament-results/data/big_dance_csv.csv') %>% 
  filter(year %in% 2002:2021)
scores[which(scores$team_2=='Cal Irvine'), 'team_2'] <- 'UC Irvine' # fix issue with names
  
# test to see if there are any names that didn't match
# if so fix in ncaaHelpers.R
left_join(scores, stats, by=c('team'='Team','year'='year')) %>% 
  filter(is.na(Conf)) %>% pull(team) %>% unique()
left_join(scores, stats, by=c('team_2'='Team','year'='year')) %>% 
  filter(is.na(Conf)) %>% pull(team_2) %>% unique()

# join
masterTBL <- left_join(scores, stats, by=c('team'='Team','year'='year')) %>% 
  left_join(., stats, by=c('team_2'='Team','year'='year'), suffix=c('','_2')) %>% 
  mutate(outcome=ifelse(score>score_2, 1, 0), # 1 win / 0 loss
         upset=ifelse(seed<seed_2 & outcome==0, 1, 0), # 1 upset / 0 no upset
         margin=score/(score+score_2))
write.csv(masterTBL, 'data/masterTBL.csv', row.names=F)
