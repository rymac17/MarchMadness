
# make and save model
options(scipen=999)
library(dplyr)
library(randomForest)
source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv')
statsTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv')


# do blowouts in the first round lead to FF success?
dft <- full_join(
  masterTBL %>% 
    filter(round==1) %>% 
    mutate(pointDiff=abs(score-score_2), winner=ifelse(score>score_2, team, team_2),
           seed=ifelse(score>score_2, seed, seed_2)) %>% 
    dplyr::select(year, winner, seed, pointDiff),
  masterTBL %>% 
    filter(round==4) %>% 
    mutate(winner=ifelse(score>score_2, team, team_2), seed=ifelse(score>score_2, seed, seed_2), inFF=1) %>% 
    dplyr::select(year, winner, seed, inFF),
  by=c('year','winner','seed'))
dft[is.na(dft)] <- 0

forest <- randomForest(factor(inFF)~pointDiff, data=dft)
print(forest)
dft[which(predict(object=forest, newdata=dft[,c(1,3)])==1),]

dft %>% 
  filter(pointDiff>=15 & inFF==1) %>% 
  group_by(year) %>% 
  summarise(n())

dft %>% 
  filter(pointDiff>=20 & seed>=3)

# 2023 teams that won the first round by 15
# Alabama, Baylor (rd32)
# Penn St (rd32), Texas, Pitt (rd32)
# Duke (rd32), Marquette (rd32)
# Kansas (rd32), UCLA, UConn

