

options(scipen=999)
source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('data/masterTBL.csv')
# statsTBL <- read.csv('data/statsTBL.csv')


# loocv
test <- unique(masterTBL$year)
modelTBL <- filter(masterTBL, year!=test[1])
# model
library(glmnet)
x <- as.matrix(select(modelTBL, AdjEM, AdjO, AdjD, AdjT, OppO, OppD,
                      AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2,
                      Losses, Losses_2))
y <- as.matrix(select(modelTBL, outcome))
set.seed(1011)
cv_outcome <- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=10, alpha=1)
coef(cv_outcome, s = "lambda.min")

teams <- mkBracket(yr=test[1])

obsDFT <- masterTBL %>% 
  filter(year==test[1]) %>% 
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

