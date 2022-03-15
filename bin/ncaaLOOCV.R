

options(scipen=999)
source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('data/masterTBL.csv')
statsTBL <- read.csv('data/statsTBL.csv')

# loocv
loocvBOOT <- lapply(unique(masterTBL$year), function(y){
  library(dplyr)
  library(glmnet)
  yr <<- y
  sampleSize <<- 1
  
  # model
  modelTBL <- filter(masterTBL, year!=yr)
  x <- as.matrix(select(modelTBL, 
                        GameT, GameO, GameD,
                        AdjEM, AdjO, AdjD, AdjT, OppO, OppD, OppEM,
                        AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2, OppEM_2))
  y <- as.matrix(select(modelTBL, outcome))
  set.seed(1011)
  cv_outcome <<- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=10, alpha=1)

  teams <- mkBracket(yr)

  obsDFT <- masterTBL %>% 
    filter(year==yr) %>% 
    mutate(winner=ifelse(score>score_2, team, team_2)) %>% 
    dplyr::select(round, region_name, winner)

  if (sampleSize==0){ bootLength=1 } else { bootLength=1:500 }
  
  boot <- sapply(bootLength, function(x){
    r <- teams
    predDFT <- data.frame()
    for (i in 1:6){
      r <- runRND(r, i)
      predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
    }
    # return(predDFT %>% filter(round>=5))
    j <- inner_join(obsDFT, predDFT, by=c('round','region_name','winner')) %>%
      mutate(RNDpts=case_when(round==1~1, round==2~2, round==3~4, round==4~8,
                              round==5~16, round==6~32))
    return(j %>% summarise(pts=sum(RNDpts)) %>% pull(pts))
    # return(nrow(j)) # out of 63
  })

  return(boot)
})

names(loocvBOOT) <- unique(masterTBL$year)
saveRDS(loocvBOOT, 'data/validation/loocvBOOT_SS1.rds')


loocvBOOT <- readRDS('data/validation/loocvBOOT.rds')
loocvBOOT <- readRDS('data/validation/loocvBOOT_SS10.rds')
loocvBOOT <- readRDS('data/validation/loocvBOOT_SS100.rds')
loocvBOOT <- readRDS('data/validation/loocvBOOT_SS100_adj.rds')
loocvBOOT <- readRDS('data/validation/loocvBOOT_SS100_luck.rds')
loocvBOOT <- readRDS('data/validation/loocvBOOT_SS0.rds')
loocvBOOT <- readRDS('data/validation/loocvBOOT_SS1_num.rds')
loocvBOOT <- readRDS('data/validation/loocvBOOT_SS1.rds')

library(ggplot2)
newdft <- data.frame(v=do.call('c',loocvBOOT), year=do.call('c', lapply(names(loocvBOOT), function(x) rep(x,500))))


ggplot(data=newdft %>% filter(year %in% 2011:2018), aes(x=v, y=..density.., col=year)) +
  geom_density()
summary(newdft %>% filter(year %in% 2011:2018) %>% pull(v))
summary(newdft %>% filter(year==2015) %>% pull(v))

ggplot(data=newdft, aes(x=v, y=..density.., col=year)) +
  geom_density()
summary(newdft$v)

ggplot(data=newdft %>% filter(year==2019), aes(x=v, y=..density..)) +
  geom_density()
summary(newdft %>% filter(year==2019) %>% pull(v))

ggplot(data=newdft, aes(x=v, y=..density..)) +
  geom_density()

summary(newdft$v)

ggplot(data=newdft, aes(y=v)) +
  geom_boxplot()

