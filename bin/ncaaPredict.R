
options(scipen=999)
library(dplyr)
library(glmnet)
source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('data/masterTBL.csv')
statsTBL <- read.csv('data/statsTBL.csv')

# model
modelTBL <- filter(masterTBL, year!=2021)
x <- as.matrix(select(modelTBL, AdjEM, AdjO, AdjD, AdjT, OppO, OppD,
                      AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2,
                      Losses, Losses_2))
y <- as.matrix(select(modelTBL, outcome))
set.seed(1011)
cv_outcome <<- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=10, alpha=1)
coef(cv_outcome, s = "lambda.min")
saveRDS(cv_outcome, 'data/model2021/cv_outcome.rds')





options(scipen=999)
library(gridExtra)
library(dplyr)
library(glmnet)
library(xlsx)
source('src/ncaaHelpers.R')
# read model
cv_outcome <- readRDS('data/model2021/cv_outcome.rds')
# read tables
masterTBL <- read.csv('data/masterTBL.csv')
statsTBL <- read.csv('data/statsTBL.csv')
# hyperparameters
yr <<- 2021
sampleSize <<- 100


teams <- read.xlsx('data/teams2021_030221.xlsx', sheetName='Sheet1')
teams[which(!teams$Team %in% (statsTBL %>% filter(year==yr) %>% pull(Team))),]

r <- mkBracket(use_historic=F)
predDFT <- data.frame()
for (i in 1:6){
  r <- runRND(r, i)
  predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
}

BRKT <- assignBRKT()

tiff(filename='data/test2019.tif', width=16, height=26, units='in', res=200)
grid.table(BRKT)
dev.off()

pdf(file='data/test2019.pdf', width=20, height=35)
grid.table(BRKT)
dev.off()


SIMgame(tbl1='Illinois', tbl2='Michigan', SS=2, alacarte=T)


stp=F
i=0
while (stp==F){
  r <- mkBracket(use_historic=F)
  predDFT <- data.frame()
  for (i in 1:6){
    r <- runRND(r, i)
    predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
  }
  w <- predDFT %>% filter(round==6) %>% pull(winner)
  i <- i+1
  stp <- w!='Gonzaga' & w!='Michigan'
}
