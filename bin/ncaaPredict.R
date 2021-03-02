
options(scipen=999)
library(gridExtra)
source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('data/masterTBL.csv')
statsTBL <- read.csv('data/statsTBL.csv')

# model
yr <<- 2019
modelTBL <- filter(masterTBL, year!=2021)
x <- as.matrix(select(modelTBL, AdjEM, AdjO, AdjD, AdjT, OppO, OppD,
                      AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2,
                      Losses, Losses_2))
y <- as.matrix(select(modelTBL, outcome))
set.seed(1011)
cv_outcome <<- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=10, alpha=1)
coef(cv_outcome, s = "lambda.min")






options(scipen=999)
library(gridExtra)
source('src/ncaaHelpers.R')
# read tables
masterTBL <- read.csv('data/masterTBL.csv')
statsTBL <- read.csv('data/statsTBL.csv')

teams <- mkBracket(2019)
modelTBL <- filter(masterTBL, year==2019)

sampleSize <<- 100
yr <- 2019

r <- teams
predDFT <- data.frame()
for (i in 1:6){
  r <- runRND(r, i)
  predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed))
}

BRKT <- assignBRKT()

tiff(filename='data/test.tif', width=15, height=20, units='in', res=200)
grid.table(BRKT)
dev.off()

