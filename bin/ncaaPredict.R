

# make and save model
options(scipen=999)
library(dplyr)
library(glmnet)
source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('data/masterTBL.csv')
statsTBL <- read.csv('data/statsTBL.csv')

# model
modelTBL <- filter(masterTBL, year!=2021)
x <- as.matrix(select(modelTBL, 
                      GameT, GameO, GameD,
                      AdjEM, AdjO, AdjD, AdjT, OppO, OppD, OppEM,
                      AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2, OppEM_2))
y <- as.matrix(select(modelTBL, outcome))
set.seed(1011)
cv_outcome <<- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=10, alpha=1)
coef(cv_outcome, s="lambda.min")
saveRDS(cv_outcome, 'data/model2021/cv_outcome.rds')




# read in 2021 teams and make predictions
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
sampleSize <<- 0

teams <- read.xlsx('data/teams2021.xlsx', sheetName='Sheet1')
teams[which(!teams$Team %in% (statsTBL %>% filter(year==yr) %>% pull(Team))),]

# first four
SIMgame(tbl1='Norfolk St', tbl2='Appalachian St', alacarte=T)
SIMgame(tbl1='Wichita St', tbl2='Drake', alacarte=T)
SIMgame(tbl1='Mount St Marys', tbl2='Texas Southern', alacarte=T)
SIMgame(tbl1='Michigan St', tbl2='UCLA', alacarte=T)


# second chance
scTeams <- c('Gonzaga','Creighton','USC','Oregon','Baylor','Villanova','Arkansas','Oral Roberts',
             'Michigan','Florida St','UCLA','Alabama','Loyola Chicago','Oregon St','Syracuse','Houston')

# # original stats
# scTBL <- expand.grid(t1=scTeams, t2=scTeams, stringsAsFactors=F) %>%
#   filter(t1!=t2)
# lapply(1:nrow(scTBL), function(x){
#   SIMgame(tbl1=scTBL[x,1], tbl2=scTBL[x,2], SS=0, alacarte=T)
# }) %>%
#   do.call('rbind', .) %>%
#   write.csv('data/scTBL_originalStats.csv')

# stats updated after 2nd round
scTBL <- expand.grid(t1=scTeams, t2=scTeams, stringsAsFactors=F) %>%
  filter(t1!=t2)
lapply(1:nrow(scTBL), function(x){
  SIMgame(tbl1=scTBL[x,1], tbl2=scTBL[x,2], SS=0, alacarte=T)
}) %>%
  do.call('rbind', .) %>%
  write.csv('data/scTBL_sweet16Stats.csv')



query <- function(t1, t2){
  oStats <- read.csv('data/scTBL_originalStats.csv')
  nStats <- read.csv('data/scTBL_sweet16Stats.csv')
  rbind(
    oStats[oStats$team_1==t1 & oStats$team_2==t2,] %>% 
      dplyr::select(-X) %>% 
      mutate(model='Original'),
    nStats[nStats$team_1==t1 & nStats$team_2==t2,] %>% 
      dplyr::select(-X) %>% 
      mutate(model='Sweet16')
  )
}

query('Gonzaga','Creighton')
query('USC','Oregon')
query('Baylor','Villanova')
query('Arkansas','Oral Roberts')
query('Michigan','Florida St')
query('Alabama','UCLA')
query('Loyola Chicago','Oregon St')
query('Houston','Syracuse')

query('Gonzaga','USC')
query('Baylor','Arkansas')
query('Michigan','Alabama')
query('Loyola Chicago','Houston')

query('Gonzaga','Michigan')
query('Baylor','Houston')

query('Gonzaga','Baylor')


# moved to shiny
sampleSize <<- 1

r <- mkBracket(use_historic=F)
predDFT <- data.frame()
for (i in 1:6){
  r <- runRND(r, i)
  predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
}

BRKT <- assignBRKT()

tiff(filename='data/test.tif', width=16, height=26, units='in', res=200)
grid.table(BRKT)
dev.off()

# pdf(file='data/test2019.pdf', width=20, height=35)
# grid.table(BRKT)
# dev.off()

SIMgame(tbl1='Texas', tbl2='Connecticut', alacarte=T)



w <- 'test'
while (!'Oregon' %in% w){
  r <- mkBracket(use_historic=F)
  predDFT <- data.frame()
  for (i in 1:6){
    r <- runRND(r, i)
    predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
  }
  w <- predDFT %>% filter(round==6) %>% pull(winner)
}

predDFT
