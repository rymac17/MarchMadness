

# make and save model
options(scipen=999)
library(dplyr)
library(glmnet)
source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv')
statsTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv')

# model
yr <- 2024
modelTBL <- filter(masterTBL, year!=yr)
x <- as.matrix(select(modelTBL, 
                      GameT, GameO, GameD,
                      AdjEM, AdjO, AdjD, AdjT, OppO, OppD, OppEM,
                      AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2, OppEM_2))
y <- as.matrix(select(modelTBL, outcome))
set.seed(1011)
cv_outcome <<- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=10, alpha=1)
coef(cv_outcome, s="lambda.min")
saveRDS(cv_outcome, paste0('data/models/cv_outcome_',yr,'.rds'))


# upsets
y <- as.matrix(select(modelTBL, upset))
set.seed(1011)
cv_upset <<- cv.glmnet(x, y, family="binomial", type.measure="auc", nfolds=10, alpha=1)
coef(cv_upset, s="lambda.min")
saveRDS(cv_upset, paste0('data/models/cv_upset_',yr,'.rds'))



# read in teams and make predictions
rm(list=ls())
options(scipen=999)
library(gridExtra)
library(dplyr)
library(glmnet)
library(openxlsx)
source('src/ncaaHelpers.R')
# hyperparameters
yr <<- 2024
sampleSize <<- 0
# read model
cv_outcome <- readRDS(paste0('data/models/cv_outcome_',yr,'.rds'))
# read tables
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv')
statsTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv')

teams <- read.xlsx(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams',yr,'.xlsx'), sheet='Sheet1')
teams[which(!teams$Team %in% (statsTBL %>% filter(year==yr) %>% pull(Team))),]

# first four
SIMgame(tbl1='Wagner', tbl2='Howard', alacarte=T)
SIMgame(tbl1='Colorado St', tbl2='Virginia', alacarte=T)
SIMgame(tbl1='Grambling St', tbl2='Montana St', alacarte=T)
SIMgame(tbl1='Colorado', tbl2='Boise St', alacarte=T)

# upsets
cv_upset <- readRDS(paste0('data/models/cv_upset_',yr,'.rds'))
SIMprob(t1='Houston', t2='Iowa', upset=T)

lapply(c(1, 2, 3, 4), function(j){
  rg <- teams %>% 
    filter(region_number==j)
  l <- lapply(rg$Team[1:11], function(t){
    sed <- rg %>% filter(Team==t) %>% pull(Seed)
    opp <- rg %>% filter(Team!=t) %>% filter(Seed>=sed+5) %>% pull(Team)
    fld <- sapply(opp, function(x) SIMprob(t1=t, t2=x, upset=T)) %>% 
      as.list() %>% 
      as.data.frame()
    return(fld)
  }) %>% 
    do.call('bind_rows',.)
  rownames(l) <- rg$Team[1:11]
  # print(l)
  return(rowMeans(l, na.rm=T))
}) %>% 
  do.call('c',.) %>% 
  sort() %>% 
  as.list() %>% 
  data.frame()




# regions ----
for (j in c(1, 2, 3, 4)){
  rg <- teams %>% 
    filter(region_number==j)
  lapply(rg$Team, function(i){
    opp <- setdiff(rg$Team, i)
    fld <- lapply(opp, function(x) SIMgame(tbl1=i, tbl2=x, SS=10, alacarte=T)) %>% 
      do.call('rbind', .) %>% 
      mutate(winPdiff=winP_1-winP_2) %>% 
      group_by(team_1) %>% 
      summarise(CumWinP=sum(winPdiff))
    return(fld)
  }) %>% 
    do.call('rbind',.) %>% 
    arrange(desc(CumWinP)) %>% 
    print(.)
}  




# final four prob ----
source('src/ncaaHelpers.R')
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv')
statsTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv')
yr=2023
teams <- read.xlsx(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams',yr,'.xlsx'), sheet='Sheet1')
cv_outcome <- readRDS(paste0('data/models/cv_outcome_',yr,'.rds'))
sampleSize=1
l <- lapply(c(1:100), function(x){
  r <- mkBracket(use_historic=F)
  predDFT <- data.frame()
  for (i in 1:6){
    r <- runRND(r, i)
    predDFT <- rbind(predDFT, r %>%
                       dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
  }
  p <- predDFT %>% filter(round==4) %>% dplyr::select(winner, region_number, Prob)
  return(p)
}) %>% purrr::reduce(full_join, by=c('winner','region_number'))
out <- data.frame(region_number=l$region_number, team=l$winner, avgProb=rowMeans(l[3:ncol(l)], na.rm=T),
                  numFF=length(3:ncol(l)) - rowSums(is.na(l[3:ncol(l)])))
arrange(out, desc(avgProb))
# region_number        team    avgProb numFF
# 1              1     Alabama 0.45085009    75
# 2              3     Houston 0.44586638    66
# 3              4        UCLA 0.32808093    44
# 4              2      Purdue 0.28017350    37
# 5              2   Tennessee 0.21401796    31
# 6              3       Texas 0.19976726    27
# 7              2   Marquette 0.14534325    16
# 8              4 Connecticut 0.13225375    16
# 9              4     Gonzaga 0.12334995    17
# 10             4      Kansas 0.12306224    19
# 11             1     Arizona 0.08667132    10
# 12             2   Kansas St 0.05772687     6
# 13             1      Baylor 0.04924508     5
# 14             3      Xavier 0.04182126     7
# 15             1   Creighton 0.04067276    10
# 16             2    Kentucky 0.03397262     6
# 17             4    St Marys 0.03289085     4
# 18             2 Michigan St 0.01976937     1
# 19             2        Duke 0.01928637     3


# test on 2022
rm(list=ls())
source('src/ncaaHelpers.R')
yr=2022
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv') %>% 
  filter(year!=yr)
statsTBL <- read.csv(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/kenpom/pre_tournament_scrape/kenpom',
                            yr,'.csv'))
teams <- read.xlsx(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams',yr,'.xlsx'), sheet='Sheet1')
cv_outcome <- readRDS(paste0('data/models/cv_outcome_',yr,'.rds'))
sampleSize=1
l <- lapply(c(1:100), function(x){
  r <- mkBracket(use_historic=F)
  predDFT <- data.frame()
  for (i in 1:6){
    r <- runRND(r, i)
    predDFT <- rbind(predDFT, r %>%
                       dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
  }
  p <- predDFT %>% filter(round==4) %>% dplyr::select(winner, region_number, Prob)
  return(p)
}) %>% purrr::reduce(full_join, by=c('winner','region_number'))
out <- data.frame(region_number=l$region_number, team=l$winner, avgProb=rowMeans(l[3:ncol(l)], na.rm=T),
                  numFF=length(3:ncol(l)) - rowSums(is.na(l[3:ncol(l)])))
arrange(out, desc(avgProb))
# region_number         team     avgProb numFF
# 1              1      Gonzaga 0.561962373    72    *sw16 (Ark)
# 2              2       Baylor 0.279909086    46    *rd32 (UNC)
# 3              2     Kentucky 0.267553944    28    *rd64 (StPe)
# 4              4       Auburn 0.260274162    43    *rd32 (Mia)
# 5              4       Kansas 0.258851926    40 ***FF***
# 6              3      Arizona 0.240581032    39    *sw16 (Hou)
# 7              3    Tennessee 0.220819584    29    *rd32 (Mich)
# 8              4         Iowa 0.158170691    12    *rd64 (Rich)
# 9              2         UCLA 0.149000567    13    *sw16 (UNC)
# 10             3    Villanova 0.124698587    11 ***FF***
# 11             3      Houston 0.117750122    19    *e8 (Vill)
# 12             2       Purdue 0.096703927    10    *sw16 (StPe)
# 13             1   Texas Tech 0.093910894    18    *sw16 (Duke)
# 14             1         Duke 0.069252386    10 ***FF***
# 15             4          LSU 0.026213552     3    *rd64 (IaSt)
# 16             2        Texas 0.025294577     2    *rd32 (Purd)
# 17             4 San Diego St 0.015320510     2    *rd64 (Crei)
# 18             3     Illinois 0.015029451     2    *rd32 (Hou)
# 19             2     St Marys 0.009132684     1    *rd32 (UCLA)
#                      North Carolina             ***FF***



# test on 2021
rm(list=ls())
source('src/ncaaHelpers.R')
yr=2021
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv') %>% 
  filter(!year %in% c(2021,2022))
statsTBL <- read.csv(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/kenpom/pre_tournament_scrape/kenpom',
                            yr,'.csv'))
teams <- read.xlsx(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams',yr,'.xlsx'), sheet='Sheet1')
cv_outcome <- readRDS(paste0('data/models/cv_outcome_',yr,'.rds'))
sampleSize=1
l <- lapply(c(1:100), function(x){
  r <- mkBracket(use_historic=F)
  predDFT <- data.frame()
  for (i in 1:6){
    r <- runRND(r, i)
    predDFT <- rbind(predDFT, r %>%
                       dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
  }
  p <- predDFT %>% filter(round==4) %>% dplyr::select(winner, region_number, Prob)
  return(p)
}) %>% purrr::reduce(full_join, by=c('winner','region_number'))
out <- data.frame(region_number=l$region_number, team=l$winner, avgProb=rowMeans(l[3:ncol(l)], na.rm=T),
                  numFF=length(3:ncol(l)) - rowSums(is.na(l[3:ncol(l)])))
arrange(out, desc(avgProb))
# region_number           team     avgProb numFF
# 1              1        Gonzaga 0.690167749    71 ***FF***
# 2              2       Michigan 0.620882972    71    *e8 (UCLA)
# 3              4       Illinois 0.547774469    71    *rd32 (LoyChi)
# 4              3        Ohio St 0.409571836    32    *rd64 (OrlRb)
# 5              3         Baylor 0.393619972    31 ***FF***
# 6              1           Iowa 0.255128491    29    *rd32 (Ore)
# 7              2        Alabama 0.228653820    17    *sw16 (UCLA)
# 8              4        Houston 0.195831173    24 ***FF***
# 9              3         Purdue 0.141058813    15    *rd64 (NTex)
# 10             3      Villanova 0.112888442     4    *sw16 (Bayl)
# 11             2          Texas 0.101760075     2    *rd64 (AbChr)
# 12             3      Wisconsin 0.099983266     4    *rd32 (Bayl)
# 13             3       Arkansas 0.094725481     7    *e8 (Bayl)
# 14             2     Florida St 0.074677316     3    *sw16 (Mich)
# 15             2    Connecticut 0.049650261     5    *rd64 (Mary)
# 16             3     Texas Tech 0.044752537     6    *rd32 (Ark)
# 17             4  West Virginia 0.040153700     5    *rd32 (Syra)
# 18             2       Maryland 0.034978650     1    *rd32 (Alab)
# 19             3 North Carolina 0.021589510     1    *rd64 (Wisc)
# 20             2           UCLA 0.008859364     1 ***FF***





# easiest path
source('src/ncaaHelpers.R')
masterTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv')
statsTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv')
yr=2023
teams <- read.xlsx(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams',yr,'.xlsx'), sheet='Sheet1')
stat='AdjEM'
stat='AdjO'
stat='AdjD'
mL <- lapply(c(1, 2, 3, 4), function(j){
  rg <- teams %>% 
    filter(region_number==j)
  l <- sapply(rg$Team, function(i){
    fld <- statsTBL %>% filter(year==yr) %>% filter(Team %in% setdiff(rg$Team, i)) %>% pull(stat)
    return(mean(fld))
  }) %>% sort()
  return(l)
}) %>% do.call('c', .) %>% sort()
mL



# # second chance 2021
# scTeams <- c('Gonzaga','Creighton','USC','Oregon','Baylor','Villanova','Arkansas','Oral Roberts',
#              'Michigan','Florida St','UCLA','Alabama','Loyola Chicago','Oregon St','Syracuse','Houston')



# # original stats
# scTBL <- expand.grid(t1=scTeams, t2=scTeams, stringsAsFactors=F) %>%
#   filter(t1!=t2)
# lapply(1:nrow(scTBL), function(x){
#   SIMgame(tbl1=scTBL[x,1], tbl2=scTBL[x,2], SS=0, alacarte=T)
# }) %>%
#   do.call('rbind', .) %>%
#   write.csv('data/scTBL_originalStats.csv')

# # stats updated after 2nd round
# scTBL <- expand.grid(t1=scTeams, t2=scTeams, stringsAsFactors=F) %>%
#   filter(t1!=t2)
# lapply(1:nrow(scTBL), function(x){
#   SIMgame(tbl1=scTBL[x,1], tbl2=scTBL[x,2], SS=0, alacarte=T)
# }) %>%
#   do.call('rbind', .) %>%
#   write.csv('data/scTBL_sweet16Stats.csv')
# 
# 
# 
# query <- function(t1, t2){
#   oStats <- read.csv('data/scTBL_originalStats.csv')
#   nStats <- read.csv('data/scTBL_sweet16Stats.csv')
#   rbind(
#     oStats[oStats$team_1==t1 & oStats$team_2==t2,] %>% 
#       dplyr::select(-X) %>% 
#       mutate(model='Original'),
#     nStats[nStats$team_1==t1 & nStats$team_2==t2,] %>% 
#       dplyr::select(-X) %>% 
#       mutate(model='Sweet16')
#   )
# }
# 
# query('Gonzaga','Creighton')
# query('USC','Oregon')
# query('Baylor','Villanova')
# query('Arkansas','Oral Roberts')
# query('Michigan','Florida St')
# query('Alabama','UCLA')
# query('Loyola Chicago','Oregon St')
# query('Houston','Syracuse')
# 
# query('Gonzaga','USC')
# query('Baylor','Arkansas')
# query('Michigan','Alabama')
# query('Loyola Chicago','Houston')
# 
# query('Gonzaga','Michigan')
# query('Baylor','Houston')
# 
# query('Gonzaga','Baylor')
# 
# 
# # moved to shiny
# sampleSize <<- 1
# 
# r <- mkBracket(use_historic=F)
# predDFT <- data.frame()
# for (i in 1:6){
#   r <- runRND(r, i)
#   predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
# }
# 
# BRKT <- assignBRKT()
# 
# tiff(filename='data/test.tif', width=16, height=26, units='in', res=200)
# grid.table(BRKT)
# dev.off()
# 
# # pdf(file='data/test2019.pdf', width=20, height=35)
# # grid.table(BRKT)
# # dev.off()
# 
# SIMgame(tbl1='Texas', tbl2='Connecticut', alacarte=T)
# 
# 
# 
# w <- 'test'
# while (!'Oregon' %in% w){
#   r <- mkBracket(use_historic=F)
#   predDFT <- data.frame()
#   for (i in 1:6){
#     r <- runRND(r, i)
#     predDFT <- rbind(predDFT, r %>% dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
#   }
#   w <- predDFT %>% filter(round==6) %>% pull(winner)
# }
# 
# predDFT
