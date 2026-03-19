
# load
source('src/ncaaHelpers.R')
yr <<- 2026
teams <- openxlsx::read.xlsx(paste0('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams',yr,'.xlsx'), sheet='Sheet1')
statsTBL <- read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv')
cv_outcome <- readRDS(paste0('data/models/cv_outcome_',yr,'.rds'))


# generate a bracket where favorite team wins
favorite <<- 'Arizona'
sampleSize <<- 1
counter <<- 0
winner <<- 'none'
while (winner != favorite){
  r <- mkBracket(use_historic=F)
  predDFT <- data.frame()
  for (i in 1:6){
    r <- runRND(r, i)
    predDFT <- rbind(predDFT, r %>%
                       dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
  }
  counter <<- counter + 1
  winner <<- dplyr::filter(predDFT, region_name == 'Championship') |> dplyr::pull(winner)
}
counter
predDFT

# new <- assignBRKT(t=teams, pdft=predDFT)
# grid.table(new)

