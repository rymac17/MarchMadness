
library(dplyr)
library(xlsx)
library(rsconnect)

# convert tables to .rds
read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv') %>% 
  saveRDS(., 'data/masterTBL.rds')

read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv') %>% 
  saveRDS(., 'data/statsTBL.rds')

read.xlsx('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams2023.xlsx', sheetName='Sheet1') %>% 
  saveRDS(., 'data/teams2023.rds')


# necessary files
nf <- c('src/ncaaHelpers.R',
        'data/models/cv_outcome_2023.rds',
        'data/masterTBL.rds',
        'data/statsTBL.rds',
        'data/teams2023.rds',
        'app.R')

# change appName to create a new app, or dont to update
deployApp(appFiles=nf,
          appName='MarchMadness')

