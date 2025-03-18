
library(dplyr)
library(rsconnect)

# convert tables to .rds
read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/masterTBL.csv') %>% 
  saveRDS(., 'data/masterTBL.rds')

read.csv('C:/Users/ryanm/Dropbox/R/MarchMadness_data/statsTBL.csv') %>% 
  saveRDS(., 'data/statsTBL.rds')

openxlsx::read.xlsx('C:/Users/ryanm/Dropbox/R/MarchMadness_data/teams/teams2025.xlsx', sheet='Sheet1') %>% 
  saveRDS(., 'data/teams2025.rds')


# necessary files
nf <- c('src/ncaaHelpers.R',
        'data/models/cv_outcome_2025.rds',
        'data/masterTBL.rds',
        'data/statsTBL.rds',
        'data/teams2025.rds',
        'app.R')

# change appName to create a new app, or dont to update
rsconnect::deployApp(appFiles=nf, appName='MarchMadness')

