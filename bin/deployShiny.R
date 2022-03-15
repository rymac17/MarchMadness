
library(rsconnect)

# convert tables to .rds
read.csv('data/masterTBL.csv') %>% 
  saveRDS(., 'shinyData/masterTBL.rds')

read.csv('data/statsTBL.csv') %>% 
  saveRDS(., 'shinyData/statsTBL.rds')

read.xlsx('data/teams2021.xlsx', sheetName='Sheet1') %>% 
  saveRDS(., 'shinyData/teams2021.rds')


# necessary files
nf <- c('src/ncaaHelpers.R',
        'data/model2021/cv_outcome.rds',
        'shinyData/masterTBL.rds',
        'shinyData/statsTBL.rds',
        'shinyData/teams2021.rds',
        'app.R')

deployApp(appFiles=nf,
          appName='MarchMadness2021')

