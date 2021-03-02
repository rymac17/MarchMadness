
options(scipen=999)

source('src/ncaaHelpers.R')

# read tables
masterTBL <- read.csv('data/masterTBL.csv')
statsTBL <- read.csv('data/statsTBL.csv')

# hold out 2019
modelTBL <- filter(masterTBL, year!=2019)

# model
library(glmnet)
# x <- as.matrix(select(modelTBL, AdjEM, AdjO, AdjD, AdjT, Luck, OppO, OppD,
#                       AdjEM_2, AdjO_2, AdjD_2, AdjT_2, Luck_2, OppO_2, OppD_2,
#                       Rk, Losses, Rk_2, Losses_2))
x <- as.matrix(select(modelTBL, AdjEM, AdjO, AdjD, AdjT, OppO, OppD,
                      AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2,
                      Rk, Losses, Rk_2, Losses_2))
y1 <- as.matrix(select(modelTBL, outcome))
# y2 <- as.matrix(select(modelTBL, upset))

set.seed(1011)
cv_outcome <- cv.glmnet(x, y1, family="binomial", type.measure="auc", nfolds=10, alpha=1)
# set.seed(1011)
# cv_upset <- cv.glmnet(x, y2, family="binomial", type.measure="auc", nfolds=10, alpha=1)

# plot(cv_outcome)
# plot(cv_upset)
# coef(cv_outcome, s = "lambda.min")
# coef(cv_outcome, s = "lambda.1se")
# coef(cv_upset, s = "lambda.min")
# coef(cv_upset, s = "lambda.1se")

# performace
p0 <- predict(cv_outcome, newx=x, s="lambda.min", type="response")
# brier score
mean((p0-y1)^2) # win
# mean((p0-y2)^2) # upset

# save models
saveRDS(cv_outcome, 'data/models/cv_outcome.rds')
# saveRDS(cv_upset, 'data/models/cv_upset.rds')
