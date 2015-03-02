## analyzeBackblaze.R

## REQUIRE
require(dplyr)
require(survival)
require(ggplot2)

## CONNECT TO DB
backblazeDB <- src_sqlite(path = 'drive_stats.db')

## MAKE TABLES
failureTable <- tbl(backblazeDB, 'failure_rates')
statsTable <- tbl(backblazeDB, 'drive_stats')
daysTable <- tbl(backblazeDB, 'drive_days')

## ANALYZE STATS
# Go into the stats table, group by model, filter for failure == 1, pull out drive age (SMART 9 RAW)
hdSurvivalA <- statsTable %>%
  select(model, failure, smart_9_raw) %>%
  group_by(model) %>%
  filter(failure == 1) %>%
  mutate(daysToFailure = smart_9_raw/24)

# Develop survival model by model
hdSurvivalDF <- data.frame(select(hdSurvivalA, model, daysToFailure, failure))
rownames(hdSurvivalDF) <- hdSurvivalDF$model
tmpSurv <- Surv(hdSurvivalDF$daysToFailure, hdSurvivalDF$failure)
hdSurvFit <- survfit(tmpSurv ~ hdSurvivalDF$model)

# Let's pull out any models with fewer than 20 records
uniqueModels <- unique(hdSurvivalDF$model)
modelCount <- vector(mode = 'list', length = length(uniqueModels))
names(modelCount) <- uniqueModels

countModels <- function(modelName)
{
  modelCount <- length(grep(TRUE, hdSurvivalDF$model == modelName))
  return(modelCount)
}

for(i in 1:length(uniqueModels))
{
  iModelName <- uniqueModels[i]
  iCount <- countModels(iModelName)
  modelCount[[i]] <- iCount
}

modelCount <- unlist(modelCount)
includeModels <- modelCount[grep(TRUE, modelCount >= 20)]
includeModels <- names(includeModels)

getModelRows <- function(modelName)
{
  modelInd <- grep(TRUE, hdSurvivalDF$model == modelName)
  subDF <- hdSurvivalDF[modelInd, ]
  return(subDF)
}

subDfList <- lapply(includeModels, getModelRows)

finalHdSurvDF <- do.call(rbind.data.frame, subDfList)

tmpSurv <- Surv(finalHdSurvDF$daysToFailure, finalHdSurvDF$failure)
hdSurvFit <- survfit(tmpSurv ~ finalHdSurvDF$model)

hdSurvPlot <- ggsurv(hdSurvFit)
hdSurvPlot + ggtitle('Failure Curves for Hard Drives from Backblaze Data (>12M records)\n') +
  xlab('\nTime in Days') + ylab('Percentage of Hard Drives Still Functioning\n')

# Summarize to mean daysToFailure
hdSurvivalB <- hdSurvival %>%
  group_by(model) %>%
  summarise(meanDaysToFailure = mean(daysToFailure)) %>%
  arrange(desc(meanDaysToFailure))

hdSurvivalB <- data.frame(hdSurvival)

## VISUALIZE
barPlot <- ggplot(data = hdSurvival, aes(x = factor(model), y = meanDaysToFailure)) + 
  geom_bar(stat = 'identity', aes(fill = factor(model))) +
  theme(axis.text.x = element_blank())

  
# tmpSurv <- data.frame(meanDaysToFailure = select(hdSurvival, meanDaysToFailure))
# tmpSurv <- data.frame(tmpSurv, failure = rep(1, nrow(tmpSurv)))
# tmpSurv <- Surv(tmpSurv$meanDaysToFailure, tmpSurv$failure)
# hdModels <- data.frame(select(hdSurvival, model))
# 
# hdSurvFit <- survfit(tmpSurv ~ hdModels[ , 1])
