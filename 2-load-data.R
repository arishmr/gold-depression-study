rm(list = ls())

## Load datasets
alldata <- read.csv("Full_Dataset.csv")

alldata$age <- as.numeric(alldata$age)
alldata$gender <- as.factor(alldata$gender)
alldata$ethnicity <- as.factor(alldata$ethnicity)
alldata$sexual.orient <- as.factor(alldata$sexual.orient)
alldata$alcohol <- as.factor(alldata$alcohol)
alldata$smoking <- as.factor(alldata$smoking)
alldata$recdrugs <- as.factor(alldata$recdrugs)
alldata$HIV.status <- factor(alldata$HIV.status)
alldata$HIV.status <- relevel(alldata$HIV.status, ref = "Participants without HIV")
alldata$language <- as.factor(alldata$language)
alldata$language <- relevel(alldata$language, ref = "English")
alldata$regimen <- as.factor(alldata$regimen)

## Create subsets of data
colnames(alldata)

demodata <- alldata %>% dplyr::select(PID, age, grade, gender, ethnicity, sexual.orient,
                                      alcohol, smoking, recdrugs,
                                      language, HIV.status,
                                      viral.load.log, viral.load.copies, CD4, CD4.abs,
                                      regimen)
testdata <- alldata %>% dplyr::select(PID, q1, q2, q3, q4, q5, q6, q7, q8, q9,
                                      total,
                                      language, HIV.status)
mridata <- alldata %>% dplyr::select(PID,
                                     BG.Cho:PWM.Ins.Cr,
                                     total,
                                     HIV.status,
                                     age, gender, diff)
blooddata <- alldata %>% dplyr::select(PID,
                                       BDNF:YKL40,
                                       total,
                                       HIV.status,
                                       age, gender)



## Define vector of PHQ-9 items
items <- c("Anhedonia", "Depressed Mood", "Sleep Changes", "Loss of Energy", "Appetite Changes",
           "Low Self-Esteem", "Trouble Concentrating", "Psychomotor Changes", "Suicide Ideation")




####################
## CLEAN UP BLOOD BIOMARKER DATA

## LOAD DATAFRAME and remove NFL data, and create character vector of biomarkers included
blooddata <- dplyr::select(blooddata, -NFL)
biomarkers <- colnames(blooddata[,2:16])
## Add HIV status and total PHQ-9 score to this dataset
blooddata$HIV.status <- alldata$HIV.status
blooddata$total <- alldata$total

## How many observations are OOR for each biomarker?
apply(blooddata, 2, function (x) {sum(x == "OOR <", na.rm = T)})
# 5 undetectable in MIP1.alpha, 36 undetectable in S100A8

## Impute undetectable values -- if OOR <, use half of the lowest detected value as assumed value
blooddata$S100A8[blooddata$S100A8 == "OOR <"] <- (min(as.numeric(blooddata$S100A8), na.rm = T))*0.5
blooddata$MIP1.alpha[blooddata$MIP1.alpha == "OOR <"] <- (min(as.numeric(blooddata$MIP1.alpha), na.rm = T))*0.5


## Convert to numeric data
blooddata <- blooddata %>% mutate_if(is.character, as.numeric)
class(blooddata$MIP1.alpha)


## DETECTING AND REMOVING OUTLIERS

summary(blooddata[,2:16])

#hist(blooddata$CD14)

outliers <- function (y) {which(y < quantile(y, 0.01, na.rm = T) | y > quantile(y, 0.99, na.rm = T))}

remove.outliers <- function (x) {
  outliers(x)
  length(outliers(x))
  if (length(outliers(x)) > 0) {
    rosner <- rosnerTest(x, length(outliers(x)))$all.stats
    rosner <- filter(rosner, Outlier == "TRUE")
    rosner <- rosner$Obs.Num
    x[rosner] <- "Outlier"
    x}
}

blooddata[,c(2:5,7:13,15:16)] <- lapply(blooddata[,c(2:5,7:13,15:16)], remove.outliers)

## How many observations are removed as outliers for each biomarker?
apply(blooddata, 2, function (x) {sum(x == "Outlier", na.rm = T)})

## Remove outliers by converting to NA
blooddata <- blooddata %>% mutate_if(is.character, as.numeric)


rm(outliers, remove.outliers)
