## Remove participants with HIV with viral load >200
alldata$PID[which(alldata$viral.load.copies > 200)]
# remove PIDs 4, 50, 59, 87 and re-run SEM for all biomarkers


#######################
######## MRI BIOMARKERS

med.df <- data.frame(
  label = character(0),
  estimate = numeric(0),
  ci.min = numeric(0),
  ci.max = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = F
)

semdata <- mridata
semdata <- filter(semdata, PID != 4 & PID != 50 & PID != 59 & PID != 87)

## Scale all biomarkers, age, and total PHQ-9 score
semdata[2:13] <- apply(semdata[2:13], 2, scale)
semdata$age <- as.numeric(scale(semdata$age))
semdata$total <- as.numeric(scale(semdata$total))
semdata$diff <- as.numeric(scale(semdata$diff))


## Create wide form dataset with scaled variables

semdata_long <- gather(
  data = semdata,
  key = biomarker,
  value = conc,
  BG.Cho:PWM.Ins.Cr,
  factor_key = T
)

semdata_long$region <- rep(c("Basal Ganglia", "MidFrontal Gray Matter", "Peritrigonal White Matter"),
                           times = 1,
                           each = 224)

semdata_long$referencing <- rep(c("Water Referenced", "Creatine Referenced"),
                                times = 6,
                                each = 56)

semdata_long$biomarker <- rep(c("Choline", "Myo-Inositol"),
                              times = 3,
                              each = 112)

semdata_long <- na.omit(semdata_long)

splitdata <- semdata_long %>%
  group_by(biomarker, region, referencing) %>%
  group_split



## MODELS

semodel <- '# direct effect
             total ~ cprime*HIV.status + age + gender + diff
           # mediator
             conc ~ a*HIV.status + age + gender + diff
             total ~ b*conc
           # indirect effect (a*b)
             ab := a*b
           # total effect
             c := cprime + (a*b)
            # proportion mediated
            prop := ((c-cprime)/c)*100
        '

## RUN SEM

set.seed(123)

med.df <- lapply(splitdata, FUN = function (x) {fit <- sem(semodel, x, se = "bootstrap", missing = "fiml.x", bootstrap = 1000)
summary(fit, standardized = T, ci = T)
output <- parameterEstimates(fit)
cprime <- output[c(1),c(4,5,9:10,8)]
a <- output[c(5),c(4,5,9:10,8)]
b <- output[c(9),c(4,5,9:10,8)]
ab <- output[c(28),c(4,5,9:10,8)]
c <- output[c(29),c(4,5,9:10,8)]
prop <- output[c(30),c(4,5,9:10,8)]
med.df[nrow(med.df)+1,] <- cprime
med.df[nrow(med.df)+1,] <- a
med.df[nrow(med.df)+1,] <- b
med.df[nrow(med.df)+1,] <- ab
med.df[nrow(med.df)+1,] <- c
med.df[nrow(med.df)+1,] <- prop
med.df
})
med.df <- reduce(med.df, full_join)


## Produce output CSV

med.df$referencing <- rep(unlist(lapply(splitdata, function (x) {c(unique(x$referencing))})), each = 6)
med.df$biomarker <- rep(unlist(lapply(splitdata, function (x) {c(unique(x$biomarker))})), each = 6)
med.df$region <- rep(unlist(lapply(splitdata, function (x) {c(unique(x$region))})), each = 6)
names(med.df)[1] <- "parameter"
colnames(med.df)
med.df$p.fdr <- p.adjust(med.df$p.value, method = "fdr")
med.df <- med.df[c(7,6,8,1:5,9)]


write.csv(med.df, "Results/Results - Virally-Suppressed MRS Mediation Adjusted All Parameters.csv", row.names = F)

rm(semodel, splitdata)
rm(med.df, semdata, semdata_long)





#########################
######## BLOOD BIOMARKERS

med.df <- data.frame(
  label = character(0),
  estimate = numeric(0),
  ci.min = numeric(0),
  ci.max = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = F
)

semdata <- blooddata
semdata <- filter(semdata, PID != 4 & PID != 50 & PID != 59 & PID != 87)

## Scale all biomarkers, age, and total PHQ-9 score
semdata[2:16] <- apply(semdata[2:16], 2, scale)
semdata$age <- as.numeric(scale(semdata$age))
semdata$total <- as.numeric(scale(semdata$total))

semdata <- gather(data = semdata,
                  key = biomarker,
                  value = conc,
                  BDNF:YKL40,
                  factor_key = T)
semdata <- na.omit(semdata)

#semdata$conc <- log2(semdata$conc)

splitdata <- semdata %>%
  group_by(biomarker) %>%
  group_split

## MODEL SPECIFICATION

semodel <- '# direct effect
             total ~ cprime*HIV.status + age + gender
           # mediator
             conc ~ a*HIV.status + age + gender
             total ~ b*conc
           # indirect effect (a*b)
             ab := a*b
           # total effect
             c := cprime + (a*b)
            # proportion mediated
            prop := ((c-cprime)/c)*100
        '

## RUN MODEL

set.seed(123)

med.df <- lapply(splitdata, FUN = function (x) {fit <- sem(semodel, x, se = "bootstrap", missing = "fiml.x", bootstrap = 1000)
summary(fit, standardized = T, ci = T)
output <- parameterEstimates(fit)
cprime <- output[c(1),c(4,5,9:10,8)]
a <- output[c(4),c(4,5,9:10,8)]
b <- output[c(7),c(4,5,9:10,8)]
ab <- output[c(21),c(4,5,9:10,8)]
c <- output[c(22),c(4,5,9:10,8)]
prop <- output[c(23),c(4,5,9:10,8)]
med.df[nrow(med.df)+1,] <- cprime
med.df[nrow(med.df)+1,] <- a
med.df[nrow(med.df)+1,] <- b
med.df[nrow(med.df)+1,] <- ab
med.df[nrow(med.df)+1,] <- c
med.df[nrow(med.df)+1,] <- prop
med.df
})
med.df <- reduce(med.df, full_join)


## PRODUCE OUTPUT CSV

med.df$biomarker <- rep(biomarkers, each = 6)
names(med.df)[1] <- "parameter"
med.df$p.fdr <- p.adjust(med.df$p.value, method = "fdr")
med.df <- med.df[c("biomarker",
                   "parameter",
                   "estimate",
                   "ci.min",
                   "ci.max",
                   "p.value",
                   "p.fdr")]


write.csv(med.df, "Results/Results - Virally-Suppressed Blood Mediation Adjusted All Parameters.csv", row.names = F)

rm(semodel, med.df, semdata, splitdata)


