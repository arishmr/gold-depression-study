# X (IV) -> HIV.status
# Y (DV) -> total PHQ-9 score
# M (mediator) -> biomarker 'conc'

med.df <- data.frame(
  label = character(0),
  estimate = numeric(0),
  ci.min = numeric(0),
  ci.max = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = F
)

semdata <- blooddata

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
             total ~ cprime*HIV.status
           # mediator
             conc ~ a*HIV.status
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
a <- output[c(2),c(4,5,9:10,8)]
b <- output[c(3),c(4,5,9:10,8)]
ab <- output[c(10),c(4,5,9:10,8)]
c <- output[c(11),c(4,5,9:10,8)]
prop <- output[c(12),c(4,5,9:10,8)]
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

med.df$biomarker <- rep(unlist(lapply(splitdata, function (x) {c(unique(x$biomarker))})), each = 6)
names(med.df)[1] <- "parameter"
med.df$p.fdr <- p.adjust(med.df$p.value, method = "fdr")
med.df <- med.df[c("biomarker",
                   "parameter",
                   "estimate",
                   "ci.min",
                   "ci.max",
                   "p.value",
                   "p.fdr")]


write.csv(med.df, "Results/Results - Blood Mediation All Parameters.csv", row.names = F)


rm(semodel, med.df, semdata, splitdata)

