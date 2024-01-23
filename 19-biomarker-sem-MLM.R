# X (IV) -> HIV.status
# Y (DV) -> total PHQ-9 score
# M (mediator) -> biomarker 'conc'
# covariates: age and gender

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
semdata$total <- log(semdata$total+1)

hist(semdata$total)

semdata <- gather(data = semdata,
                  key = biomarker,
                  value = conc,
                  BDNF:YKL40,
                  factor_key = T)
semdata <- na.omit(semdata)

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
library(plyr, include.only = c("rbind.fill"))

med.df <- lapply(splitdata, FUN = function (x) {fit <- sem(semodel, x, estimator = "MLM")
summary(fit, standardized = T, ci = T)
output <- parameterEstimates(fit)
cprime <- output[c(1),c(4,5,9:10,8)]
a <- output[c(4),c(4,5,9:10,8)]
b <- output[c(7),c(4,5,9:10,8)]
ab <- output[c(16),c(4,5,9:10,8)]
c <- output[c(17),c(4,5,9:10,8)]
prop <- output[c(18),c(4,5,9:10,8)]
med.df[nrow(med.df)+1,] <- cprime
med.df[nrow(med.df)+1,] <- a
med.df[nrow(med.df)+1,] <- b
med.df[nrow(med.df)+1,] <- ab
med.df[nrow(med.df)+1,] <- c
med.df[nrow(med.df)+1,] <- prop
med.df
})
med.df <- rbind.fill(med.df)


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


write.csv(med.df, "Results/Results - Blood Mediation Adjusted MLM.csv", row.names = F)


rm(semodel, med.df, semdata, splitdata)



## Load results from SEM
plotdata <- read.csv("Results/Results - Blood Mediation Adjusted MLM.csv")
plotdata <- plotdata %>%
  mutate(sig = ifelse(p.fdr<0.05, "Significant", "Non-Significant"))
plotdata$parameter <- as.factor(plotdata$parameter)

## Subset for relevant parameters and re-code facet levels
plotdata <- filter(plotdata, plotdata$parameter == "a" | plotdata$parameter == "b" | plotdata$parameter == "ab")
levels(plotdata$parameter)
levels(plotdata$parameter) <- c("HIV Status and Biomarker", "Indirect Effect", "Biomarker and PHQ-9 Score", "Total Effect", "HIV Status and PHQ-9 Score", "Proportion Mediated")
plotdata$parameter <- factor(plotdata$parameter, levels = c("HIV Status and Biomarker", "Biomarker and PHQ-9 Score", "Indirect Effect", "HIV Status and PHQ-9 Score", "Proportion Mediated", "Total Effect"))

## Create forest plots
gplot <- ggplot(plotdata, aes(x = estimate, reorder(biomarker, desc(biomarker)), color = sig)) +
  geom_point(size = 2, position=position_dodge(width = 0.5)) +  
  geom_errorbarh(aes(xmin = ci.min, xmax = ci.max), position=position_dodge(width = 0.5), height = 0.1) +
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed", cex = 0.5) +
  facet_wrap(~ factor(parameter), scale="fixed",
             labeller = labeller(p.type = label_wrap_gen(width = 5))) +
  labs(x = "Standardised Effect Estimate (95% CI)", y = "Biomarker", color = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette="Set1") +
  theme(panel.spacing.x = unit(2, "lines")) +
  theme(panel.spacing.y = unit(1.5, "lines")) +
  theme(axis.title = element_text(face="bold"))
gplot

ggsave("Figures/Blood Mediation Effects MLM.png", gplot, width = 8, height = 7)
rm(gplot, plotdata)


