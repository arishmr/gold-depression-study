## Twelve separate models: Cho and Ins each in BG, MFGM, PWM across water and creatine referencing

# X (IV) -> HIV.status
# Y (DV) -> total PHQ-9 score
# M (mediator) -> Cho/Ins in BG/MFGM/PWM
# Covariates: age, gender, time difference ("diff")

med.df <- data.frame(
  label = character(0),
  estimate = numeric(0),
  ci.min = numeric(0),
  ci.max = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = F
)


semdata <- mridata

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
                           each = 240)

semdata_long$referencing <- rep(c("Water Referenced", "Creatine Referenced"),
                                times = 6,
                                each = 60)

semdata_long$biomarker <- rep(c("Choline", "Myo-Inositol"),
                              times = 3,
                              each = 120)

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
library(plyr, include.only = c("rbind.fill"))

med.df <- lapply(splitdata, FUN = function (x) {fit <- sem(semodel, x, estimator = "MLM")
summary(fit, standardized = T, ci = T)
output <- parameterEstimates(fit)
cprime <- output[c(1),c(4,5,9:10,8)]
a <- output[c(5),c(4,5,9:10,8)]
b <- output[c(9),c(4,5,9:10,8)]
ab <- output[c(22),c(4,5,9:10,8)]
c <- output[c(23),c(4,5,9:10,8)]
prop <- output[c(24),c(4,5,9:10,8)]
med.df[nrow(med.df)+1,] <- cprime
med.df[nrow(med.df)+1,] <- a
med.df[nrow(med.df)+1,] <- b
med.df[nrow(med.df)+1,] <- ab
med.df[nrow(med.df)+1,] <- c
med.df[nrow(med.df)+1,] <- prop
med.df
})
med.df <- rbind.fill(med.df)


## Produce output CSV

med.df$referencing <- rep(unlist(lapply(splitdata, function (x) {c(unique(x$referencing))})), each = 6)
med.df$biomarker <- rep(unlist(lapply(splitdata, function (x) {c(unique(x$biomarker))})), each = 6)
med.df$region <- rep(unlist(lapply(splitdata, function (x) {c(unique(x$region))})), each = 6)
names(med.df)[1] <- "parameter"
colnames(med.df)
med.df$p.fdr <- p.adjust(med.df$p.value, method = "fdr")
med.df <- med.df[c(7,6,8,1:5,9)]


write.csv(med.df, "Results/Results - MRS Mediation Adjusted MLM.csv", row.names = F)

rm(semodel, splitdata)
rm(med.df, semdata, semdata_long)


## Load results from SEM
plotdata <- read.csv("Results/Results - MRS Mediation Adjusted MLM.csv")
plotdata <- plotdata %>%
  mutate(sig = ifelse(p.fdr<0.05, "Significant", "Non-Significant"))
plotdata$parameter <- as.factor(plotdata$parameter)

## Subset for relevant parameters and re-code facet levels
plotdata <- filter(plotdata, plotdata$parameter == "a" | plotdata$parameter == "b" | plotdata$parameter == "ab")
plotdata$biomarker <- as.factor(plotdata$biomarker)
plotdata$referencing <- as.factor(plotdata$referencing)
plotdata$region <- as.factor(plotdata$region)
levels(plotdata$parameter) <- c("HIV Status and Biomarker", "Indirect Effect", "Biomarker and PHQ-9 Score", "Total Effect", "HIV Status and PHQ-9 Score", "Proportion Mediated")
plotdata$parameter <- factor(plotdata$parameter, levels = c("HIV Status and Biomarker", "Biomarker and PHQ-9 Score", "Indirect Effect", "HIV Status and PHQ-9 Score", "Proportion Mediated", "Total Effect"))
levels(plotdata$biomarker) <- c("Choline", "Myo-Inositol")
levels(plotdata$referencing) <- c("Creatine Referenced", "Water Referenced")
levels(plotdata$region) <- c("Basal Ganglia", "MidFrontal Gray Matter", "Peritrigonal White Matter")

## Create forest plots
gplot <- ggplot(plotdata, aes(x = estimate, reorder(biomarker, desc(biomarker)), color = sig)) +
  geom_point(size = 2, position=position_dodge(width = 0.5)) +  
  geom_errorbarh(aes(xmin = ci.min, xmax = ci.max), position=position_dodge(width = 0.5), height = 0.1) +
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed", cex = 0.5) +
  facet_nested(factor(region) ~ factor(parameter) + factor(referencing), scale="free_y",
               labeller = labeller(region = label_wrap_gen(width = 2))) +
  labs(x = "Standardised Effect Estimate (95% CI)", y = "Biomarker", color = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette="Set1") +
  theme(panel.spacing.x = unit(2, "lines")) +
  theme(panel.spacing.y = unit(1.5, "lines")) +
  theme(axis.title = element_text(face="bold"))
gplot

ggsave("Figures/MRS Mediation Effects MLM.png", gplot, width = 12, height = 6)

rm(gplot, plotdata)


#browseURL("https://www.youtube.com/watch?v=Xwp9__PzW6k")