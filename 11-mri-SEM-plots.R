## Load results from SEM
plotdata <- read.csv("Results/Results - MRS Mediation All Parameters.csv")
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

ggsave("Figures/MRS Mediation Effects.png", gplot, width = 12, height = 6)

rm(gplot, plotdata)




### ADJUSTED FOR AGE, GENDER, AND TIME DIFFERENCE BETWEEN PHQ-9 AND MRI

## Load results from SEM
plotdata <- read.csv("Results/Results - MRS Mediation Adjusted All Parameters.csv")
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

ggsave("Figures/MRS Mediation Effects Adjusted.png", gplot, width = 12, height = 6)

rm(gplot, plotdata)
