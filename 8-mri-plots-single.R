################# SUMMARISE MEAN (SD) BIOMARKER CONCENTRATIONS BY HIV STATUS

summarydata <- mridata_long %>%
  group_by(biomarker, region, referencing, HIV.status) %>%
  summarise(n = length(PID),
            mean = mean(conc),
            sd = sd(conc))

write.csv(summarydata, "Results/Results - MRS Biomarkers Summary by HIV Status.csv", row.names = F)

summarydata <- mridata_long %>%
  group_by(biomarker, region, referencing) %>%
  summarise(n = length(PID),
            mean = mean(conc),
            sd = sd(conc))

write.csv(summarydata, "Results/Results - MRS Biomarkers Summary Overall.csv", row.names = F)

rm(summarydata)


############### GOAL: CREATE PLOTS OF BIOMARKER CONCENTRATIONS BY HIV STATUS AND BY PHQ-9 SCORE

## NEUROIMAGING BIOMARKERS BY HIV STATUS

plotdata <- mridata_long

gplot <- ggplot(plotdata, aes(HIV.status, conc, color = HIV.status)) +
  geom_boxplot(outlier.colour = NA) +
  geom_dotplot(binaxis = 'y',
               stackdir = 'center',
               #binwidth = 0.3,
               alpha = 0.2) +
  facet_nested(factor(biomarker) + factor(referencing) ~ factor(region), scale="free_y") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "HIV status", y = "Biomarker Concentration (institutional units)") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(2, "lines")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(title = element_text(face = "bold"), axis.title = element_text(face="bold"))
gplot
ggsave("Figures/MRS Biomarkers by HIV.png", gplot, width = 6, height = 7)


################# SUMMARISE AND PLOT BIOMARKER CONCENTRATIONS BY PHQ-9 SCORE (2x biomarkers, 3x brain regions, 2x referencing protocols)

gplot <- ggplot(plotdata, aes(total, conc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_nested(factor(biomarker) + factor(referencing) ~ factor(region), scale="free_y") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Total PHQ-9 Score", y = "Biomarker Concentration (institutional units)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(2, "lines")) +
  theme(title = element_text(face = "bold"), axis.title = element_text(face="bold"))
gplot
ggsave("Figures/MRS Biomarkers and PHQ9.png", gplot, width = 6, height = 7)

rm(gplot)



############# Linear regression of biomarker concentrations by HIV.status

a <- as.character(plotdata$HIV.status)
a[a=="Participants without HIV"] <- -0.5
a[a=="Participants with HIV"] <- 0.5
plotdata$HIV.status <- as.factor(a)
rm(a)


splitdata <- plotdata %>%
  group_by(biomarker, region, referencing) %>%
  group_split

reg.df <- data.frame(
  estimate = character(0),
  r.sq = character(0),
  p.value = character(0)
)

View(splitdata)

reg.df <- lapply(splitdata, FUN = function(x) {
  fit <- lm(conc ~ HIV.status, data = x)
  summary(fit)
  estimate <- as.numeric(coef(fit)[2])
  r.sq <- as.numeric(summary(fit)$adj.r.squared)
  p.value <- as.numeric(summary(fit)$coefficients[2,4])
  reg.df[nrow(reg.df)+1,] <- c(estimate, r.sq, p.value)
  reg.df
})
reg.df <- reduce(reg.df, full_join)

reg.df$p.fdr <- p.adjust(reg.df$p.value, method = "fdr")
reg.df$biomarker <- rep(c("Choline", "Myo-Inositol"), each = 6)
reg.df$region <- rep(c("Basal Ganglia", "MidFrontal Gray Matter", "Peritrigonal White Matter"), each = 2, 2)
reg.df$referencing <- rep(c("Creatine", "Water"), 6)
reg.df <- reg.df[c(5,7,6,1:4)]

write.csv(reg.df, "Results/Results - MRS Biomarkers by HIV Status.csv", row.names = F)

rm(splitdata, gplot, reg.df)


############# Linear regression of biomarker concentrations with PHQ-9 score


splitdata <- plotdata %>%
  group_by(biomarker, region, referencing) %>%
  group_split

reg.df <- data.frame(
  estimate = character(0),
  r.sq = character(0),
  p.value = character(0)
)

reg.df <- lapply(splitdata, FUN = function(x) {
  fit <- lm(total ~ conc, data = x)
  summary(fit)
  estimate <- as.numeric(coef(fit)[2])
  r.sq <- as.numeric(summary(fit)$adj.r.squared)
  p.value <- as.numeric(summary(fit)$coefficients[2,4])
  reg.df[nrow(reg.df)+1,] <- c(estimate, r.sq, p.value)
  reg.df
})
reg.df <- reduce(reg.df, full_join)

reg.df$p.fdr <- p.adjust(reg.df$p.value, method = "fdr")
reg.df$biomarker <- rep(c("Choline", "Myo-Inositol"), each = 6)
reg.df$region <- rep(c("Basal Ganglia", "MidFrontal Gray Matter", "Peritrigonal White Matter"), each = 2, 2)
reg.df$referencing <- rep(c("Creatine", "Water"), 6)
reg.df <- reg.df[c(5,7,6,1:4)]

write.csv(reg.df, "Results/Results - MRS Biomarkers by Total PHQ-9 Scores.csv", row.names = F)

rm(splitdata, gplot, reg.df)


rm(plotdata)


