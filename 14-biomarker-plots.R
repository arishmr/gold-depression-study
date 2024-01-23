################# SUMMARISE MEAN (SD) BIOMARKER CONCENTRATIONS BY HIV STATUS
data <- gather(data = blooddata,
                         key = biomarker,
                         value = conc,
                         BDNF:YKL40,
                         factor_key = T)
data <- na.omit(data)
summarydata <- data %>%
  group_by(biomarker, HIV.status) %>%
  summarise(n = length(PID),
            mean = mean(conc),
            sd = sd(conc))
unique(data$biomarker)

write.csv(summarydata, "Results/Results - Blood Biomarkers Summary by HIV Status.csv", row.names = F)

data <- gather(data = blooddata,
               key = biomarker,
               value = conc,
               BDNF:YKL40,
               factor_key = T)
data <- na.omit(data)
summarydata <- data %>%
  group_by(biomarker) %>%
  summarise(n = length(PID),
            mean = mean(conc),
            sd = sd(conc))
unique(data$biomarker)

write.csv(summarydata, "Results/Results - Blood Biomarkers Summary Overall.csv", row.names = F)

rm(summarydata, data)


############### GOAL: CREATE PLOTS OF BIOMARKER CONCENTRATIONS BY HIV STATUS AND BY PHQ-9 SCORE

## Create long-form data frame of biomarker concentrations
blooddata_long <- gather(data = blooddata,
                         key = biomarker,
                         value = conc,
                         BDNF:YKL40,
                         factor_key = T)
blooddata_long <- na.omit(blooddata_long)
blooddata_long$conc <- log2(blooddata_long$conc)

## BLOOD BIOMARKERS BY HIV STATUS

plotdata <- blooddata_long

gplot <- ggplot(plotdata, aes(HIV.status, conc, color = HIV.status)) +
  geom_boxplot(outlier.colour = NA) +
  geom_dotplot(binaxis = 'y',
               stackdir = 'center',
               #binwidth = 0.3,
               alpha = 0.2) +
  facet_wrap(factor(plotdata$biomarker), scale="free_y") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "HIV status", y = "Log2-Transformed Biomarker Concentration") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(2, "lines")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(title = element_text(face = "bold"), axis.title = element_text(face="bold"))
gplot

ggsave("Figures/Blood Biomarkers by HIV.png", gplot, width = 8, height = 7)


############# Linear regression of blood biomarker concentrations by HIV.status

a <- as.character(plotdata$HIV.status)
a[a=="Participants without HIV"] <- -0.5
a[a=="Participants with HIV"] <- 0.5
plotdata$HIV.status <- as.factor(a)
rm(a)


splitdata <- plotdata %>%
  group_by(biomarker) %>%
  group_split

reg.df <- data.frame(
  estimate = character(0),
  r.sq = character(0),
  p.value = character(0)
)

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

biomarkers

reg.df$p.fdr <- p.adjust(reg.df$p.value, method = "fdr")
reg.df$biomarker <- biomarkers
reg.df <- reg.df[c(5,1:4)]

write.csv(reg.df, "Results/Results - Blood Biomarkers by HIV Status.csv", row.names = F)

rm(splitdata, gplot, reg.df)



## BLOOD BIOMARKERS BY PHQ-9 SCORE

plotdata <- blooddata_long

gplot <- ggplot(plotdata, aes(total, conc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(factor(plotdata$biomarker), scale="free_y") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Total PHQ-9 Score", y = "Log2-Transformed Biomarker Concentration") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(2, "lines")) +
  theme(title = element_text(face = "bold"), axis.title = element_text(face="bold"))
gplot

ggsave("Figures/Blood Biomarkers and PHQ9.png", gplot, width = 8, height = 7)


############# Linear regression of blood biomarker concentrations by PHQ-9 score

splitdata <- plotdata %>%
  group_by(biomarker) %>%
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
reg.df$biomarker <- biomarkers
reg.df <- reg.df[c(5,1:4)]

write.csv(reg.df, "Results/Results - Blood Biomarkers by PHQ-9 score.csv", row.names = F)

rm(splitdata, gplot, reg.df)


rm(plotdata, blooddata_long)
