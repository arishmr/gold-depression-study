
################# PLOT TOTAL PHQ-9 SCORE BY HIV STATUS

phq9 <- alldata %>% dplyr::select(PID, q1, q2, q3, q4, q5, q6, q7, q8, q9, total,
                                      language, HIV.status)

gplot <- ggplot(phq9, aes(HIV.status, total, color = HIV.status)) +
  geom_boxplot(outlier.colour = NA) +
  geom_dotplot(aes(fill = HIV.status), binaxis = 'y',
               stackdir = 'center',
               binwidth = 0.3,
               alpha = 0.2) +
  #geom_jitter(position = position_jitter(0.1)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "HIV status", y = "Total PHQ-9 Score") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(title = element_text(face = "bold"), axis.title = element_text(face="bold"))
gplot
ggsave("Figures/PHQ-9 by HIV Status.png", gplot, width = 4, height = 4)

rm(gplot)

## Linear regression to test for difference in total PHQ-9 score by HIV status

summary(lm(total ~ HIV.status, data = phq9))



################# PLOT TOTAL PHQ-9 SCORE BY LANGUAGE OF ADMINISTRATION

gplot <- ggplot(phq9, aes(language, total, color = language)) +
  geom_boxplot(outlier.colour = NA) +
  geom_dotplot(aes(fill = language), binaxis = 'y',
               stackdir = 'center',
               binwidth = 0.3,
               alpha = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language of Administration", y = "Total PHQ-9 Score") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(title = element_text(face = "bold"), axis.title = element_text(face="bold"))
gplot
ggsave("Figures/PHQ-9 by Language.png", gplot, width = 4, height = 4)

rm(gplot)

## Linear regression to test for difference in total PHQ-9 score by language of administration

summary(lm(total ~ language, data = phq9))





################# PLOT ITEM-LEVEL PHQ-9 SCORES BY HIV STATUS

phq9.st <- stack(phq9[,c(2:10)])
names(phq9.st)[1] <- 'score'
names(phq9.st)[2] <- 'question'
phq9.st$HIV.status <- rep(phq9$HIV.status, 9)
phq9.st$question <- as.character(phq9.st$question)
phq9.st[phq9.st=="q1"] <- items[1]
phq9.st[phq9.st=="q2"] <- items[2]
phq9.st[phq9.st=="q3"] <- items[3]
phq9.st[phq9.st=="q4"] <- items[4]
phq9.st[phq9.st=="q5"] <- items[5]
phq9.st[phq9.st=="q6"] <- items[6]
phq9.st[phq9.st=="q7"] <- items[7]
phq9.st[phq9.st=="q8"] <- items[8]
phq9.st[phq9.st=="q9"] <- items[9]

gplot <- ggplot(phq9.st, aes(HIV.status, score, color = HIV.status)) +
  geom_boxplot(outlier.colour = NA) +
  ylim(0,3.5) +
  geom_dotplot(aes(fill = HIV.status), binaxis = 'y',
               stackdir = 'center',
               binwidth = 0.1,
               alpha = 0.2) +
  facet_wrap(~factor(question, levels = items), scale="free") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "HIV Status", y = "Score") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(title = element_text(face = "bold"), axis.title = element_text(face="bold"))
gplot

ggsave("Figures/PHQ-9 Items by HIV Status.png", gplot, width = 8, height = 8)
rm(gplot)



################# RUN LINEAR REGRESSION FOR ITEM-LEVEL PHQ-9 SCORES BY HIV STATUS

lm.data <- split(phq9.st, phq9.st$question)
lm.results <- lapply(lm.data, function(x) {summary(lm(score ~ HIV.status, data = x))})
coef <- lapply(lm.results, function(x) {print(x$coefficients[2])})
coef <- as.numeric(coef)
p.values <- lapply(lm.results, function(x) {print(x)$coefficients[2,4]})
p.values <- as.numeric(p.values)
p.fdr <- p.adjust(p.values, method = "fdr")
lm.results <- data.frame(Item = items,
                       coef = coef,
                       p.value = p.values,
                       p.fdr = p.fdr)


write.csv(lm.results, "Results/Results - PHQ-9 Items by HIV Status.csv", row.names = F)

rm(lm.data, lm.results, phq9.st, coef, p.values, p.fdr, phq9)



################# PLOT ITEM-LEVEL PHQ-9 SCORES AS A HEATMAP

## Determine Spearman's r for all item-level median scores with all others
phq9 <- alldata %>% dplyr::select(PID, q1, q2, q3, q4, q5, q6, q7, q8, q9, total)
cor <- cor(phq9, use = "complete.obs", method = "spearman")
write.csv(cor, "Results/Results - PHQ-9 Spearman's R.csv")

## Generate heatmap
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
melted.cor <- melt(cor)
upper_tri <- get_upper_tri(cor)
melted.cor <- melt(upper_tri, na.rm = TRUE)

gplot <- ggplot(data = melted.cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "gray") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal() +
  labs(x = "PHQ-9 Item", y = "PHQ-9 Item", title = "All Participants") +
  coord_fixed()
gplot

ggsave("Figures/PHQ-9 Correlations.png", gplot, width = 8, height = 8)


## Determine Spearman's r for item-level median scores with all others BY HIV STATUS
phq9 <- alldata %>% dplyr::select(PID, q1, q2, q3, q4, q5, q6, q7, q8, q9, total,
                                  HIV.status)
corrdata <- split(phq9, phq9$HIV.status)
corrdata <- lapply(corrdata, function (x) {x <- x[,c(1:10)]})
corrdata <- lapply(corrdata, function (x) {cor(x, use = "complete.obs", method = "spearman")})
cor.pos <- corrdata[[1]]
cor.neg <- corrdata[[2]]

write.csv(cor.pos, "Results/Results - PHQ-9 HIV+ Spearman's R.csv")
write.csv(cor.neg, "Results/Results - PHQ-9 HIV- Spearman's R.csv")

## Generate heatmaps
melted.cor <- melt(cor.pos)
upper_tri <- get_upper_tri(cor.pos)
melted.cor <- melt(upper_tri, na.rm = TRUE)

gplot <- ggplot(data = melted.cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "gray") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal() +
  labs(x = "PHQ-9 Item", y = "PHQ-9 Item", title = "Participants with HIV") +
  coord_fixed()
gplot
ggsave("Figures/PHQ-9 HIV+ Correlations.png", gplot, width = 8, height = 8)

melted.cor <- melt(cor.neg)
upper_tri <- get_upper_tri(cor.neg)
melted.cor <- melt(upper_tri, na.rm = TRUE)

gplot <- ggplot(data = melted.cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "gray") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal() +
  labs(x = "PHQ-9 Item", y = "PHQ-9 Item", title = "Participants without HIV") +
  coord_fixed()
gplot
ggsave("Figures/PHQ-9 HIV- Correlations.png", gplot, width = 8, height = 8)


rm(cor, corrdata, cor.neg, cor.pos, upper_tri,  melted.cor, gplot, get_upper_tri, phq9)









