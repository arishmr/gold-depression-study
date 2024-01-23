###################### BLOOD BIOMARKERS ONLY

## OVERALL CORRELATIONS

cordata <- blooddata[,2:16]

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

diag(cormat) <- NA
mean(abs(cormat), na.rm = T)
sd(abs(cormat), na.rm = T)
range(cormat, na.rm = T)

quantile((abs(cormat)), 0.95, na.rm = T)

cormat <- cor$r

png(file="Figures/Correlations/Blood Biomarker Correlations.png", type = "cairo", res = 300, height = 2800, width = 2800)
corrplot(cormat, type = "lower", method = "color", order = "hclust", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex= 0.75, number.digits = 2)
dev.off()

## HIV+ ONLY

cordata <- blooddata %>%
  filter(HIV.status == "Participants with HIV") %>%
  dplyr::select(BDNF:YKL40)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

png(file="Figures/Correlations/Blood Biomarker Correlations HIV+.png", type = "cairo", res = 300, height = 2800, width = 2800)
corrplot(cormat, method = "color", order = "hclust", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex = 0.75, number.digits = 2)
dev.off()


## HIV- ONLY

cordata <- blooddata %>%
  filter(HIV.status == "Participants without HIV") %>%
  dplyr::select(BDNF:YKL40)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

png(file="Figures/Correlations/Blood Biomarker Correlations HIV-.png", type = "cairo", res = 300, height = 2800, width = 2800)
corrplot(cormat, method = "color", order = "hclust", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex = 0.75, number.digits = 2)
dev.off()


rm(cor, cordata, cormat, corp)





###################### NEUROIMAGING BIOMARKERS ONLY

## OVERALL CORRELATIONS

cordata <- mridata[,2:13]
cordata <- cordata %>% rename(
  "BG Cho:Water" = "BG.Cho",
  "BG Cho:Cr" = "BG.Cho.Cr",
  "BG mI:Water" = "BG.Ins",
  "BG mI:Cr" = "BG.Ins.Cr",
  "MFGM Cho:Water" = "MFGM.Cho",
  "MFGM Cho:Cr" = "MFGM.Cho.Cr",
  "MFGM mI:Water" = "MFGM.Ins",
  "MFGM mI:Cr" = "MFGM.Ins.Cr",
  "PWM Cho:Water" = "PWM.Cho",
  "PWM Cho:Cr" = "PWM.Cho.Cr",
  "PWM mI:Water" = "PWM.Ins",
  "PWM mI:Cr" = "PWM.Ins.Cr",
)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

diag(cormat) <- NA
mean(abs(cormat), na.rm = T)
sd(abs(cormat), na.rm = T)
range(cormat, na.rm = T)

quantile((abs(cormat)), 0.95, na.rm = T)

cormat <- cor$r

png(file="Figures/Correlations/Neuroimaging Biomarker Correlations.png", type = "cairo", res = 300, height = 2800, width = 2800)
corrplot(cormat, type = "lower", method = "color", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex= 0.75, number.digits = 2)
dev.off()

## HIV+ ONLY

cordata <- mridata %>%
  filter(HIV.status == "Participants with HIV") %>%
  dplyr::select(BG.Cho:PWM.Ins.Cr)
cordata <- cordata %>% rename(
  "BG Cho:Water" = "BG.Cho",
  "BG Cho:Cr" = "BG.Cho.Cr",
  "BG mI:Water" = "BG.Ins",
  "BG mI:Cr" = "BG.Ins.Cr",
  "MFGM Cho:Water" = "MFGM.Cho",
  "MFGM Cho:Cr" = "MFGM.Cho.Cr",
  "MFGM mI:Water" = "MFGM.Ins",
  "MFGM mI:Cr" = "MFGM.Ins.Cr",
  "PWM Cho:Water" = "PWM.Cho",
  "PWM Cho:Cr" = "PWM.Cho.Cr",
  "PWM mI:Water" = "PWM.Ins",
  "PWM mI:Cr" = "PWM.Ins.Cr",
)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

png(file="Figures/Correlations/Neuroimaging Biomarker Correlations HIV+.png", type = "cairo", res = 300, height = 2800, width = 2800)
corrplot(cormat, method = "color", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex = 0.75, number.digits = 2)
dev.off()


## HIV- ONLY

cordata <- mridata %>%
  filter(HIV.status == "Participants without HIV") %>%
  dplyr::select(BG.Cho:PWM.Ins.Cr)
cordata <- cordata %>% rename(
  "BG Cho:Water" = "BG.Cho",
  "BG Cho:Cr" = "BG.Cho.Cr",
  "BG mI:Water" = "BG.Ins",
  "BG mI:Cr" = "BG.Ins.Cr",
  "MFGM Cho:Water" = "MFGM.Cho",
  "MFGM Cho:Cr" = "MFGM.Cho.Cr",
  "MFGM mI:Water" = "MFGM.Ins",
  "MFGM mI:Cr" = "MFGM.Ins.Cr",
  "PWM Cho:Water" = "PWM.Cho",
  "PWM Cho:Cr" = "PWM.Cho.Cr",
  "PWM mI:Water" = "PWM.Ins",
  "PWM mI:Cr" = "PWM.Ins.Cr",
)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

png(file="Figures/Correlations/Neuroimaging Biomarker Correlations HIV-.png", type = "cairo", res = 300, height = 2800, width = 2800)
corrplot(cormat, method = "color", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex = 0.75, number.digits = 2)
dev.off()


rm(cor, cordata, cormat, corp)





###################### NEUROIMAGING AND BLOOD BIOMARKERS TOGETHER

## OVERALL CORRELATIONS

cordata <- data.frame(c(blooddata[,2:16], mridata[,2:13]))
cordata <- cordata %>% rename(
  "BG Cho:Water" = "BG.Cho",
  "BG Cho:Cr" = "BG.Cho.Cr",
  "BG mI:Water" = "BG.Ins",
  "BG mI:Cr" = "BG.Ins.Cr",
  "MFGM Cho:Water" = "MFGM.Cho",
  "MFGM Cho:Cr" = "MFGM.Cho.Cr",
  "MFGM mI:Water" = "MFGM.Ins",
  "MFGM mI:Cr" = "MFGM.Ins.Cr",
  "PWM Cho:Water" = "PWM.Cho",
  "PWM Cho:Cr" = "PWM.Cho.Cr",
  "PWM mI:Water" = "PWM.Ins",
  "PWM mI:Cr" = "PWM.Ins.Cr",
)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r
corp <- cor$p
corp[lower.tri(corp)] <- NA
corp <- as.matrix(Matrix::forceSymmetric(corp,uplo="U"))

diag(cormat) <- NA
mean(abs(cormat), na.rm = T)
sd(abs(cormat), na.rm = T)
range(cormat, na.rm = T)

quantile((abs(cormat)), 0.95, na.rm = T)

cormat <- cor$r

png(file="Figures/Correlations/All Biomarker Correlations.png", type = "cairo", res = 300, height = 3600, width = 3600)
corrplot(cormat, type = "lower", method = "color", order = "hclust", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex= 0.75, number.digits = 2)
dev.off()

## HIV+ ONLY

cordata <- data.frame(c(blooddata[,2:16], mridata[,c(2:13,15)])) %>%
  filter(HIV.status == "Participants with HIV") %>%
  dplyr::select(BDNF:YKL40,BG.Cho:PWM.Ins.Cr)
cordata <- cordata %>% rename(
  "BG Cho:Water" = "BG.Cho",
  "BG Cho:Cr" = "BG.Cho.Cr",
  "BG mI:Water" = "BG.Ins",
  "BG mI:Cr" = "BG.Ins.Cr",
  "MFGM Cho:Water" = "MFGM.Cho",
  "MFGM Cho:Cr" = "MFGM.Cho.Cr",
  "MFGM mI:Water" = "MFGM.Ins",
  "MFGM mI:Cr" = "MFGM.Ins.Cr",
  "PWM Cho:Water" = "PWM.Cho",
  "PWM Cho:Cr" = "PWM.Cho.Cr",
  "PWM mI:Water" = "PWM.Ins",
  "PWM mI:Cr" = "PWM.Ins.Cr",
)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

png(file="Figures/Correlations/All Biomarker Correlations HIV+.png", type = "cairo", res = 300, height = 3600, width = 3600)
corrplot(cormat, method = "color", order = "hclust", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex = 0.75, number.digits = 2)
dev.off()


## HIV- ONLY

cordata <- data.frame(c(blooddata[,2:16], mridata[,c(2:13,15)])) %>%
  filter(HIV.status == "Participants without HIV") %>%
  dplyr::select(BDNF:YKL40,BG.Cho:PWM.Ins.Cr)
cordata <- cordata %>% rename(
  "BG Cho:Water" = "BG.Cho",
  "BG Cho:Cr" = "BG.Cho.Cr",
  "BG mI:Water" = "BG.Ins",
  "BG mI:Cr" = "BG.Ins.Cr",
  "MFGM Cho:Water" = "MFGM.Cho",
  "MFGM Cho:Cr" = "MFGM.Cho.Cr",
  "MFGM mI:Water" = "MFGM.Ins",
  "MFGM mI:Cr" = "MFGM.Ins.Cr",
  "PWM Cho:Water" = "PWM.Cho",
  "PWM Cho:Cr" = "PWM.Cho.Cr",
  "PWM mI:Water" = "PWM.Ins",
  "PWM mI:Cr" = "PWM.Ins.Cr",
)

cor <- corr.test(cordata, use = "pairwise", method = "spearman")
cormat <- cor$r

png(file="Figures/Correlations/All Biomarker Correlations HIV-.png", type = "cairo", res = 300, height = 3600, width = 3600)
corrplot(cormat, method = "color", order = "hclust", tl.col = "black", col = COL2("PuOr", 200),
         addgrid.col = 'white', addCoef.col = 'grey50', number.cex = 0.75, number.digits = 2)
dev.off()


rm(cor, cordata, cormat, corp)