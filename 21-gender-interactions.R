################## GOAL: DETERMINE WHETHER ANY BIOMARKERS INTERACT SIGNIFICANTLY WITH GENDER FOR PATH A AND PATH B

## before running this script - run "mri-data" and "biomarkerdata" scripts to get processed and cleaned biomarker values
## now: merge all biomarkers into one dataframe with PID, age, gender, HIV.status, and total score

data <- alldata %>%
  dplyr::select(PID, HIV.status, total, age, gender)
data1 <- blooddata %>%
  dplyr::select(1:16)
data2 <- mridata %>%
  dplyr::select(1:13)
merge <- merge(data, data1)
data <- merge(merge, data2)
rm(merge, data1, data2)

data[,c(3,4,6:32)] <- apply(data[,c(3,4,6:32)], 2, scale)

## create long-form dataframe
data <- gather(data,
               key = "biomarker",
               value = "conc",
               BDNF:PWM.Ins.Cr,
               factor_key = T)
data <- na.omit(data)

splitdata <- data %>%
  group_by(biomarker) %>%
  group_split()

library(plyr, include.only = c("rbind.fill"))

################### Path a: biomarker concentration predicted by HIV status

output_a <- data.frame(
  estimate = numeric(0),
  r.sq = numeric(0),
  p.value = numeric(0)
)

output_a <- lapply(splitdata, FUN = function(x) {
  fit <- lm(conc ~ HIV.status + age + gender + HIV.status*gender, data = x)
  summary(fit)
  estimate <- as.numeric(coef(fit)[5])
  r.sq <- as.numeric(summary(fit)$adj.r.squared)
  p.value <- as.numeric(summary(fit)$coefficients[5,4])
  output_a[nrow(output_a)+1,] <- c(estimate, r.sq, p.value)
  output_a
})
output_a <- rbind.fill(output_a)

output_a$p.fdr <- p.adjust(output_a$p.value, method = "fdr")
output_a$biomarker <- unlist(lapply(splitdata, function (x) {c(unique(x$biomarker))}))
output_a$path <- rep("path a")
output_a <- output_a[c(5,6,1:4)]


################### Path b: PHQ-9 score predicted by biomarker concentration

output_b <- data.frame(
  estimate = numeric(0),
  r.sq = numeric(0),
  p.value = numeric(0)
)

output_b <- lapply(splitdata, FUN = function(x) {
  fit <- lm(total ~ conc + age + gender + conc*gender, data = x)
  summary(fit)
  estimate <- as.numeric(coef(fit)[5])
  r.sq <- as.numeric(summary(fit)$adj.r.squared)
  p.value <- as.numeric(summary(fit)$coefficients[5,4])
  output_b[nrow(output_b)+1,] <- c(estimate, r.sq, p.value)
  output_b
})
output_b <- rbind.fill(output_b)

output_b$p.fdr <- p.adjust(output_b$p.value, method = "fdr")
output_b$biomarker <- unlist(lapply(splitdata, function (x) {c(unique(x$biomarker))}))
output_b$path <- rep("path b")
output_b <- output_b[c(5,6,1:4)]


## Produce output csv
output <- rbind(output_a, output_b)
write.csv(output, "Results/Results - Gender Interactions.csv", row.names = F)

rm(splitdata, output, output_a, output_b, data, mridata_long)
