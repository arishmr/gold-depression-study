## Use testdata to summarise descriptive statistics for PHQ-9 scores

## LINEAR REGRESSION OF PHQ-9 SCORE WITH HIV STATUS
summary(lm(total ~ HIV.status, data = alldata))
summary(lm(total ~ HIV.status + age + gender, data = alldata))

summary(lm(scale(total) ~ HIV.status, data = alldata))
summary(lm(scale(total) ~ HIV.status + scale(age) + gender, data = alldata))

summary(lm(total ~ gender, data = alldata))
summary(lm(total ~ gender + age, data = alldata))
summary(lm(scale(total) ~ gender, data = alldata))
summary(lm(scale(total) ~ gender + scale(age) + gender, data = alldata))


################# MEDIAN AND IQR FOR FULL SAMPLE

## Median and IQR for total PHQ-9 score
colnames(testdata)
summary(testdata)
# Create a data frame 'desc.overall' containing the median and IQR values of the total PHQ-9 score
desc.overall <- testdata %>%
  summarize(Median = median(.[, 10], na.rm = TRUE),
            Q1 = quantile(.[, 10], 0.25, na.rm = TRUE),
            Q3 = quantile(.[, 10], 0.75, na.rm = TRUE)) %>%
  mutate(label = "Full Sample") %>%
  dplyr::select(label, Median, Q1, Q3)

shapiro.test(testdata$total)


################# BY HIV STATUS
# Create a data frame 'desc.by.hiv' containing median and IQR values of the total PHQ-9 score by HIV.status
# also include the regression coefficient and p value from a linear regression model comparing total PHQ-9 score for the two groups
desc.by.hiv <- testdata %>%
  group_by(HIV.status) %>%
      summarize(Median = quantile(total, probs = 0.5, na.rm = TRUE),
                Q1 = quantile(total, probs = 0.25, na.rm = TRUE),
                Q3 = quantile(total, probs = 0.75, na.rm = TRUE),
                coef = summary(lm(total ~ HIV.status, data = .))$coefficients[2,1],
                p.value = summary(lm(total ~ HIV.status, data = .))$coefficients[2,4])



################# BY LANGUAGE OF ADMINISTRATION

desc.by.lang <- testdata %>%
  group_by(language) %>%
  summarize(Median = quantile(total, probs = 0.5, na.rm = TRUE),
            Q1 = quantile(total, probs = 0.25, na.rm = TRUE),
            Q3 = quantile(total, probs = 0.75, na.rm = TRUE),
            coef = summary(lm(total ~ language, data = .))$coefficients[2,1],
            p.value = summary(lm(total ~ language, data = .))$coefficients[2,4])



output <- list("Overall" = desc.overall,
               "By HIV status" = desc.by.hiv,
               "By Language" = desc.by.lang)
write.xlsx(output, "Results/PHQ-9 Descriptives.xlsx")


rm(desc.by.hiv, desc.by.lang, desc.overall, output)
