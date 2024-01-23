################ EXTRACT MRIDATA SEPARATED FOR REFERENCING PROTOCOLS

colnames(mridata)

mridata_long <- gather(
  data = mridata,
  key = biomarker,
  value = conc,
  BG.Cho:PWM.Ins.Cr,
  factor_key = T
)

mridata_long$region <- rep(c("Basal Ganglia", "MidFrontal Gray Matter", "Peritrigonal White Matter"),
                           times = 1,
                           each = 240)

mridata_long$referencing <- rep(c("Water Referenced", "Creatine Referenced"),
                           times = 6,
                           each = 60)

mridata_long$biomarker <- rep(c("Choline", "Myo-Inositol"),
                                times = 3,
                                each = 120)

mridata_long <- na.omit(mridata_long)


############################## LINEAR REGRESSION OF TOTAL PHQ-9 AND HIV STATUS
a <- mridata
a <- a %>% filter(!is.na(BG.Cho) | !is.na(MFGM.Cho) | !is.na(PWM.Cho))


## Calculate median (IQR) absolute time difference and total PHQ-9 score for participants with MRI data
median(a$diff)
quantile(a$diff, 0.25)
quantile(a$diff, 0.75)
median(a$total)
quantile(a$total, 0.25)
quantile(a$total, 0.75)


fit <- lm(total ~ HIV.status, data = a)
summary(fit)
fit <- lm(total ~ HIV.status + age + gender, data = a)
summary(fit)


rm(fit, a)




