## Use demodata and testdata to summarise demographic characteristics by HIV status and by language of PHQ-9 administration

data <- alldata %>%
  dplyr::select(
    HIV.status, age, gender,
    total,
    BG.Cho:PWM.Ins.Cr,
    BDNF:YKL40
  )

plotdata <- data %>%
  gather(key = "variable",
         value = "value",
         total:YKL40,
         factor_key = T)

ggplot(plotdata, aes(value)) +
  stat_density() +
  facet_wrap(~ variable, scale = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Figures/Variable Distributions.png", scale = 2, bg = "white")

rm(data, plotdata)

################# SUMMARISE DEMOGRAPHICS BY HIV STATUS

## Shapiro-Wilk tests of normality for numeric variables
shapiro.test(demodata$age)                 ## p < 0.001, so not normally distributed
shapiro.test(demodata$viral.load.log)      ## p < 0.001, so not normally distributed
shapiro.test(demodata$viral.load.copies)   ## p < 0.001, so not normally distributed
shapiro.test(demodata$CD4)                 ## p > 0.05, so normally distributed
shapiro.test(demodata$CD4.abs)             ## p > 0.05, so normally distributed

wilcox.test(demodata$age ~ demodata$HIV.status)

fisher.test(demodata$alcohol, demodata$HIV.status)
fisher.test(demodata$smoking, demodata$HIV.status)
fisher.test(demodata$recdrugs, demodata$HIV.status)
chisq.test(demodata$sexual.orient, demodata$HIV.status)

a <- filter(demodata, demodata$HIV.status == "Participants with HIV")
a <- sum(a$viral.load.copies <= 200)/length(a$viral.load.copies)
rm(a)

## Create gt_summary table by HIV status

tab1 <- tbl_summary(demodata,
            by = HIV.status,
            type = list(age ~ "continuous",
                        grade ~ "categorical",
                        viral.load.log ~ "continuous",
                        viral.load.copies ~ "continuous",
                        CD4 ~ "continuous",
                        CD4.abs ~ "continuous",
                        regimen ~ "categorical"),
            statistic = list(
              age ~ "{median} ({p25}, {p75})",
              viral.load.log ~ "{median} ({p25}, {p75})",
              viral.load.copies ~ "{median} ({p25}, {p75})",
              CD4 ~ "{median} ({p25}, {p75})",
              CD4.abs ~ "{median} ({p25}, {p75})",
              all_categorical() ~ "{n} ({p}%)"
            ),
            digits = all_continuous() ~ 1,
            label = list(
              grade ~ "Grade in School",
              age ~ "Age (years)",
              gender ~ "Gender",
              ethnicity ~ "Ethnicity",
              sexual.orient ~ "Sexual orientation",
              alcohol ~ "Alcohol use",
              smoking ~ "Cigarette smoking",
              recdrugs ~ "Recreational drug use",
              language ~ "Language of Administration",
              viral.load.log ~ "Viral Load (log)",
              viral.load.copies ~ "Viral Load (copies/mL)",
              CD4 ~ "CD4+ T-cell count (cells/µL)",
              CD4.abs ~ "Absolute CD4+ T-cell count (cells/µL)",
              regimen ~ "Antiretroviral Therapy Regimen"
            ),
            missing_text = "(Missing)"
) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label() %>%
  add_significance_stars()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Results - Demographics by HIV Status.docx",
             pr_section = prop_section(page_size(orient = "landscape")))



################# SUMMARISE PHQ-9 BY HIV STATUS


## Clean data codes
testdata <- na.omit(testdata)
testdata[testdata=="2 - Xhosa"] <- "isiXhosa"
testdata[testdata=="0 - English"] <- "English"
testdata[testdata=="1 - Positive"] <- "Participants with HIV"
testdata[testdata=="2 - Negative"] <- "Participants without HIV"
testdata[testdata==""] <- NA

## Create gt_summary table by HIV status

tab1 <- tbl_summary(testdata,
                    by = HIV.status,
                    type = list(
                      total ~ "continuous",
                      q1 ~ "continuous",
                      q2 ~ "continuous",
                      q3 ~ "continuous",
                      q4 ~ "continuous",
                      q5 ~ "continuous",
                      q6 ~ "continuous",
                      q7 ~ "continuous",
                      q8 ~ "continuous",
                      q9 ~ "continuous"),
                    statistic = list(
                      all_continuous() ~ "{median} ({p25}, {p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 2,
                    label = list(
                      total ~ "Total Score",
                      language ~ "Language of Administration"
                    ),
                    missing_text = "(Missing)"
) %>%
  add_p(total ~ "wilcox.test") %>%
  add_overall() %>%
  add_stat_label() %>%
  add_significance_stars()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Results - PHQ-9 by HIV Status.docx",
             pr_section = prop_section(page_size(orient = "landscape")))

## create gt_summary table by language of administration

#tab1 <- tbl_summary(testdata,
#            by = language,
#            type = list(
#              total ~ "continuous",
#              q1 ~ "continuous",
#              q2 ~ "continuous",
#              q3 ~ "continuous",
#              q4 ~ "continuous",
#              q5 ~ "continuous",
#              q6 ~ "continuous",
#              q7 ~ "continuous",
#              q8 ~ "continuous",
#              q9 ~ "continuous"),
#            statistic = list(
#              all_continuous() ~ "{median} ({p25}, {p75})",
#              all_categorical() ~ "{n} ({p}%)"
#            ),
#            digits = all_continuous() ~ 2,
#            label = list(
#              total ~ "Total Score",
#              HIV.status ~ "HIV Status"
#            ),
#            missing_text = "(Missing)"
#) %>%
#  add_p() %>%
#  add_n() %>%
#  add_overall() %>%
#  add_stat_label() %>%
#  add_significance_stars() %>%
#  as_flex_table()


rm(tab1, tab1.print)
