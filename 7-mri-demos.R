################# SUMMARISE DEMOGRAPHICS BY HIV STATUS FOR ALL PARTICIPANTS INCLUDED IN MRI ANALYSES
mridemo <- mridata[1]
mridemo <- unique(mridemo)
mridemo <- merge(mridemo, demodata, by = 'PID', all.x = T, all.y = F)
mridemo <- mridemo %>%
  dplyr::select(PID, HIV.status,
                age, gender, ethnicity, sexual.orient,
                alcohol, smoking, recdrugs)
colnames(mridemo)

a <- filter(mridemo, mridemo$HIV.status == "Participants with HIV")
sum(a$viral.load.copies <= 200)
a <- sum(a$viral.load.copies <= 200)/length(a$viral.load.copies)*100
a
rm(a)

print(fisher.test(mridemo$alcohol, mridemo$HIV.status))
print(fisher.test(mridemo$smoking, mridemo$HIV.status))
print(fisher.test(mridemo$recdrugs, mridemo$HIV.status))
print(chisq.test(mridemo$sexual.orient, mridemo$HIV.status))

## Create gt_summary table by HIV status

tab1 <- tbl_summary(mridemo,
                    by = HIV.status,
                    type = list(age ~ "continuous"),
                    statistic = list(
                      all_continuous() ~ "{median} ({p25}, {p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 1,
                    label = list(
                      age ~ "Age (years)",
                      gender ~ "Gender",
                      ethnicity ~ "Ethnicity",
                      sexual.orient ~ "Sexual orientation",
                      alcohol ~ "Alcohol use",
                      smoking ~ "Cigarette smoking",
                      recdrugs ~ "Recreational drug use"
                    ),
                    missing_text = "(Missing)"
) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label() %>%
  add_significance_stars()



tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Results - MRS-All Demographics by HIV Status.docx",
             pr_section = prop_section(page_size(orient = "landscape")))

rm(tab1, tab1.print)



## Extract dataframes for each MRI brain region
bg <- mridata %>% drop_na(BG.Cho)
bg <- merge(bg, mridemo) %>%
  dplyr::select(age, gender, ethnicity, sexual.orient,
                alcohol, smoking, recdrugs,
                HIV.status)
mfgm <- mridata %>% drop_na(MFGM.Cho)
mfgm <- merge(mfgm, mridemo) %>%
  dplyr::select(age, gender, ethnicity, sexual.orient,
                alcohol, smoking, recdrugs,
                HIV.status)
pwm <- mridata %>% drop_na(PWM.Cho)
pwm <- merge(pwm, mridemo) %>%
  dplyr::select(age, gender, ethnicity, sexual.orient,
                alcohol, smoking, recdrugs,
                HIV.status)


################# SUMMARISE DEMOGRAPHICS BY HIV STATUS - CREATE FUNCTION TO RUN FOR ALL THREE DATAFRAMES

demofunc <- function(x) {

print(fisher.test(x$alcohol, x$HIV.status))
print(fisher.test(x$smoking, x$HIV.status))
print(fisher.test(x$recdrugs, x$HIV.status))
print(chisq.test(x$sexual.orient, x$HIV.status))

## Create gt_summary table by HIV status

tab1 <- tbl_summary(x,
                    by = HIV.status,
                    type = list(age ~ "continuous"),
                    statistic = list(
                      all_continuous() ~ "{median} ({p25}, {p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 1,
                    label = list(
                      age ~ "Age (years)",
                      gender ~ "Gender",
                      ethnicity ~ "Ethnicity",
                      sexual.orient ~ "Sexual orientation",
                      alcohol ~ "Alcohol use",
                      smoking ~ "Cigarette smoking",
                      recdrugs ~ "Recreational drug use"
                    ),
                    missing_text = "(Missing)"
) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label() %>%
  add_significance_stars()



tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Results - MRS-PWM Demographics by HIV Status.docx",
             pr_section = prop_section(page_size(orient = "landscape")))
}

## Create tab1.print for each dataframe - REMEMBER TO RENAME OUTPUT FILE IN FUNCTION, AND UNCOMMENT THE CORRECT LINE WHEN SAVING!!
#demofunc(bg)
#demofunc(mfgm)
demofunc(pwm)


rm(demofunc, bg, mfgm, pwm)
