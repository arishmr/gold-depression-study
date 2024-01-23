semdata <- mridata

## Scale all biomarkers, age, and total PHQ-9 score
semdata[2:13] <- apply(semdata[2:13], 2, scale)
semdata$age <- as.numeric(scale(semdata$age))
semdata$total <- as.numeric(scale(semdata$total))
semdata$diff <- as.numeric(scale(semdata$diff))


###################### WATER REFERENCING

## MODEL SPECIFICATION

semodel <- '# direct effect
             total        ~ cprime*HIV.status + age + gender
           # mediators predicted by HIV.status
             BG.Cho       ~ a1*HIV.status + age + gender + diff
             BG.Ins       ~ a2*HIV.status + age + gender + diff
             MFGM.Cho     ~ a3*HIV.status + age + gender + diff
             MFGM.Ins     ~ a4*HIV.status + age + gender + diff
             PWM.Cho      ~ a5*HIV.status + age + gender + diff
             PWM.Ins      ~ a6*HIV.status + age + gender + diff
           # total PHQ-9 predicted by mediators
             total        ~ b1*BG.Cho
             total        ~ b2*BG.Ins
             total        ~ b3*MFGM.Cho
             total        ~ b4*MFGM.Ins
             total        ~ b5*PWM.Cho
             total        ~ b6*PWM.Ins
           # indirect effect (a*b)
             a1b1 := a1*b1
             a2b2 := a2*b2
             a3b3 := a3*b3
             a4b4 := a4*b4
             a5b5 := a5*b5
             a6b6 := a6*b6
             ab := (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6)
           # total effect
             c1 := cprime + (a1*b1)
             c2 := cprime + (a2*b2)
             c3 := cprime + (a3*b3)
             c4 := cprime + (a4*b4)
             c5 := cprime + (a5*b5)
             c6 := cprime + (a6*b6)
             c := cprime + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6)
           # proportion mediated
             prop1 := ((c1-cprime)/c1)*100
             prop2 := ((c2-cprime)/c2)*100
             prop3 := ((c3-cprime)/c3)*100
             prop4 := ((c4-cprime)/c4)*100
             prop5 := ((c5-cprime)/c5)*100
             prop6 := ((c6-cprime)/c6)*100
             prop := ((c-cprime)/c)*100
          # covariances
             BG.Cho ~~ BG.Ins + MFGM.Cho + MFGM.Ins + PWM.Cho + PWM.Ins
             BG.Ins ~~ MFGM.Cho + MFGM.Ins + PWM.Cho + PWM.Ins
             MFGM.Cho ~~ MFGM.Ins + PWM.Cho + PWM.Ins
             MFGM.Ins ~~ PWM.Cho + PWM.Ins
             PWM.Cho ~~ PWM.Ins
        '

set.seed(123)

fit <- sem(semodel, semdata, se = "bootstrap", missing = "fiml.x", bootstrap = 1000)
output <- parameterEstimates(fit)
View(output)

output <- output[!(output$label==""),4:10]

output$p.fdr <- p.adjust(output$pvalue, method = "fdr")

write.csv(output, "Results/Results - MRS Biomarkers Multiple Mediation Water.csv", row.names = F)

rm(fit, output, semodel)




###################### CREATINE REFERENCING

## MODEL SPECIFICATION

semodel <- '# direct effect
             total        ~ cprime*HIV.status + age + gender
           # mediators predicted by HIV.status
             BG.Cho.Cr       ~ a1*HIV.status + age + gender + diff
             BG.Ins.Cr       ~ a2*HIV.status + age + gender + diff
             MFGM.Cho.Cr     ~ a3*HIV.status + age + gender + diff
             MFGM.Ins.Cr     ~ a4*HIV.status + age + gender + diff
             PWM.Cho.Cr      ~ a5*HIV.status + age + gender + diff
             PWM.Ins.Cr      ~ a6*HIV.status + age + gender + diff
           # total PHQ-9 predicted by mediators
             total        ~ b1*BG.Cho.Cr
             total        ~ b2*BG.Ins.Cr
             total        ~ b3*MFGM.Cho.Cr
             total        ~ b4*MFGM.Ins.Cr
             total        ~ b5*PWM.Cho.Cr
             total        ~ b6*PWM.Ins.Cr
           # indirect effect (a*b)
             a1b1 := a1*b1
             a2b2 := a2*b2
             a3b3 := a3*b3
             a4b4 := a4*b4
             a5b5 := a5*b5
             a6b6 := a6*b6
             ab := (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6)
           # total effect
             c1 := cprime + (a1*b1)
             c2 := cprime + (a2*b2)
             c3 := cprime + (a3*b3)
             c4 := cprime + (a4*b4)
             c5 := cprime + (a5*b5)
             c6 := cprime + (a6*b6)
             c := cprime + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6)
           # proportion mediated
             prop1 := ((c1-cprime)/c1)*100
             prop2 := ((c2-cprime)/c2)*100
             prop3 := ((c3-cprime)/c3)*100
             prop4 := ((c4-cprime)/c4)*100
             prop5 := ((c5-cprime)/c5)*100
             prop6 := ((c6-cprime)/c6)*100
             prop := ((c-cprime)/c)*100
          # covariances
             BG.Cho.Cr ~~ BG.Ins.Cr + MFGM.Cho.Cr + MFGM.Ins.Cr + PWM.Cho.Cr + PWM.Ins.Cr
             BG.Ins.Cr ~~ MFGM.Cho.Cr + MFGM.Ins.Cr + PWM.Cho.Cr + PWM.Ins.Cr
             MFGM.Cho.Cr ~~ MFGM.Ins.Cr + PWM.Cho.Cr + PWM.Ins.Cr
             MFGM.Ins.Cr ~~ PWM.Cho.Cr + PWM.Ins.Cr
             PWM.Cho.Cr ~~ PWM.Ins.Cr
        '

set.seed(123)

fit <- sem(semodel, semdata, se = "bootstrap", missing = "fiml.x", bootstrap = 1000)
output <- parameterEstimates(fit)
View(output)

output <- output[!(output$label==""),4:10]

output$p.fdr <- p.adjust(output$pvalue, method = "fdr")

write.csv(output, "Results/Results - MRS Biomarkers Multiple Mediation Creatine.csv", row.names = F)

rm(fit, output, semodel)


