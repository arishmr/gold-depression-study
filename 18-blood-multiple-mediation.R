semdata <- blooddata

## Scale all biomarkers, age, and total PHQ-9 score
semdata[2:16] <- apply(semdata[2:16], 2, scale)
semdata$age <- as.numeric(scale(semdata$age))
semdata$total <- as.numeric(scale(semdata$total))


## MODEL SPECIFICATION

semodel <- '# direct effect
             total        ~ cprime*HIV.status + age + gender
           # mediators predicted by HIV.status
             BDNF         ~ a1*HIV.status + age + gender
             CD14         ~ a2*HIV.status + age + gender
             CD163        ~ a3*HIV.status + age + gender
             CRP          ~ a4*HIV.status + age + gender
             IL1.beta     ~ a5*HIV.status + age + gender
             IL18         ~ a6*HIV.status + age + gender
             IL6          ~ a7*HIV.status + age + gender
             IP10         ~ a8*HIV.status + age + gender
             MCP1         ~ a9*HIV.status + age + gender
             MIG          ~ a10*HIV.status + age + gender
             MIP1.alpha   ~ a11*HIV.status + age + gender
             RANTES       ~ a12*HIV.status + age + gender
             S100A8       ~ a13*HIV.status + age + gender
             TNF.alpha    ~ a14*HIV.status + age + gender
             YKL40        ~ a15*HIV.status + age + gender
           # total PHQ-9 predicted by mediators
             total        ~ b1*BDNF
             total        ~ b2*CD14
             total        ~ b3*CD163
             total        ~ b4*CRP
             total        ~ b5*IL1.beta
             total        ~ b6*IL18
             total        ~ b7*IL6
             total        ~ b8*IP10
             total        ~ b9*MCP1
             total        ~ b10*MIG
             total        ~ b11*MIP1.alpha
             total        ~ b12*RANTES
             total        ~ b13*S100A8
             total        ~ b14*TNF.alpha
             total        ~ b15*YKL40
           # indirect effect (a*b)
             a1b1 := a1*b1
             a2b2 := a2*b2
             a3b3 := a3*b3
             a4b4 := a4*b4
             a5b5 := a5*b5
             a6b6 := a6*b6
             a7b7 := a7*b7
             a8b8 := a8*b8
             a9b9 := a9*b9
             a10b10 := a10*b10
             a11b11 := a11*b11
             a12b12 := a12*b12
             a13b13 := a13*b13
             a14b14 := a14*b14
             a15b15 := a15*b15
             ab := (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8) + (a9*b9) + (a10*b10) +
             (a11*b11) + (a12*b12) + (a13*b13) + (a14*b14) + (a15*b15)
           # total effect
             c1 := cprime + (a1*b1)
             c2 := cprime + (a2*b2)
             c3 := cprime + (a3*b3)
             c4 := cprime + (a4*b4)
             c5 := cprime + (a5*b5)
             c6 := cprime + (a6*b6)
             c7 := cprime + (a7*b7)
             c8 := cprime + (a8*b8)
             c9 := cprime + (a9*b9)
             c10 := cprime + (a10*b10)
             c11 := cprime + (a11*b11)
             c12 := cprime + (a12*b12)
             c13 := cprime + (a13*b13)
             c14 := cprime + (a14*b14)
             c15 := cprime + (a15*b15)
             c := cprime + (a1*b1) + (a2*b2) + (a3*b3) + (a4*b4) + (a5*b5) + (a6*b6) + (a7*b7) + (a8*b8) + (a9*b9) + (a10*b10) +
             (a11*b11) + (a12*b12) + (a13*b13) + (a14*b14) + (a15*b15)
           # proportion mediated
             prop1 := ((c1-cprime)/c1)*100
             prop2 := ((c2-cprime)/c2)*100
             prop3 := ((c3-cprime)/c3)*100
             prop4 := ((c4-cprime)/c4)*100
             prop5 := ((c5-cprime)/c5)*100
             prop6 := ((c6-cprime)/c6)*100
             prop7 := ((c7-cprime)/c7)*100
             prop8 := ((c8-cprime)/c8)*100
             prop9 := ((c9-cprime)/c9)*100
             prop10 := ((c10-cprime)/c10)*100
             prop11 := ((c11-cprime)/c11)*100
             prop12 := ((c12-cprime)/c12)*100
             prop13 := ((c13-cprime)/c13)*100
             prop14 := ((c14-cprime)/c14)*100
             prop15 := ((c15-cprime)/c15)*100
             prop := ((c-cprime)/c)*100
          # covariances
             BDNF ~~ CD14 + CD163 + CRP + IL1.beta + IL18 + IL6 + IP10 + MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             CD14 ~~ CD163 + CRP + IL1.beta + IL18 + IL6 + IP10 + MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             CD163 ~~ CRP + IL1.beta + IL18 + IL6 + IP10 + MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             CRP ~~ IL1.beta + IL18 + IL6 + IP10 + MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             IL1.beta ~~ IL18 + IL6 + IP10 + MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             IL18 ~~ IL6 + IP10 + MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             IL6 ~~ IP10 + MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             IP10 ~~ MCP1 + MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             MCP1 ~~ MIG + MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             MIG ~~ MIP1.alpha + RANTES + S100A8 + TNF.alpha + YKL40
             MIP1.alpha ~~ RANTES + S100A8 + TNF.alpha + YKL40
             RANTES ~~ S100A8 + TNF.alpha + YKL40
             S100A8 ~~ TNF.alpha + YKL40
             TNF.alpha ~~ YKL40
        '

set.seed(123)

fit <- sem(semodel, semdata, se = "bootstrap", missing = "fiml.x", bootstrap = 1000)
output <- parameterEstimates(fit)
View(output)

output <- output[!(output$label==""),4:10]

output$p.fdr <- p.adjust(output$pvalue, method = "fdr")

write.csv(output, "Results/Results - Blood Biomarkers Multiple Mediation.csv", row.names = F)

rm(semdata, fit, output, semodel)




