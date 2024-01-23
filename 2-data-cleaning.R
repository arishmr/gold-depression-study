rm(list = ls())

## Load datasets
pids <- read.csv("Data/Final_PIDs.csv")
demos <- read.csv("Data/Demos.csv")
phq9 <- read.csv("Data/PHQ9.csv")
arm <- read.csv("Data/PID_arm.csv")
labs <- read.csv("Data/Labs.csv")
arv <- read.csv("Data/ARVs.csv")

## Rename columns in all datasets
colnames(demos)
names(demos)[1] <- 'PID'
names(demos)[3] <- 'age'
names(demos)[4] <- 'grade'
names(demos)[5] <- 'gender'
names(demos)[6] <- 'other.gender'
names(demos)[7] <- 'ethnicity'
names(demos)[8] <- 'other.ethnicity'
names(demos)[9] <- 'sexual.orient'
names(demos)[10] <- 'other.sexual.orient'
names(demos)[11] <- 'alcohol'
names(demos)[12] <- 'alc.quant'
names(demos)[13] <- 'smoking'
names(demos)[14] <- 'cig.quant'
names(demos)[15] <- 'recdrugs'
names(demos)[16] <- 'language'
colnames(demos)

colnames(phq9)
names(phq9)[1] <- 'PID'
names(phq9)[3] <- 'q1'
names(phq9)[4] <- 'q2'
names(phq9)[5] <- 'q3'
names(phq9)[6] <- 'q4'
names(phq9)[7] <- 'q5'
names(phq9)[8] <- 'q6'
names(phq9)[9] <- 'q7'
names(phq9)[10] <- 'q8'
names(phq9)[11] <- 'q9'
names(phq9)[13] <- 'bonus.q'
names(phq9)[15] <- 'total'
colnames(phq9)

colnames(arm)
names(arm)[1] <- 'PID'
names(arm)[2] <- 'HIV.status'

colnames(pids)
names(pids)[1] <- 'PID'
names(pids)[2] <- 'Included'

colnames(labs)
names(labs)[1] <- 'PID'
names(labs)[3] <- 'instance'
names(labs)[4] <- 'date'
names(labs)[6] <- 'viral.load.log'
names(labs)[8] <- 'viral.load.copies'
names(labs)[11] <- 'CD4'
names(labs)[13] <- 'CD4.abs'

colnames(arv)
names(arv)[1] <- 'PID'
names(arv)[3] <- 'arv.instance'
names(arv)[6] <- 'regimen'
names(arv)[7] <- 'dose.p.day'


### ARV - for each PID, select only the case for the latest "instance"
arv <- do.call("rbind", 
               by(arv, INDICES=arv$PID, FUN=function(DF) DF[which.max(DF$arv.instance), ]))
arv <- arv[c(1,3,6,7)]

################# NOW FOR LABS - a bit more complicated

## The problem here is: we have repeat measurements for participants, of variable numbers, and not all variables contained in the latest measurement.
## What we want: the latest value for each of the four relevant variables, even if they are not on the same date/instance.
## A possible solution: split the dataframe into four dataframes involving one of each of the variables, then filter to remove NA values for that variable, then select the max instance where multiple measurements exist for a participant, then stitch the four dataframes back together by PID

labs <- filter(labs, labs[5] == "Yes")
labs <- labs[c(1,3,4,6,8,11,13)]

colnames(labs)

## Split a separate dataframe with just viral load, then filter it removing NA VLs and finally select the max instance where multiple instances exist
labs.vl <- labs[c(1:3,
                  4)]
labs.vl.copies <- labs[c(1:3,
                         5)]
labs.cd4 <- labs[c(1:3,
                   6)]
labs.cd4.abs <- labs[c(1:3,
                       7)]
labs.split <- list(labs.vl, labs.vl.copies, labs.cd4, labs.cd4.abs)

labs.split <- lapply(labs.split,
                     function (x) {x <- na.omit(x)
                     x <- do.call("rbind", 
                                  by(x, INDICES=x$PID, FUN=function(DF) DF[which.max(DF$instance), ]))})

labs.vl <- labs.split[[1]]
labs.vl.copies <- labs.split[[2]]
labs.cd4 <- labs.split[[3]]
labs.cd4.abs <- labs.split[[4]]

merge1 <- merge(labs.vl, labs.vl.copies, by = "PID", all.x = T, all.y = T)
names(merge1)[2] <- 'vl.instance'
names(merge1)[3] <- 'vl.date'
names(merge1)[5] <- 'vl.copies.instance'
names(merge1)[6] <- 'vl.copies.date'
merge1 <- merge(merge1, labs.cd4, by = "PID", all.x = T, all.y = T)
names(merge1)[8] <- 'cd4.instance'
names(merge1)[9] <- 'cd4.date'
merge1 <- merge(merge1, labs.cd4.abs, by = "PID", all.x = T, all.y = T)
names(merge1)[11] <- 'cd4.abs.instance'
names(merge1)[12] <- 'cd4.abs.date'
colnames(merge1)

labs <- merge1

rm(labs.vl, labs.vl.copies, labs.cd4, labs.cd4.abs, labs.split, merge1)

############### NOW - WE CAN START TO STITCH ALL THESE VARIABLES TOGETHER

## Merge datasets - get PHQ-9, labs, arv, and demo into same csv, then filter by participants who completed the study
merge1 <- merge(demos, phq9,  by = "PID", all.x = T, all.y = T)
merge1 <- merge(merge1, arm,   by = "PID", all.x = T, all.y = FALSE)
merge1 <- merge(merge1, pids, by = "PID", all.x = T, all.y = T)
merge1 <- merge(merge1, labs, by = "PID", all.x = T, all.y = T)
merge1 <- merge(merge1, arv, by = "PID", all.x = T, all.y = T)
colnames(merge1)
rm(arm)

alldata <- merge1[,c(1,3:16,19:27,29,31,33,35,36:50)]
## two PIDs: 109 and 135, are excluded because the PHQ-9 was delivered in Afrikaans
alldata <- filter(alldata, Included == "Complete")

## Clean data codes and ensure correct variable types

alldata <- alldata %>% mutate(grade = as.numeric(gsub("Grade ", "", grade)))
alldata$language <- ifelse(alldata$language == "2 - Xhosa", "isiXhosa", ifelse(alldata$language=="0 - English", "English", "Afrikaans"))
alldata$HIV.status <- ifelse(alldata$HIV.status == "1 - Positive", "Participants with HIV", "Participants without HIV")
alldata$gender <- ifelse(alldata$gender == "1 - Girl", "Girl", "Boy")
alldata$ethnicity <- ifelse(alldata$ethnicity == "0 - Black/African", "Black/African", "Coloured")
alldata[alldata=="0- Straight/Heterosexual"] <- "Straight/Heterosexual"
alldata[alldata=="1- Gay/Lesbian/Homosexual"] <- "Gay/Lesbian/Homosexual"
alldata[alldata=="2- Bisexual/Pansexual"] <- "Bisexual/Pansexual"

alldata$age <- as.numeric(alldata$age)
alldata$gender <- as.factor(alldata$gender)
alldata$ethnicity <- as.factor(alldata$ethnicity)
alldata$sexual.orient <- as.factor(alldata$sexual.orient)
alldata$alcohol <- as.factor(alldata$alcohol)
alldata$smoking <- as.factor(alldata$smoking)
alldata$recdrugs <- as.factor(alldata$recdrugs)
alldata$HIV.status <- factor(alldata$HIV.status)
alldata$HIV.status <- relevel(alldata$HIV.status, ref = "Participants without HIV")
alldata$language <- as.factor(alldata$language)
alldata$language <- relevel(alldata$language, ref = "English")
alldata$regimen <- as.factor(alldata$regimen)

alldata <- alldata %>%
  relocate(HIV.status, .after = PID)

colnames(alldata)



items <- c("Anhedonia", "Depressed Mood", "Sleep Changes", "Loss of Energy", "Appetite Changes", "Low Self-Esteem", "Trouble Concentrating", "Psychomotor Changes", "Suicide Ideation")


rm(merge1, demos, phq9, pids, labs, arv)



################# LOAD DEMO DATASET AND TRANSFORM DATES TO USABLE FORMAT

demos <- read.csv("Data/Demos.csv")
names(demos)[1] <- 'PID'
names(demos)[2] <- 'date'
demos <- demos[,c(1,2)]
demos$demos.date <- strptime(as.character(demos$date), "%d/%m/%Y")
demos$demos.date <- format(demos$demos.date, "%Y/%m/%d")
demos <- demos[,c(1,3)]


################# LOAD PHQ-9 DATASET AND TRANSFORM DATES TO USABLE FORMAT

phq9 <- read.csv("Data/PHQ9.csv")
names(phq9)[1] <- 'PID'
names(phq9)[2] <- 'date'
phq9 <- phq9[,c(1,2)]
phq9$phq9.date <- strptime(as.character(phq9$date), "%d/%m/%Y")
phq9$phq9.date <- format(phq9$phq9.date, "%Y/%m/%d")
phq9 <- phq9[,c(1,3)]


################# LOAD MRI DATES AND TRANSFORM TO USABLE FORMAT

mri <- read.csv("Data/2023_scheduler.csv")
colnames(mri)
names(mri)[3] <- 'PID'
names(mri)[4] <- 'date1'
names(mri)[16] <- 'date2'
mri <- mri[,c(3,4,16)]
mri$mri.date1 <- strptime(as.character(mri$date1), "%d/%m/%Y")
mri$mri.date1 <- format(mri$mri.date1, "%Y/%m/%d")
mri$mri.date2 <- strptime(as.character(mri$date2), "%d/%m/%Y")
mri$mri.date2 <- format(mri$mri.date2, "%Y/%m/%d")
mri <- mri[,c(1,4,5)]


################# MERGE DATASETS TO COMBINE DEMO, PHQ-9, AND MRI DATES

testdates <- merge(demos, phq9,  by = "PID", all.x = TRUE, all.y = TRUE)
testdates <- merge(testdates, mri,  by = "PID", all.x = TRUE, all.y = FALSE)
rm(demos, phq9, mri)

testdates <- testdates[,c(1,2,3,4,5)]
testdates$demos.date <- as.Date(testdates$demos.date)
testdates$phq9.date <- as.Date(testdates$phq9.date)
testdates$mri.date1 <- as.Date(testdates$mri.date1)
testdates$mri.date2 <- as.Date(testdates$mri.date2)


################# INPUT TESTDATES INTO FULL DATASET
#alldata <- merge(alldata, testdates, by = "PID", all.x = T, all.y = F)

################# CALCULATE DIFFERENCE OF TIME BETWEEN MRI AND PHQ-9 DATES 

testdates$diff.dates1 <- difftime(testdates$mri.date1, testdates$phq9.date, units = "days")
testdates$diff.dates1 <- as.numeric(testdates$diff.dates1)
testdates$diff.dates2 <- difftime(testdates$mri.date2, testdates$phq9.date, units = "days")
testdates$diff.dates2 <- as.numeric(testdates$diff.dates2)
names(testdates)[6] <- 'MRI1.Time.Diff(Days)'
names(testdates)[7] <- 'MRI2.Time.Diff(Days)'

rm(testdates)

#################### MRI DATA CLEANING
################# MRI DATA
## Guidelines from Frances:
## repeat measurements for some participants
## some missing data in pwm
## EXCLUDE IF:
## %SD > 20
## full width at half maximum (FWHM) > 0.08
## signal to noise ratio (SNR) < 8

## Import three data files for each brain region:
## basal ganglia (BG), midfrontal gray matter (MFGM), and peritrigonal white matter (PWM)

bg <- read.csv("Data/MRS-BG.csv")
mfgm <- read.csv("Data/MRS-MFGM.csv")
pwm <- read.csv("Data/MRS-PWM.csv")

## To add data for additional PIDs that was obtained later, I used:
#a.bg <- read.csv("Data/MRS-BG.csv")
#b.bg <- read.csv("Data/MRS BG.csv")
#b.bg <- subset(b.bg, !(PID %in% a.bg$PID))
# and then copied the new PID data into the main data files

## Rename columns in datasets and subset relevant variables
colnames(bg)
names(bg)[1] <- 'PID'
names(bg)[8] <- 'FWHM'
names(bg)[9] <- 'SNR'
names(bg)[10] <- 'Cho'
names(bg)[11] <- 'Cho.SD'
names(bg)[12] <- 'Cho.Cr'
names(bg)[14] <- 'Ins.SD'
names(bg)[15] <- 'Ins.Cr'
bg <- bg[c(1,8:15,2,7)]

colnames(mfgm)
names(mfgm)[1] <- 'PID'
names(mfgm)[8] <- 'FWHM'
names(mfgm)[9] <- 'SNR'
names(mfgm)[10] <- 'Cho'
names(mfgm)[11] <- 'Cho.SD'
names(mfgm)[12] <- 'Cho.Cr'
names(mfgm)[14] <- 'Ins.SD'
names(mfgm)[15] <- 'Ins.Cr'
mfgm <- mfgm[c(1,8:15,2,7)]

colnames(pwm)
names(pwm)[1] <- 'PID'
names(pwm)[8] <- 'FWHM'
names(pwm)[9] <- 'SNR'
names(pwm)[10] <- 'Cho'
names(pwm)[11] <- 'Cho.SD'
names(pwm)[12] <- 'Cho.Cr'
names(pwm)[14] <- 'Ins.SD'
names(pwm)[15] <- 'Ins.Cr'
pwm <- pwm[c(1,8:15,2,7)]


## INCLUDE IF:
## %SD < 20
## full width at half maximum (FWHM) < 0.08
## signal to noise ratio (SNR) > 8
## exclude missing cases from pwm dataset

bg <- bg[bg$FWHM <= 0.08 & bg$SNR >= 8 & bg$Cho.SD <= 20 & bg$Ins.SD <= 20,]
which(duplicated(bg$PID)=="TRUE")
# in bg, we can remove row 19 - this participant's measurement was repeated in row 20
bg <- bg[-which(duplicated(bg$PID)=="TRUE"),]

mfgm <- mfgm[mfgm$FWHM <= 0.08 & mfgm$SNR >= 8 & mfgm$Cho.SD <= 20 & mfgm$Ins.SD <= 20,]
duplicated(mfgm$PID)           ## no duplicates

pwm <- pwm[pwm$FWHM <= 0.08 & pwm$SNR >= 8 & pwm$Cho.SD <= 20 & pwm$Ins.SD <= 20,]
# in pwm, we can remove cases with missing values
pwm <- na.omit(pwm)
which(duplicated(pwm$PID)=="TRUE")
# in pwm, we can remove row 6 - this participant's measurement was repeated in row 7
pwm <- pwm[-which(duplicated(pwm$PID)=="TRUE"),]

## remove PIDs 109 and 135, which were excluded because the PHQ9 was administered in Afrikaans
bg <- bg[bg$PID!=135 & bg$PID!=109,]
mfgm <- mfgm[mfgm$PID!=135 & mfgm$PID!=109,]
pwm <- pwm[pwm$PID!=135 & pwm$PID!=109,]

#shapiro.test(bg$Cho)
#shapiro.test(mfgm$Cho)
#shapiro.test(pwm$Cho)
#shapiro.test(bg$Cho.Cr)
#shapiro.test(mfgm$Cho.Cr)
#shapiro.test(pwm$Cho.Cr)
#shapiro.test(bg$Ins)
#shapiro.test(mfgm$Ins)
#shapiro.test(pwm$Ins)         ## not normally distributed
#shapiro.test(bg$Ins.Cr)
#shapiro.test(mfgm$Ins.Cr)
#shapiro.test(pwm$Ins.Cr)


################# FOR INCLUDED PARTICIPANTS, CALCULATE MEDIAN DIFFERENCE IN TIME BETWEEN PHQ-9 AND MRI

bg.dates <- data.frame(bg$phq9.date, bg$scan_date)
mfgm.dates <- data.frame(mfgm$phq9.date, mfgm$scan_date)
pwm.dates <- data.frame(pwm$phq9.date, pwm$scan_date)

date_list <- list(bg.dates, mfgm.dates, pwm.dates)

## For all three dataframes,
# format dates in the way that R likes,
# calculate a new variable called "diff" which is the absolute time difference between phq-9 and mri scan,
# print median (IQR) time difference for participants in each brain region,
# print p value from Shapiro-Wilk test of normality

date_list <- lapply(date_list, function(data) {
  names(data)[1] <- "phq9.date"
  names(data)[2] <- "scan_date"
  data$scan_date <- format(strptime(as.character(data$scan_date), "%d/%m/%Y"), "%Y/%m/%d")
  data$phq9.date <- format(strptime(as.character(data$phq9.date), "%d/%m/%Y"), "%Y/%m/%d")
  data$diff <- abs(as.numeric(difftime(data$scan_date, data$phq9.date, units = "days")))
  cat("Median:", median(data$diff), "\n")
  cat("Quantile 25%:", quantile(data$diff, 0.25), "\n")
  cat("Quantile 75%:", quantile(data$diff, 0.75), "\n\n")
  cat("Shapiro-Wilk Test:", shapiro.test(data$diff)$p.value, "\n\n\n")
  data
}
)

## Extract the dates dataframes, now including time difference
bg.dates <- date_list[[1]]
mfgm.dates <- date_list[[2]]
pwm.dates <- date_list[[3]]
rm(date_list)


## insert time difference into main bg/mfgm/pwm data frames
bg <- cbind(bg, bg.dates[3])
mfgm <- cbind(mfgm, mfgm.dates[3])
pwm <- cbind(pwm, pwm.dates[3])

median(c(bg$diff, pwm$diff, mfgm$diff))
quantile(c(bg$diff, pwm$diff, mfgm$diff), 0.25)
quantile(c(bg$diff, pwm$diff, mfgm$diff), 0.75)

rm(bg.dates, mfgm.dates, pwm.dates)


## Rename columns in each dataframe to reflect brain regions
bg <- bg %>%
  rename(
    BG.Cho = Cho,
    BG.Cho.Cr = Cho.Cr,
    BG.Ins = Ins,
    BG.Ins.Cr = Ins.Cr,
    BG.diff = diff
  )
mfgm <- mfgm %>%
  rename(
    MFGM.Cho = Cho,
    MFGM.Cho.Cr = Cho.Cr,
    MFGM.Ins = Ins,
    MFGM.Ins.Cr = Ins.Cr,
    MFGM.diff = diff
  )
pwm <- pwm %>%
  rename(
    PWM.Cho = Cho,
    PWM.Cho.Cr = Cho.Cr,
    PWM.Ins = Ins,
    PWM.Ins.Cr = Ins.Cr,
    PWM.diff = diff
  )

## Add demographic data for each MRI brain region
#bg <- merge(bg, demodata, all.x = T, all.y = F)
#mfgm <- merge(mfgm, demodata, all.x = T, all.y = F)
#pwm <- merge(pwm, demodata, all.x = T, all.y = F)

df.list <- list("Basal Ganglia" = bg, "MidFrontal Gray Matter" = mfgm, "Peritrigonal White Matter" = pwm)
#write.xlsx(df.list, "Cleaned Data/Data - MRS.xlsx")
rm(df.list)


## Add key columns to overall dataset for each brain region
bg <- bg %>%
  dplyr::select(
    PID,
    BG.Cho,
    BG.Cho.Cr,
    BG.Ins,
    BG.Ins.Cr,
    BG.diff
  )
mfgm <- mfgm %>%
  dplyr::select(
    PID,
    MFGM.Cho,
    MFGM.Cho.Cr,
    MFGM.Ins,
    MFGM.Ins.Cr,
    MFGM.diff
  )
pwm <- pwm %>%
  dplyr::select(
    PID,
    PWM.Cho,
    PWM.Cho.Cr,
    PWM.Ins,
    PWM.Ins.Cr,
    PWM.diff
  )

alldata <- merge(alldata, bg, all.x = T, all.y = F)
alldata <- merge(alldata, mfgm, all.x = T, all.y = F)
alldata <- merge(alldata, pwm, all.x = T, all.y = F)

alldata$diff <- coalesce(alldata$BG.diff, alldata$MFGM.diff, alldata$PWM.diff)

colnames(alldata)
alldata <- alldata %>%
  dplyr::select(-c("BG.diff", "MFGM.diff", "PWM.diff"))

######################### Create single data frame with MRS data by brain region, HIV.status, and PHQ-9 total score

#bg$Region <- rep(c("Basal Ganglia"))
#mfgm$Region <- rep(c("MidFrontal Gray Matter"))
#pwm$Region <- rep(c("Peritrigonal White Matter"))

#merged.mri <- do.call("rbind", list(bg, mfgm, pwm))
#colnames(merged.mri)
#mridata <- merge(merged.mri, subset(testdata, select = -c(language, HIV.status)), all.x = T, all.y = F, by = "PID")
#mridata <- mridata %>%
#  relocate(Region, .after = PID)
#colnames(mridata)

#mridata %>% count(HIV.status, Region)

rm(bg, mfgm, pwm)


################# BLOOD BIOMARKER DATASET


############## NFL ELISA DATA PROCESSING
# Import files from spectrophotometer
elisa_SN <- read.csv("Data/NFL ELISA.csv")
elisa_SN <- elisa_SN[,c(-1,-12,-13)]

###### STEP 1: subtract average blank absorbance from all cells
# Blank absorbance
blank <- mean(elisa_SN[8,1], elisa_SN[8,2])
elisa_SN <- elisa_SN - blank

# formula for coefficient of variation %cv = sd/mean
cv <- function (x){sd(x)*100/mean(x)}

##### STEP 2: create standard curve and fit to 4-PL curve using drc package

# vector for standard curve concentrations
stdconc <- c(1000, 500, 250, 125, 62.5, 31.25, 15.63)

# extract standards and check %CV for any outliers
stds <- elisa_SN[1:7,1:2]
stds$cv <- apply(stds, 1, cv)
rm(stds)

# create dataframe with unlisted absorbances and standard concentrations
stdcurve <- data.frame(stdconc,
                       stds.abs = unlist(c(elisa_SN[1:7,1:2])))

# plot standard curve
#ggplot(stdcurve, aes(x = stdconc, y = stds.abs))+
#  geom_point() +
#  geom_smooth() +
#  labs(x = "Concentration (pg/mL)", y = "Corrected Absorbance") +
#  theme_minimal()

# fit 4-PL curve and derive estimates
library(drc)
drc.fit <- drm(stds.abs ~ stdconc,
               data = stdcurve,
               fct = LL2.4(names = c("Slope", "Lower", "Upper", "ED50"))
)
summary(drc.fit)
#plot(drc.fit)

##### STEP 3: extract sample absorbances and predict concentration using 4-PL curve

sample.abs <- data.frame(
  Blood_ID = 1:64,
  absorbance = unlist(c(elisa_SN[1:8,3:10])),
  row.names = NULL
)
sample.abs <- sample.abs[-64,]

conc <- ED(drc.fit,sample.abs$absorbance,type="absolute",display=F)

sample.abs$Estimate <- conc[,1]
sample.abs$SE <- conc[,2]

sample.abs <- sample.abs %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))

rm(stdcurve, drc.fit, stdconc, blank, cv, elisa_SN, conc)

## Match Blood_IDs and PIDs
matching <- read.csv("Data/PID Matching.csv")
NFL <- merge(sample.abs, matching, by = "Blood_ID")
# remove duplicate case (Blood_ID 62)
NFL <- subset(NFL, Blood_ID != "62")
# subset dataframe to yield actual PIDs and estimates + SEs
NFL <- NFL[,c(5,3)]
names(NFL)[2] <- "NFL"
rm(sample.abs)

################# LUMINEX DATA CLEANING
## Luminex data has been processed by the instrument software and cleaned in Excel
blooddata <- read.csv("Data/Luminex Data Final.csv")
names(blooddata)[1] <- "Blood_ID"
blooddata$Blood_ID <- gsub("X", "", blooddata$Blood_ID)
blooddata <- merge(blooddata, matching, all.x = T, all.y = F)
## Match Blood_ID to PID
blooddata <- blooddata %>%
  relocate(PID, .after = Blood_ID) %>%
  dplyr::select(-Blood_ID)
## drop PIDs 109 and 135 (excluded as PHQ-9 was in Afrikaans)
blooddata <- subset(blooddata, PID != "109" & PID != "135")

## Merge ELISA and Luminex Data
blooddata <- merge(blooddata, NFL, all.x = T, all.y = F)
new_order = sort(colnames(blooddata[-1]))
blooddata <- blooddata[, c("PID", new_order)]
rm(NFL, matching, new_order)

## Add all blood biomarker data to full dataset
alldata <- merge(alldata, blooddata, all.x = T)
rm(blooddata)


################# EXPORT ALL DATA FILES
write.csv(alldata, "Cleaned Data/Full_Dataset.csv", row.names = FALSE)
#write.csv(demodata, "Cleaned Data/Data - Demos.csv", row.names = FALSE)
#write.csv(testdata, "Cleaned Data/Data - PHQ9.csv", row.names = FALSE)
#write.csv(testdates, "Cleaned Data/Test Dates.csv", row.names = F)
#write.csv(mridata, "Cleaned Data/Data - MRI.csv", row.names = F)
#write.csv(blooddata, "Cleaned Data/Data - Blood Biomarkers.csv", row.names = F)

colnames(alldata)

