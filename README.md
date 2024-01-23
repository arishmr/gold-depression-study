This code repository contains analysis code for the GOLD Depression Study, carried out at the Family Centre for Research with Ubuntu (FAMCRU), Tygerberg Hospital, Cape Town, South Africa.

The associated dataset for this study is available publicly at https://doi.org/10.7488/ds/7507

For any questions about the code or dataset, please contact the lead investigator: Arish Mudra Rakshasa-Loots (arish.mudrarakshasa@ed.ac.uk).


## SCRIPTS

Data analysis is carried out using the following scripts:

## Section 1: Preamble
Always run these when opening the project
1-packages					Load (and install, if not already installed) the required packages for analysis
2-load-data					Load the dataset (which can be downloaded from the DOI provided above) and clean up blood biomarker data

## Section 2: Descriptive Statistics
3-demos-by-hiv				Summarise participant characteristics overall and by HIV status or language of PHQ-9 administration
4-phq9-descr				Summarise total PHQ-9 scores for participants overall and by HIV status or language of administration
5-phq9-plot					Plot PHQ-9 total or item-level scores by HIV status or language of administration, and plot correlations between PHQ-9 item-level scores

## Section 3: MRS Biomarkers
6-mri-data					Set up MRS data analysis and calculate time difference between PHQ-9 and MRS
7-mri-demos					Summarise participant characteristics for all participants with MRS data available
8-mri-plots-single			Produce a single plot of both MRS biomarkers across two referencing protocols and three brain regions by HIV status and by PHQ-9 score
9-mri-sem					Run SEM to calculate standardised effect estimates for each path in the mediation analysis for MRS biomarkers
10-mri-sem-adj				Run SEM to calculate standardised effect estimates for each path in the mediation analysis adjusted for age, gender, and time between PHQ-9 and MRS
11-mri-sem-plots				Using results from [9] and [10], create forest plots for path estimates with CIs
12-mri-multiple-mediation		Run SEM with all MRS biomarkers at once (separately for both referencing protocols)
13-mri-sem-MLM				Run SEM with MLM estimator for MRS biomarkers

## Section 4: Blood Biomarkers
14-biomarker-plots			Produce a single plot of all blood biomarkers by HIV status and by PHQ-9 score
15-biomarker-sem				Run SEM to calculate standardised effect estimates for each path in the mediation analysis for blood biomarkers
16-biomarker-sem-adj			Run SEM to calculate standardised effect estimates for each path in the mediation analysis adjusted for age and gender
17-biomarker-sem-plots			Using results from [15] and [16], create forest plots for path estimates with CIs
18-blood-multiple-mediation		Run SEM with all blood biomarkers at once
19-biomarker-sem-MLM			Run SEM with MLM estimator for blood biomarkers

## Section 5: Exploratory Analyses
20-corrplots				Produce correlation matrices for blood biomarkers, MRS biomarkers, and all biomarkers
21-gender-interactions			Calculate interactions of gender with mediation model
22-virally-suppressed-only		Run SEM while excluding n = 4 participants with HIV viral load >200 for MRS and blood biomarkers


## NOTES

- My code is set up so that the 1-packages and 2-load-data scripts need to be run once each time when the project is opened, but not necessarily again.
- I export figures and results files to a /Figures and /Results sub-folder within my working directory, respectively. 
- While the scripts are relatively independent of each other, I generally run scripts in a particular section (descriptive stats, MRI analysis, blood biomarker analysis) in the order in which they are named. The 6-mri-data script needs to be run before running any of the other MRI analysis scripts. The 11-mri-sem-plots and 17-biomarker-sem-plots rely on results files exported by the preceding SEM scripts.

## VARIABLES

PID - participant ID, uniform across all data files
HIV.status - participant's HIV status (participant with HIV or without HIV)
age - in years
grade - in school, numeric or School of Skills
gender - Boy or Girl or Transgender/Genderqueer or other
other.gender - if participants wrote in their own gender (other)
ethnicity - Black/African, White, Coloured or other
other.ethnicity - if participants wrote in their own ethnicity (other)
sexual.orient - Straight/Heterosexual or Gay/Lesbian/Homosexual or Bisexual/Pansexual or other
other.sexual.orient - if participants wrote in their own sexual orientation (other)
alcohol - did you consume any alcohol within the past 6 months
alc.quant - if yes to alcohol, how much alcohol did you drink in the past week
smoking - did you smoke cigarettes within the past 6 months
cig.quant - if yes to smoking, how many cigarettes do you smoke in a week
recdrugs - did you use any recreational drugs in the past 6 months
language - language of administration of PHQ-9 and demographic questionnaire - English or isiXhosa

q1:q9 - each item of the PHQ-9 in order
bonus.q - additional (optional) question of the PHQ-9
total - total score on the 9 items of the PHQ-9

Included - did participants have a complete PHQ-9 in order to be included in the study

vl.instance - which instance of log viral load results available for each participant were used (latest available)
vl.date - date of latest log viral load results
viral.load.log - log value of HIV viral load
vl.copies.instance - which instance of viral load results available for each participant were used (latest available)
vl.copies.date - date of latest viral load results
viral.load.copies - absolute value of HIV viral load - 20 is the detection limit, so 19 represents undetectable viral load
cd4.instance - which instance of CD4 cell count available for each participant were used (latest available)
cd4.date - date of latest CD4 cell count
CD4 - value of CD4 cell count
cd4.abs.instance - which instance of absolute CD4 cell count available for each participant were used (latest available)
cd4.abs.date - date of latest absolute CD4 cell count
CD4.abs - value of absolute CD4 cell count
arv.instance - which instance of antiretroviral therapy regimen available for each participant were used (latest available)
regimen - antiretroviral therapy regimen
dose.p.day - dose per day of antiretroviral therapy regimen

BG.Cho - concentration of choline in basal ganglia referenced to water
BG.Cho.Cr - concentration of choline in basal ganglia referenced to creatine
BG.Ins - concentration of myo-inositol in basal ganglia referenced to water
BG.Ins.Cr - concentration of myo-inositol in basal ganglia referenced to creatine

MFGM.Cho - concentration of choline in midfrontal gray matter referenced to water
MFGM.Cho.Cr - concentration of choline in midfrontal gray matter referenced to creatine
MFGM.Ins - concentration of myo-inositol in midfrontal gray matter referenced to water
MFGM.Ins.Cr - concentration of myo-inositol in midfrontal gray matter referenced to creatine

PWM.Cho - concentration of choline in peritrigonal white matter referenced to water
PWM.Cho.Cr - concentration of choline in peritrigonal white matter referenced to creatine
PWM.Ins - concentration of myo-inositol in peritrigonal white matter referenced to water
PWM.Ins.Cr - concentration of myo-inositol in peritrigonal white matter referenced to creatine

diff - absolute difference (in days) between PHQ-9 administration and MRI scan date

BDNF:YKL40 - absolute concentrations of blood biomarkers measured in serum using Luminex immunoassay
OOR < represents undetectable concentration
