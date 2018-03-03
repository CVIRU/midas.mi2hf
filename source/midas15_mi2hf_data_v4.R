# |----------------------------------------------------------------------------------|
# | Project: Heart Failures after MIs in MIDAS                                       |
# | Script: Make the data set for the analysis                                       |
# | Authors: Jen Wellings; Davit Sargsyan                                            |   
# | Created: 04/21/2017                                                              |
# | Modified: 07/08/2017, using new MIDAS15 with Patient Type variable               |
# |           07/29/2017, keep inpatient records only;redefine lipoid disorder, etc. |
# |           02/28/2018, add PCI and CABG (revascularization)                       |
# |           03/02/2018, add AMI type - subcardial vs. other                        |
# |----------------------------------------------------------------------------------|
# Header----
# Save consol output to a log file
# sink(file = "tmp/log_midas15_mi2hf_data_v4.txt")
date()

# Load packages
require(data.table)
require(knitr)

# PART I----
# Move up one directory
wd <- getwd()
setwd("..")
DATA_HOME <- paste(getwd(),
                   "data/midas.mi2hf",
                   sep = "/")
# Reset working directory
setwd(wd)
getwd()

# Load MIDAS----
# NOTE: data was preprocessed. See project 
# 'midas' R script 'export_midas_from_csv_to_rdata.R'
system.time(load("E:/MIDAS/midas15.RData"))
midas15

# Remove unused variables----
midas15[, ZIP := NULL]
midas15[, TOTBIL := NULL]
midas15[, DIV := NULL]
gc()

# Number of patients
length(unique(midas15$Patient_ID))
# 4,680,205

# Keep only records from 01/01/1995 (admission dates)
midas15 <- subset(midas15, ADMDAT >= "1995-01-01")

# Remove anybody who was younger than 18 at any admisson
id.rm <- midas15$Patient_ID[midas15$AGE < 18]
midas15 <- subset(midas15, !(Patient_ID %in% id.rm))
hist(midas15$AGE, 100)
gc()

# NEW (07/29/2017): keep inpatient records only
midas15 <- droplevels(subset(midas15,
                             PAT_TYPE %in% c(-1, 0)))

summary(midas15)
midas15[, PAT_TYPE := NULL]
gc()

# Separate diagnostic codes (DX1:DX9)----
dx <- midas15[, DX1:DX9]
cnames <- names(dx)

dx.1.3 <- data.table(apply(dx,
                           2,
                           substr,
                           start = 1,
                           stop = 3))

# Exclusions: Cancers----
# See "C:\Users\ds752\Documents\svn_local\trunk\Cardiovascular\Jen Wellings HF After MI\ICD-9-CM Cancer Codes Exclude.txt" for details)
excl.rec <- rowSums(apply(X = dx.1.3,
                          MARGIN = 2, 
                          FUN = function(a) {
                            a %in% as.character(c(140:165,
                                                  170:176, 
                                                  180:239))
                          })) > 0
table(excl.rec)
id.rm <- unique(midas15$Patient_ID[excl.rec])
rec.keep <- which(!(midas15$Patient_ID %in% id.rm))

midas15 <- midas15[rec.keep, ]

# Save
save(midas15, 
     file = file.path(DATA_HOME,
                      "midas15_mi2hf_02282018.RData"), 
     compress = FALSE)

# Clean workspace
rm(list = ls())
gc()
# beepr::beep(3)

# PART II----
# Move up one directory
wd <- getwd()
setwd("..")
DATA_HOME <- paste(getwd(),
                   "data/midas.mi2hf",
                   sep = "/")
# Reset working directory
setwd(wd)

# Reload the data----
load(file.path(DATA_HOME, 
               "midas15_mi2hf_02282018.RData"))
midas15

# Separate diagnoses and procedure codes----
dx <- midas15[, DX1:DX9]
cnames <- names(dx)

dx.1.3 <- data.table(apply(dx,
                           2,
                           substr,
                           start = 1,
                           stop = 3))

# Vascular Disease: Miocardial Infarction----
# http://www.icd9data.com/2015/Volume1/390-459/410-414/410/default.htm
midas15[, ami := (rowSums(dx.1.3 == "410") > 0)]

# AMI as reason for admission (DX1)
midas15[, ami.dx1 := (dx.1.3$DX1 == "410")]

kable(addmargins(table(ami = midas15$ami,
                       ami.dx1 = midas15$ami.dx1)))
# Rows: DX1-9
# Columns: DX1 only
# |      |   FALSE|   TRUE|     Sum|
# |:-----|-------:|------:|-------:|
# |FALSE | 7827685|      0| 7827685|
# |TRUE  |  124448| 335600|  460048|
# |Sum   | 7952133| 335600| 8287733|

# Keep patients who was admitted for AMI from 01/01/2000 onward
id.keep <- unique(midas15$Patient_ID[midas15$ami.dx1 &
                                       midas15$ADMDAT >= "2000-01-01"])
dt1 <- subset(midas15, 
              Patient_ID %in% id.keep)
setkey(dt1)
length(unique(dt1$Patient_ID))
# 1,082,998 records of 183,780 patients

# Same for DXs
# Separate diagnoses and procedure codes----
dx <- dt1[, DX1:DX9]
dx.1.3 <- data.table(apply(dx,
                           2,
                           substr,
                           start = 1,
                           stop = 3))

proc <- dt1[, PROC1:PROC8]
for (i in 1:ncol(proc)){
  print(typeof(proc[[i]]))
  proc[[i]] <- as.character(proc[[i]])
  proc[[i]][is.na(proc[[i]])] <- ""
}

rm(midas15)
gc()

# Risk factors----
# See "C:\Users\ds752\Documents\svn_local\trunk\Cardiovascular\MCHADS\source\final\MIDAS ICD-9 Code Counts.pptx" for details
# Copy is saved to 'docs' folder in this project
# 0a. Subendocardial AMI anywehre
dt1[, sub.ami := rowSums(apply(dx,
                               MARGIN = 2,
                               FUN = function(a) {
                                 a %in% as.character(c(41070:41072))
                               })) > 0]

# 0b. Subendocardial AMI as reason for admission (DX1)
dt1[, sub.ami.dx1 := (DX1 %in% as.character(c(41070:41072)))]

kable(addmargins(table(sub.ami = dt1$sub.ami,
                       sub.ami.dx1 = dt1$sub.ami.dx1)))
# Rows: DX1-9
# Columns: DX1 only
# |      |  FALSE|   TRUE|     Sum|
# |:-----|------:|------:|-------:|
# |FALSE | 891141|      0|  891141|
# |TRUE  |  25605| 166527|  192132|
# |Sum   | 916746| 166527| 1083273|

# 1a. Acute CFH----
# http://www.icd9data.com/2015/Volume1/390-459/420-429/428/default.htm
dt1[, chf.acute := rowSums(apply(dx,
                                 MARGIN = 2,
                                 FUN = function(a) {
                                   a %in% as.character(c(4280,
                                                         42820:42821,
                                                         42823,
                                                         42830:42831,
                                                         42833,
                                                         42840:42841,
                                                         42843))
                                 }),
                           na.rm = TRUE) > 0]

# 1b. Acute CFH in DX1----
dt1[, chf.acute.dx1 := (DX1 %in%as.character(c(4280,
                                               42820:42821,
                                               42823,
                                               42830:42831,
                                               42833,
                                               42840:42841,
                                               42843)))]

kable(addmargins(table(chf.acute = dt1$chf.acute,
                       chf.acute.dx1 = dt1$chf.acute.dx1)))
# Rows: DX1-9
# Columns: DX1 only
# |      |  FALSE|  TRUE|     Sum|
# |:-----|------:|-----:|-------:|
# |FALSE | 767444|     0|  767444|
# |TRUE  | 232330| 83499|  315829|
# |Sum   | 999774| 83499| 1083273|

gc()

# 1c. Chronic CFH----
dt1[, chf.chron := rowSums(apply(dx,
                                 MARGIN = 2,
                                 FUN = function(a) {
                                   a %in% as.character(c(42822,
                                                         42832,
                                                         42842))
                                 }),
                           na.rm = TRUE) > 0]
table(dt1$chf.chron)
# FALSE    TRUE
# 1059782   23491
gc()

# 2. Hypertension----
dt1[, hyp := rowSums(apply(dx,
                           MARGIN = 2,
                           FUN = function(a) {
                             a %in% as.character(c(4010:4011,
                                                   4019,
                                                   40200:40201,
                                                   40290:40291,
                                                   40300:40301,
                                                   40310:40311,
                                                   40390:40391,
                                                   40400:40403,
                                                   40410:40413,
                                                   40490:40493,
                                                   40501,
                                                   40509,
                                                   40511,
                                                   40519,
                                                   40591,
                                                   40599))
                           }),
                     na.rm = TRUE) > 0]
table(dt1$hyp)
# FALSE   TRUE 
# 368572 714701 
gc()

# 3. Diabetes mellitus----
dt1[, diab := (rowSums(dx.1.3 == "250",
                       na.rm = TRUE) > 0)]
table(dt1$diab)
# FALSE   TRUE 
# 694793 388480
gc()

# 4. Chronic liver disease and cirrhosis----
dt1[, cld := (rowSums(dx.1.3 == "571", 
                      na.rm = TRUE) > 0)]
table(dt1$cld)
# FALSE    TRUE 
# 1075389    7884
gc()

# 5. Chronic kidney disease----
dt1[, ckd := (rowSums(dx.1.3 == "585",
                      na.rm = TRUE) > 0)]
table(dt1$ckd)
# FALSE   TRUE 
# 953045 130228 
gc()

# 6. Chronic obstructive pulmonary disease (COPD)----
dt1[, copd := rowSums(apply(dx.1.3,
                            MARGIN = 2,
                            FUN = function(a) {
                              a %in% as.character(490:496)
                            }),
                      na.rm = TRUE) > 0]
table(dt1$copd)
# FALSE   TRUE 
# 873354 209919 
gc()

# 7. Disorders of lipoid metabolism----
dt1[, lipid := rowSums(apply(dx,
                             MARGIN = 2,
                             FUN = function(a) {
                               a %in% as.character(c(2720:2724,
                                                     2728:2729))
                             }),
                       na.rm = TRUE) > 0]
table(dt1$lipid)
# FALSE   TRUE 
# 739296 343977 
gc()

# 8. PCI----
# ATTN: can be also 06.6: Excision Of Lingual Thyroid!
dt1[, pci := rowSums(apply(proc,
                           MARGIN = 2,
                           FUN = function(a) {
                             a %in% as.character(c(66,
                                                   3601:3602,
                                                   3905))
                           }),
                     na.rm = TRUE) > 0]
table(dt1$pci)
# FALSE   TRUE 
# 960044 123229 
gc()

# 9. CABG----
dt1[, cabg := rowSums(apply(proc,
                            MARGIN = 2,
                            FUN = function(a) {
                              a %in% as.character(c(3610:3616,
                                                    362,
                                                    3961))
                            }),
                      na.rm = TRUE) > 0]
table(dt1$cabg)
# FALSE    TRUE 
# 1047725   35548 
gc()

# Clean memory
dt1
summary(dt1)
rm(dx,
   dx.1.3,
   proc)
gc()

# First MI discharge (DX1) between 01/01/2000 and 12/31/2015----
setkey(dt1, DSCHDAT, Patient_ID)

dt1[, first := min(DSCHDAT[ami.dx1 &
                             DSCHDAT >= "2000-01-01"],
                   na.rm = TRUE), 
    by = Patient_ID]

# Admissions prior to first MI (5 years look-back)
dt1[, prior := ((DSCHDAT < first) & 
                  (difftime(DSCHDAT, 
                            first,
                            units = "days") < 5*365.25))]

# First AF admissions
dt1[, current := (DSCHDAT == first)]

# Summary
sum(dt1$prior)
sum(dt1$current)
dt1 <- droplevels(dt1)
summary(dt1)
save(dt1, 
     file = file.path(DATA_HOME, 
                      "dt1_02282018.RData"),
     compress = FALSE)

# PART III----
# load(file.path(DATA_HOME, "dt1_02282018.RData"))
# Outcomes and histories (prior to 1st PCI)----
summary(dt1)
system.time(
  hh <- dt1[, list(ADMDAT,
                   DSCHDAT,
                   patbdte,
                   NEWDTD,
                   CAUSE,
                   HOSP,
                   SEX,
                   PRIME,
                   RACE,
                   HISPAN,
                   AGE,
                   dschyear = as.numeric(substr(DSCHDAT, 1, 4)),
                   first,
                   prior,
                   current,
                   ami.dx1,
                   ami,
                   sub.ami.dx1,
                   sub.ami,
                   readm = sum(!(prior | current) &
                                 (difftime(ADMDAT,
                                           first,
                                           units = "days") > 0) &
                                 (is.na(NEWDTD) | (difftime(NEWDTD,
                                                            DSCHDAT,
                                                            units = "days")) > 0)) > 0,
                   readm.dat = min(ADMDAT[!(prior | current) &
                                            (difftime(ADMDAT,
                                                      first,
                                                      units = "days") > 0) &
                                            (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                       DSCHDAT,
                                                                       units = "days")) > 0)],
                                   na.rm = TRUE),
                   days2readm = -1,
                   post.chf.acute.dx1 = sum(chf.acute.dx1 & 
                                              !(prior | current) &
                                              (difftime(ADMDAT,
                                                        first,
                                                        units = "days") >= 0) &
                                              (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                         DSCHDAT,
                                                                         units = "days")) > 0)) > 0,
                   post.chf.acute.dx1.dat = min(ADMDAT[chf.acute.dx1 & 
                                                         !(prior | current) &
                                                         (difftime(ADMDAT,
                                                                   first,
                                                                   units = "days") >= 0) &
                                                         (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                                    DSCHDAT,
                                                                                    units = "days")) > 0)],
                                                na.rm = TRUE),
                   days2post.chf.acute.dx1 = -1,
                   post.pci = sum(pci & 
                                    !(prior | current) &
                                    (difftime(ADMDAT,
                                              first,
                                              units = "days") >= 0) &
                                    (is.na(NEWDTD) | (difftime(NEWDTD,
                                                               DSCHDAT,
                                                               units = "days")) > 0)) > 0,
                   post.pci.dat = min(ADMDAT[pci & 
                                               !(prior | current) &
                                               (difftime(ADMDAT,
                                                         first,
                                                         units = "days") >= 0) &
                                               (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                          DSCHDAT,
                                                                          units = "days")) > 0)],
                                      na.rm = TRUE),
                   days2post.pci = -1,
                   post.cabg = sum(cabg & 
                                     !(prior | current) &
                                     (difftime(ADMDAT,
                                               first,
                                               units = "days") >= 0) &
                                     (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                DSCHDAT,
                                                                units = "days")) > 0)) > 0,
                   post.cabg.dat = min(ADMDAT[cabg & 
                                                !(prior | current) &
                                                (difftime(ADMDAT,
                                                          first,
                                                          units = "days") >= 0) &
                                                (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                           DSCHDAT,
                                                                           units = "days")) > 0)],
                                       na.rm = TRUE),
                   days2post.cabg = -1,
                   dead = sum(!(prior | current) &
                                (difftime(NEWDTD,
                                          first,
                                          units = "days") > 0),
                              na.rm = TRUE) > 0,
                   days2death = -1,
                   cvdeath = FALSE,
                   days2cvdeath = -1,
                   hami = (sum(ami & prior) > 0),
                   hchf.acute = (sum(chf.acute & prior) > 0),
                   chf.acute.current = (sum(chf.acute & current) > 0),
                   hchf.chron = (sum(chf.chron & (prior | current)) > 0),
                   hhyp = (sum(hyp & (prior | current)) > 0), 
                   hdiab = (sum(diab & (prior | current)) > 0), 
                   hcld = (sum(cld & (prior | current)) > 0),
                   hckd = (sum(ckd & (prior | current)) > 0),
                   hcopd = (sum(copd & (prior | current)) > 0),
                   hlipid = (sum(lipid & (prior | current)) > 0),
                   hpci = (sum(pci & prior) > 0),
                   hcabg = (sum(cabg & prior) > 0)), 
            by = Patient_ID]
)
summary(hh)
max(hh$post.chf.acute.dx1.dat[is.finite(hh$post.chf.acute.dx1.dat)])
gc()

# Separate first MI admission
# Remove all cases with no MI records
case <- unique(subset(hh, current & ami.dx1))

# If the are are more than 1 records of 1st MI admissions per person,
nrow(case) - length(unique(case$Patient_ID))
# Remove 366 patient with duplicate records
case <- case[!(Patient_ID %in% Patient_ID[duplicated(Patient_ID)]), ]
summary(case)

# Remove patients that died at 1st MI discharge
case <- droplevels(subset(case, (is.na(NEWDTD) | NEWDTD != first)))

# Remove anyone with history of MI
case <- droplevels(subset(case, !hami))

summary(case)
case[, ami.dx1 := NULL] # All patients have it
case[, ami := NULL] # All patients have it
case[, hami := NULL] # No patient has it
case[, prior := NULL] # False for all patients
case[, current := NULL] # True for all patients
gc()

# Days to events----
# a. Days to readmission for any reason----
case$days2readm <- as.numeric(as.character(difftime(case$readm.dat,
                                                    case$first,
                                                    units = "days")))
case$days2readm[is.infinite(case$days2readm)] <- NA
summary(case$days2readm)
hist(case$days2readm, 100)

# b. Days to readmission for acute CHF----
case$days2post.chf.acute.dx1 <- as.numeric(as.character(difftime(case$post.chf.acute.dx1.dat,
                                                                 case$first,
                                                                 units = "days")))
case$days2post.chf.acute.dx1[is.infinite(case$days2post.chf.acute.dx1)] <- NA
summary(case$days2post.chf.acute.dx1)
hist(case$days2post.chf.acute.dx1, 100)

# c. Days to all-cause death----
case$days2death <- as.numeric(as.character(difftime(case$NEWDTD,
                                                    case$first,
                                                    units = "days")))
case$days2death[is.infinite(case$days2death)] <- NA
summary(case$days2death)
hist(case$days2death, 100)

# c. Days to cardiovascular death----
# Source: http://www.health.state.ok.us/stats/Vital_Statistics/Death/039_causes.shtml
# |-------------------------------------------------------------------|
# | Major cardiovascular diseases        | I00-I78                    |
# | Diseases of heart	                   | I00-I09, I11, I13, I20-I51 |
# | Hypertensive heart disease with      |                            | 
# |    or without renal disease          | I11,I13                    |
# | Ischemic heart diseases	             | I20-I25                    |
# | Other diseases of heart	             | I00-I09,I26-I51            |
# | Essential (primary) hypertension     |                            |
# |    and hypertensive renal disease	   | I10,I12                    |
# | Cerebrovascular diseases	           | I60-I69                    |
# | Atherosclerosis	                     | I70                        |
# | Other diseases of circulatory system | I71-I78                    |
# |-------------------------------------------------------------------|

case$cvdeath[case$dead & substr(case$CAUSE, 1, 1) == "I"] <- TRUE

kable(addmargins(table(all_cause_death = case$dead,
                       cv_death = case$cvdeath)))
# Row: all-cause death
# Column: cv death
# |      |  FALSE|  TRUE|    Sum|
# |:-----|------:|-----:|------:|
# |FALSE | 116536|     0| 116536|
# |TRUE  |  17318| 23852|  41170|
# |Sum   | 133854| 23852| 157706|

case$days2cvdeath <- case$days2death
case$days2cvdeath[!case$cvdeath] <- NA
summary(case$days2cvdeath)
hist(case$days2cvdeath, 100)

# d. Days to PCI anfter AMI----
case$days2post.pci <- as.numeric(as.character(difftime(case$post.pci.dat,
                                                       case$first,
                                                       units = "days")))
case$days2post.pci[is.infinite(case$days2post.pci)] <- NA
summary(case$days2post.pci)
hist(case$days2post.pci, 100)

# d. Days to PCI anfter AMI----
case$days2post.cabg <- as.numeric(as.character(difftime(case$post.cabg.dat,
                                                        case$first,
                                                        units = "days")))
case$days2post.cabg[is.infinite(case$days2post.cabg)] <- NA
summary(case$days2post.cabg)
hist(case$days2post.cabg, 100)

# Summary of the subset----
summary(case)
case

# Save----
save(case, 
     file = file.path(DATA_HOME, "case_02282018.RData"),
     compress = FALSE)

# Clean memory----
gc()
# sink()