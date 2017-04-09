# Project: Heart Failures after MIs in MIDAS       
# Author: Jen Wellings; Davit Sargsyan   
# Created:  04/22/2016
# Modified: 04/08/2017
########################################
DATA_HOME <- "C:/Users/ds752/Documents/git_local/data/midas.mi2hf"

require(data.table)
# Load MIDAS
setwd("C:/MIDAS")
system.time(load("midas15.RData"))
midas15

# Remove unused variables
midas15[, LOCATION := NULL]
midas15[, CAUSE := NULL]
midas15[, DeathRNUM := NULL]
midas15[, ZIP := NULL]
midas15[, STATUS := NULL]
midas15[, SOURCE := NULL]
midas15[, TOTBIL := NULL]
midas15[, DRG := NULL]
midas15[, RECDID := NULL]
midas15[, DSHYR := NULL]
midas15[, PRDTE1 := NULL]
midas15[, PRDTE2 := NULL]
midas15[, PRDTE3 := NULL]
midas15[, PRDTE4 := NULL]
midas15[, PRDTE5 := NULL]
midas15[, PRDTE6 := NULL]
midas15[, PRDTE7 := NULL]
midas15[, PRDTE8 := NULL]
midas15[, SECOND := NULL]
midas15[, THIRD := NULL]
midas15[, DIV := NULL]
gc()

# Number of patients
length(unique(midas15$Patient_ID))
# 4,842,160

# Convert dates
midas15[, NEWDTD := as.Date(NEWDTD, format = "%m/%d/%Y")]
midas15[, ADMDAT := as.Date(ADMDAT, format = "%m/%d/%Y")]
midas15[, DSCHDAT := as.Date(DSCHDAT, format = "%m/%d/%Y")]
midas15[, patbdte := as.Date(patbdte, format = "%m/%d/%Y")]

# Missing birthdays
midas15[is.na(midas15$patbdte)]
# All records for these patients
midas15[Patient_ID %in% Patient_ID[is.na(patbdte)]]
# There are 9 patients with missing birthday records; REMOVE THEM
midas15$Patient_ID[is.na(midas15$patbdte)]
midas15 <- subset(midas15, 
                  !(Patient_ID %in% Patient_ID[is.na(patbdte)]))
summary(midas15$patbdte)

# People born before 1886s
subset(midas15, patbdte < "1886-01-01")

# Number of hospitals
unique(midas15$HOSP)
length(unique(midas15$HOSP))
table(midas15$HOSP)
sum(is.na(midas15$HOSP))
# 95 + 1(#2000, error code?)

# Convert to factors
# Sex
midas15[, SEX := factor(SEX, levels = c("F", "M"))]

# Race
table(midas15$RACE)
midas15$RACE1 <- "Other"
midas15$RACE1[midas15$RACE == 1] <- "White"
midas15$RACE1[midas15$RACE == 2] <- "Black"
midas15[, RACE1 := factor(RACE1,
                          levels = c("White",
                                     "Black",
                                     "Other"))]
table(midas15$RACE1)
midas15[, RACE := NULL]
names(midas15)[ncol(midas15)] <- "RACE"

# Primary insurance
midas15$PRIME[!(midas15$PRIME %in% c("medicaid",
                                     "medicare",
                                     "self pay" ,
                                     "COMMERCIAL"))] <- "Other"
midas15$PRIME[midas15$PRIME %in% c("medicaid",
                                   "self pay")] <- "medicaid/self-pay"
table(midas15$PRIME)

# Hispanic
table(midas15$HISPAN)
midas15$HISP <- "Hispanic"
midas15$HISP[midas15$HISPAN %in% c(".", "9", "A")] <- "Unknown"
midas15$HISP[midas15$HISPAN == "0"] <- "Non-hispanic"
midas15$HISP <- factor(midas15$HISP,
                       levels = c("Hispanic",
                                  "Non-hispanic",
                                  "Unknown"))
table(midas15$HISP)
midas15[, HISPAN := NULL]
names(midas15)[ncol(midas15)] <- "HISPAN"
gc()

summary(midas15)
gc()

setkey(midas15,
       Patient_ID,
       ADMDAT)
midas15[, NDX := 1:.N,
        by = Patient_ID]

# Keep only records from 01/01/1995 (admission dates)
midas15 <- subset(midas15, ADMDAT >= "1995-01-11")
gc()

# 3. Remove anybody who was younger than 20 at any admisson
midas15[, AGE := floor(as.numeric(difftime(ADMDAT, 
                                           patbdte,
                                           units = "days"))/365.25)]
hist(midas15$AGE, 100)
id.rm <- midas15$Patient_ID[midas15$AGE < 20]
midas15 <- subset(midas15, !(Patient_ID %in% id.rm))
gc()
hist(midas15$AGE, 100)

#**********************************************************
# Separate diagnostic codes (DX1:DX9)
dx <- midas15[, .(Patient_ID, DX1, DX2, DX3, DX4, DX5, DX6, DX7, DX8, DX9)]
cnames <- names(dx)

dx.1.3 <- copy(dx)
dx.1.3[, cnames[-1] := lapply(dx.1.3[, cnames[-1], with = FALSE],
                              substr,
                              start = 1,
                              stop = 3)]

# Separate procedure codes (PROC1:PROC9)
proc <- midas15[, .(Patient_ID, PROC1, PROC2, PROC3, PROC4, PROC5, PROC6, PROC7, PROC8)]

dt1 <- data.table(id = midas15$Patient_ID)

save(midas15, dx, dx.1.3, proc, 
     file = file.path(DATA_HOME, "midas15_mi2hf_04082017.RData"), 
     compress = FALSE)

#**********************************************************
# PART II----
load(file.path(DATA_HOME, "midas15_mi2hf_04082017.RData"))

# Vascular Disease: Miocardial Infarction
# 410 Acute myocardial infarction
## 410.0 Acute myocardial infarction of anterolateral wall
### 410.00 Acute myocardial infarction of anterolateral wall, episode of care unspecified
### 410.01 Acute myocardial infarction of anterolateral wall, initial episode of care
### 410.02 Acute myocardial infarction of anterolateral wall, subsequent episode of care
## 410.1 Acute myocardial infarction of other anterior wall
### 410.10 Acute myocardial infarction of other anterior wall, episode of care unspecified
### 410.11 Acute myocardial infarction of other anterior wall, initial episode of care
### 410.12 Acute myocardial infarction of other anterior wall, subsequent episode of care
## 410.2 Acute myocardial infarction of inferolateral wall
### 410.20 Acute myocardial infarction of inferolateral wall, episode of care unspecified
### 410.21 Acute myocardial infarction of inferolateral wall, initial episode of care
### 410.22 Acute myocardial infarction of inferolateral wall, subsequent episode of care
## 410.3 Acute myocardial infarction of inferoposterior wall
### 410.30 Acute myocardial infarction of inferoposterior wall, episode of care unspecified
### 410.31 Acute myocardial infarction of inferoposterior wall, initial episode of care
### 410.32 Acute myocardial infarction of inferoposterior wall, subsequent episode of care
## 410.4 Acute myocardial infarction of other inferior wall
### 410.40 Acute myocardial infarction of other inferior wall, episode of care unspecified convert
### 410.41 Acute myocardial infarction of other inferior wall, initial episode of care
### 410.42 Acute myocardial infarction of other inferior wall, subsequent episode of care
## 410.5 Acute myocardial infarction of other lateral wall
### 410.50 Acute myocardial infarction of other lateral wall, episode of care unspecified
### 410.51 Acute myocardial infarction of other lateral wall, initial episode of care
### 410.52 Acute myocardial infarction of other lateral wall, subsequent episode of care
## 410.6 True posterior wall infarction
### 410.60 True posterior wall infarction, episode of care unspecified
### 410.61 True posterior wall infarction, initial episode of care
### 410.62 True posterior wall infarction, subsequent episode of care
## 410.7 Subendocardial infarction
### 410.70 Subendocardial infarction, episode of care unspecified
### 410.71 Subendocardial infarction, initial episode of care
### 410.72 Subendocardial infarction, subsequent episode of care
## 410.8 Acute myocardial infarction of other specified sites
### 410.80 Acute myocardial infarction of other specified sites, episode of care unspecified
### 410.81 Acute myocardial infarction of other specified sites, initial episode of care
### 410.82 Acute myocardial infarction of other specified sites, subsequent episode of care
## 410.9 Acute myocardial infarction of unspecified site
### 410.90 Acute myocardial infarction of unspecified site, episode of care unspecified
### 410.91 Acute myocardial infarction of unspecified site, initial episode of care
### 410.92 Acute myocardial infarction of unspecified site, subsequent episode of care
dt1[, ami := factor(as.numeric(rowSums(dx.1.3 == "410") > 0))]
addmargins(table(dt1$ami))

# Vascular Disease: Myocardial infarction of anterolateral wall: 410.0x
dt1[, ami.antlat := factor(as.numeric(rowSums(dx.1.4 == "4100") > 0))]
addmargins(table(dt1$ami.antlat))

# Vascular Disease: Myocardial infarction of other anterior wall: 410.1x
dt1[, ami.ant := factor(as.numeric(rowSums(dx.1.4 == "4101") > 0))]
addmargins(table(dt1$ami.ant))

# Vascular Disease: Myocardial infarction of inferolateral, inferoposterior, 
# inferior or true posterior walls: 410.2x, 410.3x, 410.4x and 410.6x
dt1[, ami.inf := factor(as.numeric(rowSums(data.table(rowSums(dx.1.4 == "4102", na.rm = TRUE),
                                                      rowSums(dx.1.4 == "4103", na.rm = TRUE),
                                                      rowSums(dx.1.4 == "4104", na.rm = TRUE),
                                                      rowSums(dx.1.4 == "4106", na.rm = TRUE))) > 0))]
addmargins(table(dt1$ami.inf))

# Vascular Disease: Myocardial infarction of other lateral wall: 410.5x
dt1[, ami.lat := factor(as.numeric(rowSums(dx.1.4 == "4105") > 0))]
addmargins(table(dt1$ami.lat))

# Vascular Disease: Myocardial infarction of subendocardial wall: 410.5x
# NOTE: BEST (JK)
dt1[, ami.subec := factor(as.numeric(rowSums(dx.1.4 == "4107") > 0))]
addmargins(table(dt1$ami.subec))

# Vascular Disease: Myocardial infarction of unspecified sites: 410.8x and 410.9x
dt1[, ami.other := factor(as.numeric(rowSums(data.table(rowSums(dx.1.4 == "4108", na.rm = TRUE),
                                                        rowSums(dx.1.4 == "4109", na.rm = TRUE))) > 0))]
addmargins(table(dt1$ami.other))
########################################
# EXCLUSIONS 

# 1. Keep only patients with MI records between 01/01/2000 and 12/31/20011 (discharge dates)
# to ensure at least 5 year look-back and 1 year follow-up
as.Date(max(midas$DSCHDAT), origin = "1960-01-01")
as.Date(14610, origin = "1960-01-01")
as.Date(18992, origin = "1960-01-01")
id.keep <- unique(midas$PATIENT_ID[midas$DSCHDAT >= 14610 & midas$DSCHDAT <= 18992 & dt1$ami == 1])
rec.keep <- which(midas$PATIENT_ID%in%id.keep)

midas <- midas[rec.keep, ]
dx <- dx[rec.keep, ]
proc <- proc[rec.keep, ]
dx.1.3 <- dx.1.3[rec.keep, ]
dt1 <- dt1[rec.keep, ]

rm(id.keep,
   rec.keep)
gc()
length(unique(midas$PATIENT_ID))
# 249,381 patients

# 2. Cancers
# See "C:\Users\ds752\Documents\svn_local\trunk\Cardiovascular\Jen Wellings HF After MI\ICD-9-CM Cancer Codes Exclude.txt" for details)
excl <- as.character(c(140:165, 170:176, 180:239))
excl.rec <- apply(dx.1.3[, -1, with = FALSE], 1, FUN = function(a) return(sum(a %in% excl, na.rm = TRUE) > 0))
table(excl.rec)
id.rm <- unique(midas$PATIENT_ID[excl.rec])
rec.keep <- which(!(midas$PATIENT_ID %in% id.rm))

midas <- midas[rec.keep, ]
dx <- dx[rec.keep, ]
proc <- proc[rec.keep, ]
dx.1.3 <- dx.1.3[rec.keep, ]
dt1 <- dt1[rec.keep, ]

rm(excl,
   excl.rec,
   id.rm,
   rec.keep)
gc()

length(unique(midas$PATIENT_ID))
# 1,249,550 records of 184,761 patients

# save(midas, file = "jen_midas_locked_04232016.RData", compress = FALSE)
# ########################################
# system.time(load("jen_midas_locked_04232016.RData"))

########################################
# Conditions
# See "C:\Users\ds752\Documents\svn_local\trunk\Cardiovascular\MCHADS\source\final\MIDAS ICD-9 Code Counts.pptx" for details
# 1a. Acute CFH
dt1[, chf.acute := factor(as.numeric(rowSums(data.table(rowSums(dx == "4280", na.rm = TRUE),
                                                        rowSums(dx == "4281", na.rm = TRUE),
                                                        rowSums(dx == "42820", na.rm = TRUE),
                                                        rowSums(dx == "42821", na.rm = TRUE),
                                                        rowSums(dx == "42823", na.rm = TRUE),
                                                        rowSums(dx == "42830", na.rm = TRUE),
                                                        rowSums(dx == "42831", na.rm = TRUE),
                                                        rowSums(dx == "42833", na.rm = TRUE),
                                                        rowSums(dx == "42840", na.rm = TRUE),
                                                        rowSums(dx == "42841", na.rm = TRUE),
                                                        rowSums(dx == "42842", na.rm = TRUE),
                                                        rowSums(dx == "42843", na.rm = TRUE))) > 0))]
gc()
table(dt1$chf.acute)

# 1b. Chronic CFH
dt1[, chf.chron := factor(as.numeric(rowSums(data.table(rowSums(dx == "42822", na.rm = TRUE),
                                                        rowSums(dx == "42832", na.rm = TRUE))) > 0))]
gc()
table(dt1$chf.chron)

# 2. Hypertension
dt1[, hyp.401 := factor(as.numeric(rowSums(data.table(rowSums(dx == "4010", na.rm = TRUE),
                                                      rowSums(dx == "4011", na.rm = TRUE),
                                                      rowSums(dx == "4019", na.rm = TRUE))) > 0))]
table(dt1$hyp.401)

dt1[, hyp.402 := factor(as.numeric(rowSums(data.table(rowSums(dx == "40200", na.rm = TRUE),
                                                      rowSums(dx == "40201", na.rm = TRUE),
                                                      rowSums(dx == "40290", na.rm = TRUE),
                                                      rowSums(dx == "40291", na.rm = TRUE))) > 0))]
table(dt1$hyp.402)

dt1[, hyp.403 := factor(as.numeric(rowSums(data.table(rowSums(dx == "40300", na.rm = TRUE),
                                                      rowSums(dx == "40301", na.rm = TRUE),
                                                      rowSums(dx == "40310", na.rm = TRUE),
                                                      rowSums(dx == "40311", na.rm = TRUE),
                                                      rowSums(dx == "40390", na.rm = TRUE),
                                                      rowSums(dx == "40391", na.rm = TRUE))) > 0))]
table(dt1$hyp.403)

dt1[, hyp.404 := factor(as.numeric(rowSums(data.table(rowSums(dx == "40400", na.rm = TRUE),
                                                      rowSums(dx == "40401", na.rm = TRUE),
                                                      rowSums(dx == "40402", na.rm = TRUE),
                                                      rowSums(dx == "40403", na.rm = TRUE),
                                                      rowSums(dx == "40410", na.rm = TRUE),
                                                      rowSums(dx == "40411", na.rm = TRUE),
                                                      rowSums(dx == "40412", na.rm = TRUE),
                                                      rowSums(dx == "40413", na.rm = TRUE),
                                                      rowSums(dx == "40490", na.rm = TRUE),
                                                      rowSums(dx == "40491", na.rm = TRUE),
                                                      rowSums(dx == "40492", na.rm = TRUE),
                                                      rowSums(dx == "40493", na.rm = TRUE))) > 0))]
table(dt1$hyp.404)

dt1[, hyp.405 := factor(as.numeric(rowSums(data.table(rowSums(dx == "40501", na.rm = TRUE),
                                                      rowSums(dx == "40509", na.rm = TRUE),
                                                      rowSums(dx == "40511", na.rm = TRUE),
                                                      rowSums(dx == "40519", na.rm = TRUE),
                                                      rowSums(dx == "40591", na.rm = TRUE),
                                                      rowSums(dx == "40599", na.rm = TRUE))) > 0))]
table(dt1$hyp.405)

dt1[, hyp := factor(as.numeric(rowSums(dt1[, c("hyp.401",
                                               "hyp.402",
                                               "hyp.403",
                                               "hyp.404",
                                               "hyp.405"), with = FALSE] == 1) > 0))]
table(dt1$hyp)

# 3. Diabetes
dt1[, diab := factor(as.numeric(rowSums(dx.1.3 == "250", na.rm = TRUE) > 0))]
table(dt1$diab)

# 4. Chronic liver disease and cirrhosis
# 571 Chronic liver disease and cirrhosis
## 571.0 Alcoholic fatty liver
## 571.1 Acute alcoholic hepatitis
## 571.2 Alcoholic cirrhosis of liver
## 571.3 Alcoholic liver damage, unspecified
## 571.4 Chronic hepatitis
### 571.40 Chronic hepatitis, unspecified
### 571.41 Chronic persistent hepatitis
### 571.42 Autoimmune hepatitis
### 571.49 Other chronic hepatitis
### 571.5 Cirrhosis of liver without mention of alcohol
### 571.6 Biliary cirrhosis
### 571.8 Other chronic nonalcoholic liver disease
### 571.9 Unspecified chronic liver disease without mention of alcohol
dt1[, cld := factor(as.numeric(rowSums(dx.1.3 == "571", na.rm = TRUE) > 0))]
table(dt1$cld)

# # 5. Acute kidney failure
# # 584 Acute kidney failure
# ## 584.5 Acute kidney failure with lesion of tubular necrosis
# ## 584.6 Acute kidney failure with lesion of renal cortical necrosis
# ## 584.7 Acute kidney failure with lesion of renal medullary [papillary] necrosis
# ## 584.8 Acute kidney failure with other specified pathological lesion in kidney
# ## 584.9 Acute kidney failure, unspecified
# dt1[, akf := factor(as.numeric(rowSums(dx.1.3 == "584", na.rm = TRUE) > 0))]
# table(dt1$akf)

# 5a. Chronic kidney disease
# 585 Chronic kidney disease (ckd)
## 585.1 Chronic kidney disease, Stage I
## 585.2 Chronic kidney disease, Stage II (mild)
## 585.3 Chronic kidney disease, Stage III (moderate)
## 585.4 Chronic kidney disease, Stage IV (severe)
## 585.5 Chronic kidney disease, Stage V
## 585.6 End stage renal disease
## 585.9 Chronic kidney disease, unspecified
dt1[, ckf := factor(as.numeric(rowSums(dx.1.3 == "585", na.rm = TRUE) > 0))]
table(dt1$ckf)

# 6. COPD
# 490 Bronchitis, not specified as acute or chronic
# 491 Chronic bronchitis
# 492 Emphysema
# 493 Asthma
# 494 Bronchiectasis
# 495 Extrinsic allergic alveolitis
# 496 Chronic airway obstruction, not elsewhere classified
dt1[, copd := factor(as.numeric(rowSums(data.table(rowSums(dx.1.3 == "490", na.rm = TRUE),
                                                   rowSums(dx.1.3 == "491", na.rm = TRUE),
                                                   rowSums(dx.1.3 == "492", na.rm = TRUE),
                                                   rowSums(dx.1.3 == "493", na.rm = TRUE),
                                                   rowSums(dx.1.3 == "494", na.rm = TRUE),
                                                   rowSums(dx.1.3 == "495", na.rm = TRUE),
                                                   rowSums(dx.1.3 == "496", na.rm = TRUE))) > 0))]
table(dt1$copd)

# 7. Disorders of lipoid metabolism
# 272 Disorders of lipoid metabolism
## 272.0 Pure hypercholesterolemia
## 272.1 Pure hyperglyceridemia
## 272.2 Mixed hyperlipidemia
## 272.3 Hyperchylomicronemia
## 272.4 Other and unspecified hyperlipidemia
## 272.5 Lipoprotein deficiencies
## 272.6 Lipodystrophy
## 272.7 Lipidoses
## 272.8 Other disorders of lipoid metabolism
## 272.9 Unspecified disorder of lipoid metabolism
dt1[, lipid := factor(as.numeric(rowSums(dx.1.3 == "272", na.rm = TRUE) > 0))]
table(dt1$lipid)

# Merge conditions with demographic data
dt1[, c("bdat",
        "deathdat",
        "age",
        "dschdat",
        "dschyear",
        "sex",
        "race") := list(midas$PATBDTE,
                        midas$NEWDTD,
                        floor((midas$DSCHDAT - midas$PATBDTE)/365.25),
                        midas$DSCHDAT,
                        substr(as.Date(midas$DSCHDAT,
                                       origin = "1960-01-01"),
                               start = 1,
                               stop = 4),
                        midas$SEX,
                        midas$RACE)]

conditions <- copy(dt1)

# Clean memory
rm(dx,
   dx.1.3,
   dx.1.4)
gc()
conditions
summary(conditions)

########################################
# First MI records between 01/01/2000 and 12/31/2011
setkey(conditions, dschdat)
setkey(conditions, id)
conditions
length(unique(conditions$id))
range(conditions$dschdat)
as.Date(14610, origin = "1960-01-01")
as.Date(18992, origin = "1960-01-01")

conditions[, first := min(dschdat[ami == 1 & 
                                    dschdat >= 14610 &
                                    dschdat <= 18992],
                          na.rm = TRUE), 
           by = id]

#  Admissions prior to MI (5 years look-back)
conditions[, prior := (dschdat < first) & 
             (dschdat > first - 5*365.25)]
conditions[, id := factor(id)]
conditions <- droplevels(conditions)
summary(conditions)

# Outcomes and histories (prior to 1st MI)
hh <- conditions[ , list(post.chf.acute.30 = factor(as.numeric(sum((chf.acute == 1) & 
                                                                     !prior &
                                                                     (dschdat > first) &
                                                                     (dschdat < first + 31)) > 0), 
                                                    levels = 0:1),
                         post.chf.chron.30 = factor(as.numeric(sum(chf.chron == 1 & 
                                                                     !prior &
                                                                     dschdat > first &
                                                                     dschdat < first + 31) > 0), 
                                                    levels = 0:1),
                         post.chf.acute.90 = factor(as.numeric(sum(chf.acute == 1 & 
                                                                     !prior &
                                                                     dschdat > first &
                                                                     dschdat < first + 91) > 0), 
                                                    levels = 0:1),
                         post.chf.chron.90 = factor(as.numeric(sum(chf.chron == 1 & 
                                                                     !prior &
                                                                     dschdat > first &
                                                                     dschdat < first + 91) > 0), 
                                                    levels = 0:1),
                         post.chf.acute.180 = factor(as.numeric(sum(chf.acute == 1 & 
                                                                      !prior &
                                                                      dschdat > first &
                                                                      dschdat < first + 181) > 0), 
                                                     levels = 0:1),
                         post.chf.chron.180 = factor(as.numeric(sum(chf.chron == 1 & 
                                                                      !prior &
                                                                      dschdat > first &
                                                                      dschdat < first + 181) > 0), 
                                                     levels = 0:1),
                         post.chf.acute.1y = factor(as.numeric(sum(chf.acute == 1 & 
                                                                     !prior &
                                                                     dschdat > first &
                                                                     dschdat < first + 366) > 0), 
                                                    levels = 0:1),
                         post.chf.chron.1y = factor(as.numeric(sum(chf.chron == 1 & 
                                                                     !prior &
                                                                     dschdat > first &
                                                                     dschdat < first + 366) > 0), 
                                                    levels = 0:1),
                         hami = factor(as.numeric(sum(ami == 1 & prior) > 0), 
                                       levels = 0:1),
                         hami.antlat = factor(as.numeric(sum(ami.antlat == 1 & prior) > 0), 
                                              levels = 0:1),
                         hami.ant = factor(as.numeric(sum(ami.ant == 1 & prior) > 0), 
                                           levels = 0:1),
                         hami.inf = factor(as.numeric(sum(ami.inf == 1 & prior) > 0), 
                                           levels = 0:1),
                         hami.lat = factor(as.numeric(sum(ami.lat == 1 & prior) > 0), 
                                           levels = 0:1),
                         hami.subec = factor(as.numeric(sum(ami.subec == 1 & prior) > 0), 
                                             levels = 0:1),
                         hami.other = factor(as.numeric(sum(ami.other == 1 & prior) > 0), 
                                             levels = 0:1),
                         hchf.acute = factor(as.numeric(sum(chf.acute == 1 & prior) > 0), 
                                             levels = 0:1),
                         hchf.chron = factor(as.numeric(sum(chf.chron == 1 & prior) > 0), 
                                             levels = 0:1),
                         hhyp.401 = factor(as.numeric(sum(hyp.401 == 1 & prior) > 0), 
                                           levels = 0:1),
                         hhyp.402 = factor(as.numeric(sum(hyp.402 == 1 & prior) > 0), 
                                           levels = 0:1),
                         hhyp.403 = factor(as.numeric(sum(hyp.403 == 1 & prior) > 0), 
                                           levels = 0:1),
                         hhyp.404 = factor(as.numeric(sum(hyp.404 == 1 & prior) > 0), 
                                           levels = 0:1),
                         hhyp.405 = factor(as.numeric(sum(hyp.405 == 1 & prior) > 0), 
                                           levels = 0:1),
                         hhyp = factor(as.numeric(sum(hyp == 1 & prior) > 0), 
                                       levels = 0:1),
                         hdiab = factor(as.numeric(sum(diab == 1 & prior) > 0), 
                                        levels = 0:1),
                         hcld = factor(as.numeric(sum(cld == 1 & prior) > 0), 
                                       levels = 0:1),
                         hckf = factor(as.numeric(sum(ckf == 1 & prior) > 0), 
                                       levels = 0:1),
                         hcopd = factor(as.numeric(sum(copd == 1 & prior) > 0), 
                                        levels = 0:1),
                         hlipid = factor(as.numeric(sum(lipid == 1 & prior) > 0), 
                                         levels = 0:1)),
                  by = id]
summary(hh)

# Remove patients with history of AMI
hh$id <- as.character(hh$id)
id.keep <- hh$id[hh$hami == 0]
hh <- subset(hh, subset = hh$id %in% id.keep)
summary(hh)

# Remove AMIs
hh$hami <- hh$hami.antlat <- hh$hami.ant <- hh$hami.inf <- hh$hami.lat <- hh$hami.subec <- hh$hami.other <- NULL
hh

# Merge AMI visit with histories and outcomes
ami.visit <- subset(conditions, ami == 1 & dschdat == first)

# Remove duplicates
ami.visit <- unique(ami.visit)
setkey(ami.visit, id)
ami.visit$id <- as.character(ami.visit$id)

# NOTE: there are 340 subjects with conflicting records: REMOVE
sum(duplicated(ami.visit$id))
id.dupl <- ami.visit$id[duplicated(ami.visit$id)]
subset(ami.visit, id %in% id.dupl)

ami.visit <- subset(ami.visit, !(id %in% id.dupl))

# Merge all
case <- merge(ami.visit, hh, by = "id")

# Remove 'first' and 'prior'
case$first <- NULL
case$prior <- NULL
summary(case)

# Remove subjects that died during at 1st MI admission, if any
case <- droplevels(subset(case, (is.na(deathdat) | dschdat <= deathdat)))
summary(case)

#############################################
# Save all
save(case, file = "tmp/jen_case_07092016.RData", compress = FALSE)
case