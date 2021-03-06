# |----------------------------------------------------------------------------------|
# | Project: Heart Failures after MIs in MIDAS                                       |
# | Script: Data analysis                                                            |
# | Authors: Jen Wellings; Davit Sargsyan                                            |   
# | Created: 04/21/2017                                                              |
# | Modified: 07/08/2017, kept inpatients only                                       |
# |           07/29/2017, added demographics table; compared with census data        |
# |----------------------------------------------------------------------------------|
# Header----
# Save consol output to a log file
sink(file = "tmp/log_analysis_v3.txt")

# Move up one directory
wd <- getwd()
setwd("..")
DATA_HOME <- paste(getwd(),
                   "data/midas.mi2hf",
                   sep = "/")
# Reset working directory
setwd(wd)
getwd()

# Load packages
require(data.table)
require(ggplot2)
require(gridExtra)
require(knitr)

# PART I: Load data----
load(file.path(DATA_HOME, "case_07292017.RData"))
case[, hhyp.401 := NULL]
case[, hhyp.402 := NULL]
case[, hhyp.403 := NULL]
case[, hhyp.404 := NULL]
case[, hhyp.405 := NULL]
gc()

# AMI discharges----
t1 <- table(case$dschyear)
t1 <- data.table(Year = as.numeric(names(t1)),
                 Counts = c(t1))
t1

# Plot AMI discharges----
p1 <- ggplot(t1,
             aes(x = Year,
                 y = Counts)) +
  geom_line(size = 1) +
  geom_point(size = 3,
             shape = 21,
             fill = "red") +
  scale_x_continuous(breaks = 2000:2015,
                     limits = c(2000, 2017)) +
  scale_y_continuous(breaks = seq(8000, 12000, 500)) +
  ggtitle("Number of First AMI Admissions Between 2000 and 2015\n157,655 Patients Admitted For AMI For The First Time") +
  annotation_custom(grob = tableGrob(t1,
                                     rows = NULL,
                                     theme = ttheme_default(base_size = 9)),
                    xmin = 2015,
                    xmax = 2018,
                    ymin = 9000,
                    ymax = 11000) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p1

tiff(filename = "tmp/ami_per_year.tiff",
     height = 5.5,
     width = 8,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p1)
graphics.off()

# Pubication/Table1: Demographics----
summary(case)
table1 <- t(data.table(`Mean Age at First AMI Admission +/- S.D.` = paste(round(mean(case$AGE), 1),
                                                                          "+/-",
                                                                          round(sd(case$AGE), 1),
                                                                          sep = ""),
                       `Female(%)` = round(100*sum(case$SEX == "F")/nrow(case), 
                                           1),
                       `Race(%)` = "",
                       `  White` = round(100*sum(case$RACE == "White")/nrow(case), 
                                         1),
                       `  Black` = round(100*sum(case$RACE == "Black")/nrow(case), 
                                         1),
                       `  Other` = round(100*sum(case$RACE == "Other")/nrow(case), 
                                         1),
                       `Ethnicity(%)` = "",
                       `  Hispanic` = round(100*sum(case$HISPAN == "Hispanic")/nrow(case), 
                                            1),
                       `  Non-hispanic` = round(100*sum(case$HISPAN == "Non-hispanic")/nrow(case), 
                                                1),
                       `  Other` = round(100*sum(case$HISPAN == "Unknown")/nrow(case), 
                                         1),
                       `Insurance(%)` = "",
                       `  Commercial` = round(100*sum(case$PRIME == "COMMERCIAL")/nrow(case), 
                                              1),
                       `  Medicare` = round(100*sum(case$PRIME == "medicare")/nrow(case), 
                                            1),
                       `  Medicade/Self-Pay/Other` = round(100*sum(case$PRIME == "medicaid/self-pay/other")/nrow(case), 
                                                           1)))
colnames(table1) <- "Summary"
table1
kable(table1)

  # |                                         |Summary   |
  # |:----------------------------------------|:---------|
  # |Mean Age at First AMI Admission +/- S.D. |66.7+/-15 |
  # |Female(%)                                |39.7      |
  # |Race(%)                                  |          |
  # |White                                    |50.1      |
  # |Black                                    |42.2      |
  # |Other                                    |7.7       |
  # |Ethnicity(%)                             |          |
  # |Hispanic                                 |8.6       |
  # |Non-hispanic                             |80.6      |
  # |Other                                    |10.8      |
  # |Insurance(%)                             |          |
  # |Commercial                               |45.5      |
  # |Medicare                                 |46.7      |
  # |Medicade/Self-Pay/Other                  |7.9       |

# Race/Sex combination table----
t2 <- addmargins(table(case$SEX,
                       case$RACE))
kable(round(100*t2/t2[3, 4], 1))
# |    | White| Black| Other|   Sum|
# |:---|-----:|-----:|-----:|-----:|
# |F   |  20.3|  16.8|   2.6|  39.7|
# |M   |  29.8|  25.3|   5.1|  60.3|
# |Sum |  50.1|  42.2|   7.7| 100.0|

# Population adjustment (NJ by race)----
# Source: https://www.census.gov/cps/data/cpstablecreator.html
njpop <- fread("docs/race_sex_census_nj_2009_2015.csv")
njpop$race <- factor(njpop$race,
                     levels = unique(njpop$race))
p2 <- ggplot(njpop,
             aes(x = year,
                 y = count,
                 group = race,
                 fill = race)) +
  facet_wrap(~ sex) +
  geom_line(size = 1) +
  geom_point(size = 3,
             shape = 21) +
  scale_x_continuous("Year",
                     breaks = 2009:2015) +
  scale_y_continuous("Population(Thousands)") +
  ggtitle("NJ Population By Race and Sex Over Time") + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p2

tiff(filename = "tmp/nj_race_sex_census_2009_2015.tiff",
     height = 4,
     width = 6,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p2)
graphics.off()

# Since there was little change, take averages
t3 <- aggregate(njpop$count,
                FUN = mean,
                by = list(njpop$race,
                          njpop$sex))
colnames(t3) <- c("race", 
                  "sex",
                  "avg")
t3 <- data.table(t3)
t3$count2 <- 0
t3[, count2 := avg[race == "total"] - sum(avg[race != "total"]),
   by = sex]
t3$avg[t3$race == "total"] <- t3$count2[t3$race == "total"]
t3$race <- as.character(t3$race)
t3$race[t3$race == "total"] <- "other"
t3[, count2 := NULL]
t3$race <- factor(t3$race,
                  levels = c("white",
                             "black",
                             "other"))
t4 <- dcast.data.table(t3,
                       sex ~ race)
t4 <- t4[, -1]
t4$total <- rowSums(t4)
t4
t4 <- data.table(rbind(t4,
                       t(data.frame(colSums(t4)))))
rownames(t4) <- c("Female",
                  "Male",
                  "Total")
t4

# Average racial compositon among 18+ year old NJ residents (2009-2015 censors) ----
kable(round(100*as.matrix(t4)/as.matrix(t4)[3, 4], 1))
# | white| black| other| total|
# |-----:|-----:|-----:|-----:|
# |  39.5|   7.5|   4.9|    52|
# |  36.8|   6.0|   5.2|    48|
# |  76.2|  13.6|  10.2|   100|

# How much more likely to get AMI for a black NJ recident?----
t5 <- t2/t4
kable(data.table(Race = c("Black Female/White Female",
                    "Black Male/White Male",
                    "Black All/White All"),
           `AMI Risk Foldchage` = round(t5$black/t5$white, 1)))
# Risk foldchange, adjusted for population
# |Race                      | AMI Risk Foldchage|
# |:-------------------------|------------------:|
# |Black Female/White Female |                4.3|
# |Black Male/White Male     |                5.2|
# |Black All/White All       |                4.7|

kable(data.table(Race = c("Other Female/White Female",
                          "Other Male/White Male",
                          "Other All/White All"),
                 `AMI Risk Foldchage` = round(t5$other/t5$white, 1)))
  # |Race                      | AMI Risk Foldchage|
  # |:-------------------------|------------------:|
  # |Other Female/White Female |                1.0|
  # |Other Male/White Male     |                1.2|
  # |Other All/White All       |                1.1|
  
# Data processing----
# Make age by decade
case$decade <- floor(case$AGE/10)
summary(case$decade)
gc()

# History of acute CHF vs acute CHF recorded at 1st AMI admission----
t0 <- addmargins(table(`History of Acute CHF` = case$hchf.acute,
                       `Acute CHF at 1st AMI Admission` = case$chf.acute.current))
t0 <- round(100*t0/t0[3, 3], 1)
kable(t0)
#   |      |achf @ 1st ami     |
#   |h.achf| FALSE| TRUE|   Sum|
#   |:-----|-----:|----:|-----:|
#   |FALSE |  69.6| 18.3|  87.9|
#   |TRUE  |   4.5|  7.7|  12.1|
#   |Sum   |  74.0| 26.0| 100.0|

#**********************************************************
# PART II: Rates----
addmargins(table(case$dschyear))
# 1. All-cause death rates----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       case$days2death < 31 &
                          !is.na(case$days2death)))
t1 <- data.table(dschyear = rownames(t1),
                 death30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*death30/N, 2)]

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$days2death < 91 &
                         !is.na(case$days2death)))
t2 <- data.table(dschyear = rownames(t2),
                 death90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*death90/N, 2)]

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$days2death < 181 &
                         !is.na(case$days2death)))
t3 <- data.table(dschyear = rownames(t3),
                 death180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*death180/N, 2)]

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$days2death < 366 &
                         !is.na(case$days2death)))
t4 <- data.table(dschyear = rownames(t4),
                 death1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*death1y/N, 2)]

# Combine and plot----
tt1 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt1

# Remove sums and the year 2015
tt1 <- tt1[-c(nrow(tt1) -1,
              nrow(tt1)),]

# Melt to long format
death <- melt.data.table(data = tt1, 
                         id.vars = "dschyear",
                         measure.vars = c(2:5),
                         variable.name = "death",
                         value.name = "rate")
death

# Re-level
death$death <- factor(death$death,
                      levels = rev(levels(death$death)))
death

# Plot all-cause death rates----
p1 <- ggplot(data = death,
             aes(x = dschyear,
                 y = rate,
                 group = death,
                 fill = death)) +
  geom_line(size = 0.5) +
  geom_point(size = 2,
             shape = 21) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_discrete("1st AMI Discharge Year",
                   breaks = unique(death$dschyear),
                   labels = unique(death$dschyear)) +
  scale_y_continuous("Rate (%)") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(death$death)) +
  ggtitle("All-Cause Death After AMI Discharge") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Follow-up"))
p1

tiff(filename = "tmp/death_after_ami_rates.tiff",
     height = 5,
     width = 5,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p1)
graphics.off()

# 2. Readmissions due to HF after AMI----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 31 &
                         !is.na(case$days2post.chf.acute.dx1)))
t1 <- data.table(dschyear = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 91 &
                         !is.na(case$days2post.chf.acute.dx1)))
t2 <- data.table(dschyear = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 181 &
                         !is.na(case$days2post.chf.acute.dx1)))
t3 <- data.table(dschyear = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 366 &
                         !is.na(case$days2post.chf.acute.dx1)))
t4 <- data.table(dschyear = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Combine and plot----
tt2 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)

# Remove sums and the year 2015
tt2 <- tt2[-c(nrow(tt2) -1,
              nrow(tt2)),]

# Melt to long format
readm.hf.dx1 <- melt.data.table(data = tt2, 
                                 id.vars = "dschyear",
                                 measure.vars = c(2:5),
                                 variable.name = "readm",
                                 value.name = "rate")
readm.hf.dx1

# Plot HF admission rates----
readm.hf.dx1$readm <- factor(readm.hf.dx1$readm,
                              levels = rev(levels(readm.hf.dx1$readm)))
readm.hf.dx1

p2 <- ggplot(data = readm.hf.dx1,
             aes(x = dschyear,
                 y = rate,
                 group = readm,
                 fill = readm)) +
  geom_line(size = 0.5) +
  geom_point(size = 2,
             shape = 21) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_discrete("1st AMI Discharge Year",
                     breaks = unique(readm.hf.dx1$dschyear),
                     labels = unique(readm.hf.dx1$dschyear)) +
  scale_y_continuous("Rate (%)") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(readm.hf.dx1$readm)) +
  ggtitle("HF Admission After AMI Discharge") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Follow-up"))
p2

tiff(filename = "tmp/hf_after_ami_rates.tiff",
     height = 5,
     width = 5,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p2)
graphics.off()

# 3. Readmissions due to HF after AMI among survivals----
# 30 days----
tmp <- droplevels(subset(case,
                         ((days2cvdeath > 30 | is.na(days2cvdeath)) &
                            (days2post.chf.acute.dx1 > 30 | is.na(days2post.chf.acute.dx1))) |
                           ((days2post.chf.acute.dx1 < 31) &
                              (days2cvdeath - days2post.chf.acute.dx1 > 0))))
# 153,322 patinets
t1 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 31 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t1 <- data.table(dschyear = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# 90 days----
tmp <- droplevels(subset(case,
                         ((days2cvdeath > 90 | is.na(days2cvdeath)) &
                            (days2post.chf.acute.dx1 > 90 | is.na(days2post.chf.acute.dx1))) |
                           ((days2post.chf.acute.dx1 < 91) &
                              (days2cvdeath - days2post.chf.acute.dx1 > 0))))
# 150,040 patinets
t2 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 91 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t2 <- data.table(dschyear = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# 180 days----
tmp <- droplevels(subset(case,
                         ((days2cvdeath > 180 | is.na(days2cvdeath)) &
                            (days2post.chf.acute.dx1 > 180 | is.na(days2post.chf.acute.dx1))) |
                           ((days2post.chf.acute.dx1 < 181) &
                              (days2cvdeath - days2post.chf.acute.dx1 > 0))))
# 147,518 patients
t3 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 181 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t3 <- data.table(dschyear = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# 1 year----
tmp <- droplevels(subset(case,
                         ((days2cvdeath > 365 | is.na(days2cvdeath)) &
                            (days2post.chf.acute.dx1 > 365 | is.na(days2post.chf.acute.dx1))) |
                           ((days2post.chf.acute.dx1 < 366) &
                              (days2cvdeath - days2post.chf.acute.dx1 > 0))))
# 144,535 patients
t4 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 366 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t4 <- data.table(dschyear = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Combine and plot----
tt3 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)

# Remove sums and the year 2015
tt3 <- tt3[-c(nrow(tt3) -1,
              nrow(tt3)),]

# Melt to long format
readm.hf.dx1.alive <- melt.data.table(data = tt3, 
                                      id.vars = "dschyear",
                                      measure.vars = c(2:5),
                                      variable.name = "readm",
                                      value.name = "rate")
readm.hf.dx1.alive

# Plot HF admission rates----
readm.hf.dx1.alive$readm <- factor(readm.hf.dx1.alive$readm,
                             levels = rev(levels(readm.hf.dx1.alive$readm)))
readm.hf.dx1.alive

p3 <- ggplot(data = readm.hf.dx1.alive,
             aes(x = dschyear,
                 y = rate,
                 group = readm,
                 fill = readm)) +
  geom_line(size = 0.5) +
  geom_point(size = 2,
             shape = 21) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_discrete("1st AMI Discharge Year",
                   breaks = unique(readm.hf.dx1$dschyear),
                   labels = unique(readm.hf.dx1$dschyear)) +
  scale_y_continuous("Rate (%)") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(readm.hf.dx1$readm)) +
  ggtitle("HF Admission After AMI Discharge Among Patinets \n Who Survived Until HF Or The End Of Follow-up") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Follow-up"))
p3

tiff(filename = "tmp/hf_after_ami_alive_rates.tiff",
     height = 5,
     width = 5,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p3)
graphics.off()

# 4. Cardiovascular death rates----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       case$days2cvdeath < 31 &
                         !is.na(case$days2cvdeath)))
t1 <- data.table(dschyear = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$days2cvdeath < 91 &
                         !is.na(case$days2cvdeath)))
t2 <- data.table(dschyear = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$days2cvdeath < 181 &
                         !is.na(case$days2cvdeath)))
t3 <- data.table(dschyear = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$days2cvdeath < 366 &
                         !is.na(case$days2cvdeath)))
t4 <- data.table(dschyear = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Combine and plot----
tt4 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)

# Remove sums and the year 2015
tt4 <- tt4[-c(nrow(tt4) -1,
              nrow(tt4)),]

# Melt to long format
cvdeath <- melt.data.table(data = tt4, 
                           id.vars = "dschyear",
                           measure.vars = c(2:5),
                           variable.name = "cvdeath",
                           value.name = "rate")
cvdeath

# Plot CV death rates----
cvdeath$readm <- factor(cvdeath$cvdeath,
                             levels = rev(levels(cvdeath$cvdeath)))
cvdeath

p4 <- ggplot(data = cvdeath,
             aes(x = dschyear,
                 y = rate,
                 group = cvdeath,
                 fill = cvdeath)) +
  geom_line(size = 0.5) +
  geom_point(size = 2,
             shape = 21) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_discrete("1st AMI Discharge Year",
                   breaks = unique(readm.hf.dx1$dschyear),
                   labels = unique(readm.hf.dx1$dschyear)) +
  scale_y_continuous("Rate (%)") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(readm.hf.dx1$readm)) +
  ggtitle("Cardiovascular Death After AMI Discharge") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Follow-up"))
p4

tiff(filename = "tmp/cvdeath_after_ami_rates.tiff",
     height = 5,
     width = 5,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p4)
graphics.off()

# 5. Readmissions due to AMI rates, no acute CHF prior or at 1st AMI admission----
tmp <- droplevels(subset(case,
                         !(hchf.acute | chf.acute.current)))
# 109,717 patients

# 30 days----
t1 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 31 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t1 <- data.table(dschyear = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# Remove total and 2015
t1 <- t1[-c((nrow(t1) - 1):nrow(t1)),]
t1$dschyear <- as.numeric(t1$dschyear)

# 90 days----
t2 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 91 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t2 <- data.table(dschyear = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# Remove total and 2015
t2 <- t2[-c((nrow(t2) - 1):nrow(t2)),]
t2$dschyear <- as.numeric(t2$dschyear)

# 180 days----
t3 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 181 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t3 <- data.table(dschyear = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# Remove total and 2015
t3 <- t3[-c((nrow(t3) - 1):nrow(t3)),]
t3$dschyear <- as.numeric(t3$dschyear)

# 1 year----
t4 <- addmargins(table(tmp$dschyear,
                       tmp$days2post.chf.acute.dx1 < 366 &
                         !is.na(tmp$days2post.chf.acute.dx1)))
t4 <- data.table(dschyear = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Remove total and 2015
t4 <- t4[-c((nrow(t4) - 1):nrow(t4)),]
t4$dschyear <- as.numeric(t4$dschyear)

# Combine and plot----
tt5 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt5

# Melt to long format
readm.hf.dx1.no.hcf <- melt.data.table(data = tt5, 
                                       id.vars = "dschyear",
                                       measure.vars = c(2:5),
                                       variable.name = "readm",
                                       value.name = "rate")
readm.hf.dx1.no.hcf

# Plot HF admission rates----
readm.hf.dx1.no.hcf$readm <- factor(readm.hf.dx1.no.hcf$readm,
                                    levels = rev(levels(readm.hf.dx1.no.hcf$readm)))
readm.hf.dx1.no.hcf

p5 <- ggplot(data = readm.hf.dx1.no.hcf,
             aes(x = dschyear,
                 y = rate,
                 group = readm,
                 fill = readm)) +
  geom_line(size = 0.5) +
  geom_point(size = 2,
             shape = 21) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_continuous("1st AMI Discharge Year",
                   breaks = unique(readm.hf.dx1.no.hcf$dschyear),
                   labels = unique(readm.hf.dx1.no.hcf$dschyear)) +
  scale_y_continuous("Rate (%)") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(readm.hf.dx1.no.hcf$readm)) +
  ggtitle("HF Admission After AMI Discharge \n Among Patinets  With No Prior HF") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Follow-up"))
p5

tiff(filename = "tmp/hf_after_ami_no_hist_of_hf_rates.tiff",
     height = 5,
     width = 5,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p3)
graphics.off()

#**********************************************************
# PART III: Models----
# 1. All-cause death----
m1.30 <- glm((days2death < 31) ~ dschyear +
               decade + 
               SEX +
               HISPAN +
               PRIME +
               hchf.acute +
               chf.acute.current +
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
summary(m1.30)

m1.90 <- glm((days2death < 91) ~ dschyear +
               decade + 
               SEX +
               HISPAN +
               PRIME +
               hchf.acute +
               chf.acute.current +
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
summary(m1.90)

m1.180 <- glm((days2death < 181) ~ dschyear +
                decade + 
                SEX +
                HISPAN +
                PRIME +
                hchf.acute +
                chf.acute.current +
                hchf.chron +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = case)
summary(m1.180)

m1.1y <- glm((days2death < 366) ~ dschyear +
               decade + 
               SEX +
               HISPAN +
               PRIME +
               hchf.acute +
               chf.acute.current +
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
summary(m1.1y)

# Combine
t.col.names <- c("Discharge year",
                 "Age (by Decade)",
                 "Sex (Male)",
                 "Non-Hispanic vs. Hispanic",
                 "Unknown vs. Hispanic",
                 "Commercial Insurance vs. Medicare",
                 "Medicade/Self-Pay/Other vs. Medicare",
                 "History of Acute CHF",
                 "Co-Diagnisis of Acute CHF",
                 "History of Chronic CHF",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Liver Disease",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism")
s1 <- list(summary(m1.30),
           summary(m1.90),
           summary(m1.180),
           summary(m1.1y))

tm1 <- lapply(s1, function(a) {
  out <- data.table(names = rownames(a$coefficients)[-1],
                    m1.cf = exp(a$coefficients[-1, 1]),
                    m1.lb = exp(a$coefficients[-1, 1] - 1.96*a$coefficients[-1, 2]),
                    m1.ub = exp(a$coefficients[-1, 1] + 1.96*a$coefficients[-1, 2]),
                    pval = ifelse(a$coefficients[-1, 4] >= 0.001,
                                  round(a$coefficients[-1, 4], 3),
                                  "<0.001"),
                    star = ifelse(a$coefficients[-1, 4] <= 0.05, 
                                  ifelse(a$coefficients[-1, 4] <= 0.01,
                                         "**",
                                         "*"), ""))
  rownames(out) <- NULL
  out$names <- t.col.names
  return(out)
})

tm1 <- do.call("rbind", 
               tm1)
tm1 <- data.table(cutoff = rep(c("30 Days",
                                 "90 Days",
                                 "180 Days",
                                 "1 Year"),
                               each = length(t.col.names)),
                  tm1)
tm1$names <- factor(tm1$names, 
                    levels = unique(tm1$names))
tm1$cutoff <- factor(as.character(tm1$cutoff),
                     levels = c("30 Days", 
                                "90 Days", 
                                "180 Days", 
                                "1 Year"))
tm1
write.csv(tm1, 
          file = "tmp/all-cause_death_after_ami_or.csv",
          row.names = FALSE)

# Plot OR----
p6 <- ggplot(tm1, 
             aes(x = names, 
                 y = m1.cf)) +
  facet_wrap( ~ cutoff, 
              ncol = 2) + 
  geom_hline(yintercept = 1,
             colour = "grey",
             size = 1.1,
             linetype = 2) +
  geom_errorbar(aes(ymin = m1.lb,
                    ymax = m1.ub),
                size = 0.4,
                width = 0.2) +
  geom_point(aes(x = names, 
                 y = m1.cf,
                 fill = names),
             size = 2,
             shape = 21) +
  scale_fill_manual(labels = paste(LETTERS[1:nlevels(tm1$names)],
                                   levels(tm1$names),
                                   sep = ": "),
                    values = rainbow(nlevels(tm1$names))) +
  scale_x_discrete("Risk Factors",
                   labels = LETTERS[1:nlevels(tm1$names)]) + 
  scale_y_continuous("Odds Ratios") + 
  ggtitle("Odds Ratios of All-Cause Death After First AMI Discharge") +
  theme(axis.text.x = element_text(size = 12,
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend("Risk Factors Legend"))
p6

tiff(filename = "tmp/all-cause_death_after_ami_or.tiff",
     height = 8,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p6)
graphics.off()

# Table1: OR, 95% C.I., and p-values----
tm1$est <- paste(round(tm1$m1.cf, 3),
                 " (",
                 round(tm1$m1.lb, 3),
                 ", ",
                 round(tm1$m1.ub, 3),
                 ")\n",
                 tm1$pval,
                 sep = "")
tm2 <- dcast.data.table(data = tm1,
                        names ~ cutoff,
                        value.var = "est")
names(tm2)[1] <- "Risk Factor"
tm2
write.csv(tm2, 
          file = "tmp/all-cause_death_after_ami_or_combined.csv",
          row.names = FALSE)
