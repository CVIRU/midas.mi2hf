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
table1
kable(table1)
  # |                                         |          |
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
# kable(t0)
#   |      |achf @ 1st ami     |
#   |h.achf| FALSE| TRUE|   Sum|
#   |:-----|-----:|----:|-----:|
#   |FALSE |  69.6| 18.3|  87.9|
#   |TRUE  |   4.5|  7.7|  12.1|
#   |Sum   |  74.0| 26.0| 100.0|

#**********************************************************
# PART II: Rates----
# a. All-Cause Death----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       case$days2death < 31))
t1 <- data.table(dschyear = rownames(t1),
                 death30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*death30/N, 2)]

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$days2death < 91))
t2 <- data.table(dschyear = rownames(t2),
                 death90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*death90/N, 2)]

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$days2death < 181))
t3 <- data.table(dschyear = rownames(t3),
                 death180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*death180/N, 2)]

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$days2death < 366))
t4 <- data.table(dschyear = rownames(t4),
                 death1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*death1y/N, 2)]

# Combine and plot----
merge
tt1 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)

# Remove 2015 and the sum
tt1 <- tt1[-c(16, 17)]
tt1

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
                   breaks = unique(readm.hf.dx1$dschyear),
                   labels = unique(readm.hf.dx1$dschyear)) +
  scale_y_continuous("Rate (%)") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(readm.hf.dx1$readm)) +
  ggtitle("All-Cause Death After AMI Discharge") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Follow-up"))
p1

tiff(filename = "tmp/death_after_ami_rates.tiff",
     height = 4,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p1)
graphics.off()

# b. AMI Readmissions----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 31))
t1 <- data.table(dschyear = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 91))
t2 <- data.table(dschyear = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 181))
t3 <- data.table(dschyear = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$days2post.chf.acute.dx1 < 366))
t4 <- data.table(dschyear = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Combine and plot----
merge
tt1 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)

# Remove 2015 and the sum
tt1 <- tt1[-c(16, 17)]
tt1

# Melt to long format
readm.hf.dx1 <- melt.data.table(data = tt1, 
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
     height = 4,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p1)
graphics.off()

CONTINUE HERE!!! 07/31/2017
EXCLUDE DEAD PEOPLE FROM DENOMINATOR ABOVE (PART b)?
NOTE: NUMBERS ARE WRONG AS RAES ARE TOO HIGH AND RISING. CHECK!!!

#**********************************************************
# PART III: Rates, no acute CHF prior or at 1st AMI admission----
tmp <- droplevels(subset(case,
                         !(hchf.acute | chf.acute.current)))
summary(tmp)

# 30 days----
t1 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.30))
t1 <- data.table(dschyear = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# Remove total and 2015
t1 <- t1[-c((nrow(t1) - 1):nrow(t1)),]
t1$dschyear <- as.numeric(t1$dschyear)

# 90 days----
t2 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.90))
t2 <- data.table(dschyear = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# Remove total and 2015
t2 <- t2[-c((nrow(t2) - 1):nrow(t2)),]
t2$dschyear <- as.numeric(t2$dschyear)

# 180 days----
t3 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.180))
t3 <- data.table(dschyear = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# Remove total and 2015
t3 <- t3[-c((nrow(t3) - 1):nrow(t3)),]
t3$dschyear <- as.numeric(t3$dschyear)

# 1 year----
t4 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.1y))
t4 <- data.table(dschyear = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Remove total and 2015
t4 <- t4[-c((nrow(t4) - 1):nrow(t4)),]
t4$dschyear <- as.numeric(t4$dschyear)

# Combine and plot----
tt2 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt2
plot(tt2)

# Melt to long format
tt2.l <- melt.data.table(data = tt2, 
                         id.vars = "dschyear",
                         measure.vars = c(2:5))

# Add a break point before 2008
tt2.l <- rbind.data.frame(tt2.l,
                          data.frame(dschyear = rep(2007.5, 4),
                                     variable = unique(tt2.l$variable),
                                     value = rep(NA, 4)))
tt2.l$variable <- factor(tt2.l$variable,
                         levels = rev(levels(tt2.l$variable)))

# Figure2: Plot HF rates over time----
colnames(tt2) <- c("1st AMI\nDischarge Year",
                   "30-Day Rate",
                   "90-Day Rate",
                   "180-Day Rate",
                   "1-Year Rate")
rownames(tt2) <- NULL
tt2
write.csv(tt2, 
          file = "tmp/hf_after_ami_rates_no_hchf.csv",
          row.names = FALSE)

p2 <- ggplot(tt2.l,
             aes(x = dschyear,
                 y = value,
                 group = variable)) +
  geom_line(size = 0.5) +
  geom_point(aes(x = dschyear,
                 y = value,
                 fill = variable),
             size = 2,
             shape = 21) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_continuous("1st AMI Discharge Year",
                     breaks = 2000:2014,
                     labels = 2000:2014) +
  scale_y_continuous("HF Admission Rate (%)",
                     limits = c(0, 4.5)) +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(tt2.l$variable)) +
  ggtitle("HF Admission After 1st AMI Discharge\nNo Prior Or Current HF") +
  guides(colour = guide_legend(title = "Follow-up")) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))


tiff(filename = "tmp/hf_after_ami_rates_no_hchf.tiff",
     height = 6,
     width = 13,
     units = 'in',
     res = 300,
     compression = "lzw+p")
grid.arrange(p2,
             tableGrob(tt2,
                       rows = NULL),
             nrow = 1)
graphics.off()

#**********************************************************
# PART IV: Models----
s1 <- list()

# Add variable: before/after 2008
case$before2008 <- TRUE
case$before2008[case$dschyear >= 2008] <- FALSE
# Compute number of years from reference year: 
# for 2000 to 2007, use year 2000;
# for 2008 to 2014, use year 2008
case$years <- case$dschyear - 2000
case$years[!case$before2008] <- case$dschyear[!case$before2008] - 2008

# Model1.30: Acute CHF, 30 days
m1.30 <- glm(post.chf.acute.dx1.30 ~ years*before2008 +
               decade + 
               SEX +
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
s1$d30 <- summary(m1.30)
s1$d30

# Model1.90: Acute CHF, 90 days
m1.90 <- glm(post.chf.acute.dx1.90 ~ years*before2008 +
               decade + 
               SEX +
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
s1$d90 <- summary(m1.90)
s1$d90

# Model1.180: Acute CHF, 180 days
m1.180 <- glm(post.chf.acute.dx1.180 ~ years*before2008 +
                decade + 
                SEX +
                hhyp + 
                hdiab + 
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = case)
s1$d180 <- summary(m1.180)
s1$d180

# Model1.1y: Acute CHF, 1 year
m1.1y <- glm(post.chf.acute.dx1.1y ~ years*before2008 +
               decade + 
               SEX +
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
s1$y1 <- summary(m1.1y)
s1$y1

# Combine
t.col.names <- c("Years From Reference(*)",
                 "2000 to 2007",
                 "Age (by Decade)",
                 "Sex (Male)",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism",
                 "Interaction: Years From Reference*(Between 2000 and 2007)")

tm0 <- lapply(s1, function(a) {
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

tm1 <- do.call("rbind", tm0)
tm1 <- data.table(cutoff = rep(c("30 Days",
                                 "90 Days",
                                 "180 Days",
                                 "1 Year"),
                               each = length(t.col.names)),
                  tm1)
tm1$names <- factor(tm1$names, levels = unique(tm1$names))
tm1$cutoff <- factor(as.character(tm1$cutoff),
                     levels = c("30 Days", "90 Days", "180 Days",  "1 Year"))
tm1
write.csv(tm1, 
          file = "tmp/hf_after_ami_or.csv",
          row.names = FALSE)

# Figure3: Plot OR----
tiff(filename = "tmp/hf_after_ami_or.tiff",
     height = 6,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tm1, 
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
  ggtitle("Odds Ratios of HF Admission After First AMI Discharge") +
  theme(axis.text.x = element_text(size = 12,
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend("Risk Factors Legend"))
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
          file = "tmp/hf_after_ami_or_combined.csv",
          row.names = FALSE)

#**********************************************************
# PART V: Models, no acute CHF prior or at 1st AMI admission----
tmp <- droplevels(subset(case,
                         !(hchf.acute | chf.acute.current)))
summary(tmp)
s1 <- list()

# Model1.30: Acute CHF, 30 days
m1.30 <- glm(post.chf.acute.dx1.30 ~ years*before2008 +
               decade + 
               SEX +
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = tmp)
s1$d30 <- summary(m1.30)
s1$d30

# Model1.90: Acute CHF, 90 days
m1.90 <- glm(post.chf.acute.dx1.90 ~ years*before2008 +
               decade + 
               SEX +
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = tmp)
s1$d90 <- summary(m1.90)
s1$d90

# Model1.180: Acute CHF, 180 days
m1.180 <- glm(post.chf.acute.dx1.180 ~ years*before2008 +
                decade + 
                SEX +
                hhyp + 
                hdiab + 
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = tmp)
s1$d180 <- summary(m1.180)
s1$d180

# Model1.1y: Acute CHF, 1 year
m1.1y <- glm(post.chf.acute.dx1.1y ~ years*before2008 +
               decade + 
               SEX +
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = tmp)
s1$y1 <- summary(m1.1y)
s1$y1

# Combine
t.col.names <- c("Years From Reference(*)",
                 "2000 to 2007",
                 "Age (by Decade)",
                 "Sex (Male)",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism",
                 "Interaction: Years From Reference*(Between 2000 and 2007)")

tm0 <- lapply(s1, function(a) {
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

tm3 <- do.call("rbind", tm0)
tm3 <- data.table(cutoff = rep(c("30 Days",
                                 "90 Days",
                                 "180 Days",
                                 "1 Year"),
                               each = length(t.col.names)),
                  tm3)
tm3$names <- factor(tm3$names, levels = unique(tm3$names))
tm3$cutoff <- factor(as.character(tm3$cutoff),
                     levels = c("30 Days", "90 Days", "180 Days",  "1 Year"))
tm3
write.csv(tm3, 
          file = "tmp/hf_after_ami_or_no_hchf.csv",
          row.names = FALSE)

# Plot OR----
# Figure3: Plot OR----
tiff(filename = "tmp/hf_after_ami_or_no_hchf.tiff",
     height = 6,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tm3, 
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
  scale_fill_manual(labels = paste(LETTERS[1:nlevels(tm3$names)],
                                   levels(tm3$names),
                                   sep = ": "),
                    values = rainbow(nlevels(tm3$names))) +
  scale_x_discrete("Risk Factors",
                   labels = LETTERS[1:nlevels(tm3$names)]) + 
  scale_y_continuous("Odds Ratios") + 
  ggtitle("Odds Ratios of HF Admission After First AMI Discharge") +
  theme(axis.text.x = element_text(size = 12,
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend("Risk Factors Legend"))
graphics.off()

# Table2: OR, 95% C.I., and p-values----
tm3$est <- paste(round(tm3$m1.cf, 3),
                 " (",
                 round(tm3$m1.lb, 3),
                 ", ",
                 round(tm3$m1.ub, 3),
                 ")\n",
                 tm3$pval,
                 sep = "")
tm4 <- dcast.data.table(data = tm3,
                        names ~ cutoff,
                        value.var = "est")
names(tm4)[1] <- "Risk Factor"
tm4
write.csv(tm4, 
          file = "tmp/hf_after_ami_or_no_hchf_combined.csv",
          row.names = FALSE)
#**********************************************************
# EVERYTHING BELOW THIS LINE WAS NOT USED FOR PUBLICATION!----
# PART VI: Extended Models----
# NOTE: remove History of Acute CHF
s2 <- list()

# Model1.30: Acute CHF, 30 days
m1.30 <- glm(post.chf.acute.dx1.30 ~ decade + 
               SEX +
               PRIME +
               RACE +
               HISPAN +
               dschyear + 
               hchf.acute*chf.acute.current +
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
s2$d30 <- summary(m1.30)
s2$d30

# Model1.90: Acute CHF, 90 days
m1.90 <- glm(post.chf.acute.dx1.90 ~ decade + 
               SEX +
               PRIME +
               RACE +
               HISPAN +
               dschyear + 
               hchf.acute*chf.acute.current +
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
s2$d90 <- summary(m1.90)
s2$d90

# Model1.180: Acute CHF, 180 days
m1.180 <- glm(post.chf.acute.dx1.180 ~ decade + 
                SEX +
                PRIME +
                RACE +
                HISPAN +
                dschyear + 
                hchf.acute*chf.acute.current +
                hchf.chron +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = case)
s2$d180 <- summary(m1.180)
s2$d180

# Model1.1y: Acute CHF, 1 year
m1.1y <- glm(post.chf.acute.dx1.1y ~ decade + 
               SEX +
               PRIME +
               RACE +
               HISPAN +
               dschyear + 
               hchf.acute*chf.acute.current +
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = case)
s2$y1 <- summary(m1.1y)
s2$y1

# Combine
t.col.names <- c("Age (by Decade)",
                 "Sex (Male)",
                 "Insurance (Commercial)",
                 "Insurance (Medicade/self-pay/other)",
                 "Race (Black)",
                 "Race (Other)",
                 "Hispanic (No)",
                 "Hispanic (Unknown)",
                 "1st AMI Discharge Year",
                 "History of Acute CHF",
                 "Acute CHF at 1st AMI Admission",
                 "History of Chronic CHF",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Liver Disease",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism",
                 "Interaction: Current or Past Achute CHF")

tm0 <- lapply(s2, function(a) {
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

tm2 <- do.call("rbind", tm0)
tm2 <- data.table(cutoff = rep(c("30 Days",
                                 "90 Days",
                                 "180 Days",
                                 "1 Year"),
                               each = length(t.col.names)),
                  tm2)
tm2$names <- factor(tm2$names, levels = unique(tm2$names))
tm2$cutoff <- factor(as.character(tm2$cutoff),
                     levels = c("30 Days", "90 Days", "180 Days",  "1 Year"))
tm2
write.csv(tm2, 
          file = "tmp/hf_after_ami_or_extended.csv",
          row.names = FALSE)

# Plot OR----
tiff(filename = "tmp/hf_after_ami_or_extended.tiff",
     height = 8,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tm2, 
       aes(x = names, 
           y = m1.cf),
       log10 = "y") +
  facet_wrap( ~ cutoff, 
              ncol = 2) + 
  geom_hline(yintercept = 1,
             colour = "grey",
             size = 1.1,
             linetype = 2) +
  geom_errorbar(aes(ymin = m1.lb,
                    ymax = m1.ub,
                    colour = names),
                size = 1.1,
                width = 0.2) +
  geom_point(aes(x = names, 
                 y = m1.cf),
             size = 2) +
  scale_colour_discrete(guide = FALSE) +
  scale_x_discrete("Risk Factors") + 
  scale_y_log10("Odds Ratios",
                breaks = 0:5) + 
  ggtitle("Risk of Acute Congestive Heart Failure After 1st Acute MI") +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 60, 
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12))
graphics.off()

#**********************************************************
# PART VII: Extended Models, No Acute CHF at 1st AMI admission or earlier----
# NOTE: remove History of Acute CHF
s3 <- list()
tmp <- droplevels(subset(case,
                         !(hchf.acute | chf.acute.current)))
summary(tmp)

# Model1.30: Acute CHF, 30 days
m1.30 <- glm(post.chf.acute.dx1.30 ~ decade + 
               SEX +
               PRIME +
               RACE +
               HISPAN +
               dschyear + 
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = tmp)
s3$d30 <- summary(m1.30)
s3$d30

# Model1.90: Acute CHF, 90 days
m1.90 <- glm(post.chf.acute.dx1.90 ~ decade + 
               SEX +
               PRIME +
               RACE +
               HISPAN +
               dschyear + 
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = tmp)
s3$d90 <- summary(m1.90)
s3$d90

# Model1.180: Acute CHF, 180 days
m1.180 <- glm(post.chf.acute.dx1.180 ~ decade + 
                SEX +
                PRIME +
                RACE +
                HISPAN +
                dschyear + 
                hchf.chron +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = tmp)
s3$d180 <- summary(m1.180)
s3$d180

# Model1.1y: Acute CHF, 1 year
m1.1y <- glm(post.chf.acute.dx1.1y ~ decade + 
               SEX +
               PRIME +
               RACE +
               HISPAN +
               dschyear + 
               hchf.chron +
               hhyp + 
               hdiab + 
               hcld +
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data = tmp)
s3$y1 <- summary(m1.1y)
s3$y1

# Combine
t.col.names <- c("Age (by Decade)",
                 "Sex (Male)",
                 "Insurance (Commercial)",
                 "Insurance (Medicade/self-pay/other)",
                 "Race (Black)",
                 "Race (Other)",
                 "Hispanic (No)",
                 "Hispanic (Unknown)",
                 "1st AMI Discharge Year",
                 "History of Chronic CHF",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Liver Disease",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism")

tm0 <- lapply(s3, function(a) {
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

tm3 <- do.call("rbind", tm0)
tm3 <- data.table(cutoff = rep(c("30 Days",
                                 "90 Days",
                                 "180 Days",
                                 "1 Year"),
                               each = length(t.col.names)),
                  tm3)
tm3$names <- factor(tm3$names, levels = unique(tm3$names))
tm3$cutoff <- factor(as.character(tm3$cutoff),
                     levels = c("30 Days", "90 Days", "180 Days",  "1 Year"))
tm3
write.csv(tm3, 
          file = "tmp/hf_after_ami_or_extended_no_hchf.csv",
          row.names = FALSE)

# Plot OR----
tiff(filename = "tmp/hf_after_ami_or_extended_no_hchf.tiff",
     height = 8,
     width = 10,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tm3, 
       aes(x = names, 
           y = m1.cf)) +
  facet_wrap( ~ cutoff, 
              ncol = 2) + 
  geom_hline(yintercept = 1,
             colour = "grey",
             size = 1.1,
             linetype = 2) +
  geom_errorbar(aes(ymin = m1.lb,
                    ymax = m1.ub,
                    colour = names),
                size = 1.1,
                width = 0.2) +
  geom_point(aes(x = names, 
                 y = m1.cf),
             size = 2) +
  scale_colour_discrete(guide = FALSE) +
  scale_x_discrete("Risk Factors") + 
  scale_y_log10("Odds Ratios",
                breaks = 0:5) + 
  ggtitle("Risk of Acute CHF After 1st AMI, No History or Current ACHF") +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 60, 
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12))
graphics.off()

#**********************************************************
# PART VIII: Tables----
# Female
t5 <- addmargins(table(case$SEX))
t5
100*t5/t5[3]

# Primary insurance
t5 <- addmargins(table(case$PRIME))
t5
100*t5/t5[4]

# Race
t5 <- addmargins(table(case$RACE))
t5
100*t5/t5[4]

# Hispanic
t5 <- addmargins(table(case$HISPAN))
t5
100*t5/t5[4]

# Average age
mean(case$AGE)
sd(case$AGE)