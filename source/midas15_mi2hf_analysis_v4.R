# |----------------------------------------------------------------------------------|
# | Project: Heart Failure Following Myocardial Infarction in MIDAS                  |
# | Study ID: Pro20150002703                                                         |
# | Script: Data analysis                                                            |
# | Principal Investigator: John B. Kostis                                           |
# | Coordinator: Jen Wellings                                                        |
# | Statistician: Davit Sargsyan                                                     |   
# | Created:  04/21/2017                                                             |
# | Modified: 07/08/2017, kept inpatients only                                       |
# |           07/29/2017, added demographics table; compared with census data        |
# |           08/19/2017, Study only the patients with no prior or co-occuring HF    |
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

# Number of patients
nrow(case)

# Remove patients with prior or co-occurung HF (DS, 08/19/2017)----
case <- droplevels(subset(case,
                         !(hchf.acute | chf.acute.current)))
nrow(case)

# Make agper 10 year
case$decade <- floor(case$AGE/10)

# AMI discharges----
t1 <- table(case$dschyear)
t1 <- data.table(Year = as.numeric(names(t1)),
                 Counts = c(t1))
t1

# Plot AMI discharges----
p0 <- ggplot(t1,
             aes(x = Year,
                 y = Counts)) +
  geom_line(size = 1) +
  geom_point(size = 3,
             shape = 21,
             fill = "red") +
  scale_x_continuous("NUmber of Patients",
                     breaks = 2000:2015,
                     limits = c(2000, 2015)) +
  scale_y_continuous(breaks = seq(0, 8000, 1000),
                     limits = c(0, 8000)) +
  ggtitle(paste("Number of First AMI Discharges \n",
                "Total of",
                nrow(case),
                "Patients \n No Prior or Co-Occuring HF")) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p0

tiff(filename = "tmp/ami_per_year.tiff",
     height = 5,
     width = 5,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p0)
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
kable(table1)
#  [1] "|                                         |Summary     |"
#  [2] "|:----------------------------------------|:-----------|"
#  [3] "|Mean Age at First AMI Admission +/- S.D. |63.3+/-14.4 |"
#  [4] "|Female(%)                                |34.9        |"
#  [5] "|Race(%)                                  |            |"
#  [6] "|White                                    |49.3        |"
#  [7] "|Black                                    |42.5        |"
#  [8] "|Other                                    |8.2         |"
#  [9] "|Ethnicity(%)                             |            |"
# [10] "|Hispanic                                 |8.9         |"
# [11] "|Non-hispanic                             |80.2        |"
# [12] "|Other                                    |10.9        |"
# [13] "|Insurance(%)                             |            |"
# [14] "|Commercial                               |53.8        |"
# [15] "|Medicare                                 |37.3        |"
# [16] "|Medicade/Self-Pay/Other                  |8.9         |"
  
# PART II: Rates----
# Publication/Table 1a Appendix: readmissions rates due to HF----
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

# Combine----
t1a.apx <- data.table(dschyear = t1$dschyear,
                      rate.30 = t1$rate,
                      rate.90 = t2$rate,
                      rate.180 = t3$rate,
                      rate.1y = t4$rate)

# Table 1b Appendix: all-cause death rates----
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

# Combine----
t1b.apx <- data.table(dschyear = t1$dschyear,
                      rate.30 = t1$rate,
                      rate.90 = t2$rate,
                      rate.180 = t3$rate,
                      rate.1y = t4$rate)

# Table 1c Appendix: all-cause death or readmission due to HF rates----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       (case$days2post.chf.acute.dx1 < 31 &
                          !is.na(case$days2post.chf.acute.dx1)) |
                       (case$days2death < 31 &
                           !is.na(case$days2death))))
t1 <- data.table(dschyear = rownames(t1),
                 death.or.hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*death.or.hf30/N, 2)]

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       (case$days2post.chf.acute.dx1 < 91 &
                          !is.na(case$days2post.chf.acute.dx1)) |
                         (case$days2death < 91 &
                            !is.na(case$days2death))))
t2 <- data.table(dschyear = rownames(t2),
                 death.or.hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*death.or.hf90/N, 2)]

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       (case$days2post.chf.acute.dx1 < 181 &
                          !is.na(case$days2post.chf.acute.dx1)) |
                         (case$days2death < 181 &
                            !is.na(case$days2death))))
t3 <- data.table(dschyear = rownames(t3),
                 death.or.hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*death.or.hf180/N, 2)]

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       (case$days2post.chf.acute.dx1 < 366 &
                          !is.na(case$days2post.chf.acute.dx1)) |
                         (case$days2death < 366 &
                            !is.na(case$days2death))))
t4 <- data.table(dschyear = rownames(t4),
                 death.or.hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*death.or.hf1y/N, 2)]

# Combine----
t1c.apx <- data.table(dschyear = t1$dschyear,
                      rate.30 = t1$rate,
                      rate.90 = t2$rate,
                      rate.180 = t3$rate,
                      rate.1y = t4$rate)

# Print Table 1a,b&c Appendix----
# Remove sums
t1a.apx <- t1a.apx[-nrow(t1a.apx)]
t1b.apx <- t1b.apx[-nrow(t1b.apx)]
t1c.apx <- t1c.apx[-nrow(t1c.apx)]

# Save tables as a list
t1.apx <- list(t1a.apx,
               t1b.apx,
               t1c.apx)
names(t1.apx) <- c("Readmission for HF",
                   "All-Cause Death",
                   "Readmission for HF or Death")
for(i in 1:length(t1.apx)) {
  print(t1.apx[i])
}

# Plot rates----
out <- list()
for (i in 1:length(t1.apx)) {
  out[[i]] <- melt.data.table(data = t1.apx[[i]], 
                              id.vars = "dschyear",
                              measure.vars = c(2:5),
                              variable.name = "followup",
                              value.name = "rate")
  out[[i]]$endpoint <- names(t1.apx)[[i]]
}

out <- do.call("rbind", out)

# Re-level follow-up
out$followup <- factor(out$followup,
                       levels = rev(levels(out$followup)))

# Endpoint as a factor
out$endpoint <- factor(out$endpoint,
                       levels = names(t1.apx))

# Figure 1: all rates----
p1 <- ggplot(data = out,
             aes(x = dschyear,
                 y = rate,
                 group = followup,
                 fill = followup)) +
  facet_wrap(~ endpoint,
             nrow = 1,
             scales = "free_y") +
  geom_line(size = 0.5) +
  geom_point(size = 2,
             shape = 21) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_x_discrete("1st AMI Discharge Year",
                   breaks = unique(out$dschyear),
                   labels = unique(out$dschyear)) +
  scale_y_continuous("Rate (%)") +
  ggtitle("Figure1: Rates of Readmission and/or All-Cause Death") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(out$followup)) +
  guides(fill = guide_legend(title = "Follow-up")) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))

tiff(filename = "tmp/post_ami_rates.tiff",
     height = 5,
     width = 10,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p1)
graphics.off()

# PART III: Models----
# NOTE (DS, 08/19/2017) : there are 267 cases of chronic HF; 
#                         no significant effect; disregarded but kept
# Publication/Table 2a: readmissions rates due to HF----
# a. readmission for HF----
case$hf30 <- case$hf90 <- case$hf180 <- case$hf1y <- FALSE
case$hf30[case$days2post.chf.acute.dx1 < 31] <- TRUE
case$hf90[case$days2post.chf.acute.dx1 < 91] <- TRUE
case$hf180[case$days2post.chf.acute.dx1 < 181] <- TRUE
case$hf1y[case$days2post.chf.acute.dx1 < 366] <- TRUE

m2a.30 <- glm(hf30 ~ dschyear +
                decade + 
                SEX +
                HISPAN +
                PRIME +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = case)
summary(m2a.30)

m2a.90 <- glm(hf90 ~ dschyear +
                decade + 
                SEX +
                HISPAN +
                PRIME +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = case)
summary(m2a.90)

m2a.180 <- glm(hf180 ~ dschyear +
                 decade + 
                 SEX +
                 HISPAN +
                 PRIME +
                 hhyp + 
                 hdiab + 
                 hcld +
                 hckd + 
                 hcopd +
                 hlipid,
               family = binomial(logit),
               data = case)
summary(m2a.180)

m2a.1y <- glm(hf1y ~ dschyear +
                decade + 
                SEX +
                HISPAN +
                PRIME +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data = case)
summary(m2a.1y)

# Combine
t2a.names <- c("Discharge year",
               "Age (per 10 Years)",
               "Sex (Male)",
               "Non-Hispanic vs. Hispanic",
               "Unknown vs. Hispanic",
               "Commercial Insurance vs. Medicare",
               "Medicade/Self-Pay/Other vs. Medicare",
               "History of Hypertension",
               "History of Diabetes",
               "History of Chronic Liver Disease",
               "History of Chronic Kidney Disease",
               "History of COPD",
               "History of Disorder of Lipoid Metabolism")
s2a <- list(summary(m2a.30),
            summary(m2a.90),
            summary(m2a.180),
            summary(m2a.1y))

t2a <- lapply(s2a, function(a) {
  out <- data.table(names = rownames(a$coefficients)[-1],
                    coef = exp(a$coefficients[-1, 1]),
                    lb = exp(a$coefficients[-1, 1] - 1.96*a$coefficients[-1, 2]),
                    ub = exp(a$coefficients[-1, 1] + 1.96*a$coefficients[-1, 2]),
                    pval = ifelse(a$coefficients[-1, 4] >= 0.001,
                                  round(a$coefficients[-1, 4], 3),
                                  "<0.001"),
                    sign = ifelse(a$coefficients[-1, 4] <= 0.05, 
                                  ifelse(a$coefficients[-1, 4] <= 0.01,
                                         "**",
                                         "*"), ""))
  rownames(out) <- NULL
  out$names <- t2a.names
  return(out)
})

t2a <- do.call("rbind", t2a)

t2a <- data.table(followup = rep(c("30 Days",
                                   "90 Days",
                                   "180 Days",
                                   "1 Year"),
                                 each = length(t2a.names)),
                  t2a)


t2a$names <- factor(t2a$names, 
                    levels = unique(t2a$names))
t2a$followup <- factor(as.character(t2a$followup),
                       levels = c("30 Days", 
                                  "90 Days", 
                                  "180 Days", 
                                  "1 Year"))
t2a
write.csv(t2a, 
          file = "tmp/hf_after_ami_or.csv",
          row.names = FALSE)

# Plot OR----
p2a <- ggplot(t2a, 
             aes(x = names, 
                 y = coef)) +
  facet_wrap( ~ followup, 
              ncol = 2) + 
  geom_hline(yintercept = 1,
             colour = "grey",
             size = 1.1,
             linetype = 2) +
  geom_errorbar(aes(ymin = lb,
                    ymax = ub),
                size = 0.4,
                width = 0.2) +
  geom_point(aes(x = names, 
                 y = coef,
                 fill = names),
             size = 2,
             shape = 21) +
  scale_fill_manual(labels = paste(LETTERS[1:nlevels(t2a$names)],
                                   levels(t2a$names),
                                   sep = ": "),
                    values = rainbow(nlevels(t2a$names))) +
  scale_x_discrete("Risk Factors",
                   labels = LETTERS[1:nlevels(t2a$names)]) + 
  scale_y_continuous("Odds Ratios") + 
  ggtitle("Odds Ratios of All-Cause Death After First AMI Discharge") +
  theme(axis.text.x = element_text(size = 12,
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend("Risk Factors Legend"))
p2a

tiff(filename = "tmp/hf_after_ami_or.tiff",
     height = 8,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p2a)
graphics.off()


CONTINUE HERE! 08/19/2017, DS

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
