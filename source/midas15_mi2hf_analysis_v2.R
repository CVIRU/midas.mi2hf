# |------------------------------------------|
# | Author:   Davit Sargsyan                 |
# | Created:  04/29/2016                     |
# | Last modified: 06/05/2017                |
# | Project: HF after first MI, Jen Wellings |
# |------------------------------------------|
#**********************************************************
# Header----
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
load(file.path(DATA_HOME, "case_07082017.RData"))

# AMI discharges----
t1 <- table(case$dschyear,
            case$ami.dx1)
t1 <- data.table(`Patient Type` = rep("All", nrow(t1)),
                 Year = rownames(t1),
                 Counts = t1[, 1])
t1

# Records fom 2008 on, patients type 0 (in-patients) and 2 (emergency)
tmp <- droplevels(subset(case,
                         (dschyear > 2007) &
                           (PAT_TYPE %in% c(0, 2))))
summary(tmp)
t2 <- table(Year = tmp$dschyear,
            Counts = tmp$ami.dx1,
            `Patient Type` = tmp$PAT_TYPE)
t2 <- data.table(`Patient Type` = c(rep("Inpatient", 
                                        length(t2[, , 1])),
                                    rep("Emergency",
                                        length(t2[, , 2]))),
                 Year = as.numeric(c(names(t2[, , 1]),
                                     names(t2[, , 2]))),
                 Counts = c(t2[, , 1],
                            t2[, , 2]))
t2

# Merge all tables
tt <- rbindlist(list(t1, t2))
tt$`Patient Type` <- factor(tt$`Patient Type`,
                            levels = unique(tt$`Patient Type`))
tt

# Cast tt
t.w <- dcast.data.table(tt,
                        Year ~ `Patient Type`,
                        fun.aggregate = mean,
                        value.var = "Counts")
t.w

# Plot AMI discharges----
tiff(filename = "tmp/pat_type.tiff",
     height = 7,
     width = 8,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tt,
       aes(x = Year,
           y = Counts,
           group = `Patient Type`)) +
  geom_line(size = 1) +
  geom_point(aes(fill = `Patient Type`),
             size = 3,
             shape = 21) +
  ggtitle("Number of First AMI Admissions Between 2000 and 2015") +
  annotation_custom(grob = tableGrob(t.w,
                                     rows = NULL,
                                     theme = ttheme_default(base_size = 9)),
                    xmin = 2,
                    xmax = 6,
                    ymin = 1000,
                    ymax = 7500) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
graphics.off()

# Data processing----
# Make age by decade
case$decade <- floor(case$AGE/10)
summary(case$decade)
gc()

# History of acute CHF vs acute CHF recorded at 1st AMI admission
t0 <- addmargins(table(`History of Acute CHF` = case$hchf.acute,
                       `Acute CHF at 1st AMI Admission` = case$chf.acute.current))
t0 <- round(100*t0/t0[3, 3], 1)
t0
kable(t0)
# |       |achf @ 1st ami     |
# | h.achf| FALSE| TRUE|   Sum|
# |:------|-----:|----:|-----:|
# |FALSE  |  69.0| 18.3|  87.3|
# |TRUE   |   4.7|  8.0|  12.7|
# |Sum    |  73.7| 26.3| 100.0|

#**********************************************************
# PART II: Rates----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.30))
t1 <- data.table(dschyear = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# Remove total and 2015
t1 <- t1[-c((nrow(t1) - 1):nrow(t1)),]
t1$dschyear <- as.numeric(t1$dschyear)

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.90))
t2 <- data.table(dschyear = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# Remove total and 2015
t2 <- t2[-c((nrow(t2) - 1):nrow(t2)),]
t2$dschyear <- as.numeric(t2$dschyear)

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.180))
t3 <- data.table(dschyear = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# Remove total and 2015
t3 <- t3[-c((nrow(t3) - 1):nrow(t3)),]
t3$dschyear <- as.numeric(t3$dschyear)

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.1y))
t4 <- data.table(dschyear = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Remove total and 2015
t4 <- t4[-c((nrow(t4) - 1):nrow(t4)),]
t4$dschyear <- as.numeric(t4$dschyear)

# Combine and plot----
tt1 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt1
plot(tt1)

# Melt to long format
tt1.l <- melt.data.table(data = tt1, 
                         id.vars = "dschyear",
                         measure.vars = c(2:5))

#**********************************************************
# PART IIa: Rates, Inpatients Only----
tmp <- droplevels(subset(case,
                         (PAT_TYPE == 0) &
                           (dschyear > 2007)))
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

#**********************************************************
# PART IIb: Rates, Emergency Only----
tmp <- droplevels(subset(case,
                         (PAT_TYPE == 2) &
                           (dschyear > 2007)))
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
tt3 <- data.table(dschyear = t1$dschyear,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt3
plot(tt2)

# Melt to long format
tt3.l <- melt.data.table(data = tt3, 
                         id.vars = "dschyear",
                         measure.vars = c(2:5))

#**********************************************************
# Part III: plot HF rates----
tt4 <- data.table(rbindlist(list(tt1.l,
                                 tt2.l,
                                 tt3.l)),
                  pat_type = c(rep("All", 
                                   nrow(tt1.l)),
                               rep("Inpatient",
                                   nrow(tt2.l)),
                               rep("Emergency",
                                   nrow(tt3.l))))
tt4$variable <- factor(tt4$variable,
                         levels = rev(levels(tt4$variable)))
tt4

p1 <- ggplot(data = tt4) +
  scale_x_continuous("1st AMI Discharge Year",
                     breaks = unique(tt4$dschyear),
                     labels = unique(tt4$dschyear)) +
  scale_y_continuous("Rate (%)") +
  scale_fill_manual(label = c("1 Year",
                              "180 Days",
                              "90 Days",
                              "30 Days"),
                    values = unique(tt4$variable)) +
  ggtitle("HF Admission After AMI Discharge") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Follow-up")) +
  geom_line(data = droplevels(subset(tt4,
                                     tt4$pat_type == "All")),
            aes(x = dschyear,
                y = value,
                group = variable),
            size = 0.5) +
  geom_point(data = droplevels(subset(tt4,
                                      tt4$pat_type == "All")),
             aes(x = dschyear,
                 y = value,
                 group = variable,
                 fill = variable),
             size = 2,
             shape = 21) +
  geom_line(data = droplevels(subset(tt4,
                                     tt4$pat_type == "Inpatient")),
            aes(x = dschyear,
                y = value,
                group = variable),
            size = 0.5,
            linetype = "dashed") +
  geom_point(data = droplevels(subset(tt4,
                                      tt4$pat_type == "Inpatient")),
             aes(x = dschyear,
                 y = value,
                 group = variable,
                 fill = variable),
             size = 2,
             shape = 22) +
  geom_line(data = droplevels(subset(tt4,
                                     tt4$pat_type == "Emergency")),
            aes(x = dschyear,
                y = value,
                group = variable),
            size = 0.5,
            linetype = "dotted") +
  geom_point(data = droplevels(subset(tt4,
                                      tt4$pat_type == "Emergency")),
             aes(x = dschyear,
                 y = value,
                 group = variable,
                 fill = variable),
             size = 2,
             shape = 23) +
  geom_hline(yintercept = 0,
             linetype = "dashed")

tiff(filename = "tmp/hf_after_ami_rates.tiff",
     height = 4,
     width = 12,
     units = 'in',
     res = 300,
     compression = "lzw+p")
grid.arrange(p1,
             tableGrob(tt1,
                       rows = NULL,
                       theme = ttheme_default(base_size = 8)),
             tableGrob(tt2,
                       rows = NULL,
                       theme = ttheme_default(base_size = 8)),
             tableGrob(tt3,
                       rows = NULL,
                       theme = ttheme_default(base_size = 8)),
             nrow = 2,
             layout_matrix = rbind(c(1, 1, 2, 3),
                                   c(1, 1, 2, 4)))
graphics.off()
# |----------------------------------------------------------------|
# | NOTE: 'layout_matrix' argument takes a matrix with values      |    
# |       corresponding to the object number. e. g in my example   |
# |       above, it's a 1X4 matrix, with first 2 columns occupied  |
# |       by Object1 = p1, next one by Object2 = tt1, and the last |
# |       column is reserved for Object3 = tt2.                    |
# |----------------------------------------------------------------|

CONTINUE HERE!!! (07/08/2017)

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