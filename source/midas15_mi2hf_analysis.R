# Author:   Davit Sargsyan
# Created:  04/29/2016
# Last modified: 06/05/2017
# Project: HF after first MI, Jen Wellings
#**********************************************************
# PART I: Load data----
DATA_HOME <- "C:/Users/ds752/Documents/git_local/data/midas.mi2hf"
require(data.table)
require(ggplot2)

load(file.path(DATA_HOME, "case_02052017.RData"))

# Make age by decade
case$decade <- floor(case$AGE/10)
summary(case$decade)
gc()

# History of acute CHF vs acute CHF recorded at 1st AMI admission
t0 <- addmargins(table(`History of Acute CHF` = case$hchf.acute,
                       `Acute CHF at 1st AMI Admission` = case$chf.acute.current))
t0 <- round(100*t0/t0[3, 3], 1)
t0

# roc.plot <- function(mod.name, main) {
#   c.stat <- list()
#   for (i in 1:length(mod.name)) {
#     out <- eval(parse(text = paste("roc(predictions = predict(",
#                                    mod.name[i],
#                                    ", type = 'response'), labels = factor(",
#                                    mod.name[i],
#                                    "$y))",
#                                    sep = "")))
#     if (i == 1) {
#       plot(out,
#            col=i,
#            lty = i,
#            lwd=2,
#            main = main)
#     } else {
#       plot(out,
#            add = T,
#            col = i,
#            lty = i,
#            lwd = 2)
#     }
#     
#     c.stat[[i]] <- auc(out)
#   }
#   c.stat <- round(do.call("c", c.stat), 3)
#   c.stat
#   
#   legend("bottomright",
#          legend = paste(mod.name, ", c stat =", c.stat),
#          col = 1:length(c.stat),
#          lty = 1:length(c.stat))
# }

#**********************************************************
# PART II: Rates----
# 30 days----
t1 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.30))
t1 <- data.table(year = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# Remove total and 2015
t1 <- t1[-c((nrow(t1) - 1):nrow(t1)),]
t1$year <- as.numeric(t1$year)

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.90))
t2 <- data.table(year = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# Remove total and 2015
t2 <- t2[-c((nrow(t2) - 1):nrow(t2)),]
t2$year <- as.numeric(t2$year)

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.180))
t3 <- data.table(year = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# Remove total and 2015
t3 <- t3[-c((nrow(t3) - 1):nrow(t3)),]
t3$year <- as.numeric(t3$year)

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.1y))
t4 <- data.table(year = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Remove total and 2015
t4 <- t4[-c((nrow(t4) - 1):nrow(t4)),]
t4$year <- as.numeric(t4$year)

# Combine and plot----
tt1 <- data.table(year = t1$year,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt1
write.csv(tt1, 
          file = "tmp/hf_after_ami_rates.csv",
          row.names = FALSE)
plot(tt1)

tt1.l <- melt.data.table(data = tt1, 
                         id.vars = "year",
                         measure.vars = c(2:5))
tt1.l$variable <- factor(tt1.l$variable,
                         levels = rev(levels(tt1.l$variable)))

# Plot HF rates over time----
tiff(filename = "tmp/hf_after_ami_rates.tiff",
     height = 5,
     width = 6,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tt1.l,
       aes(x = year,
           y = value,
           colour = variable,
           group = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  # geom_vline(xintercept = 2000,
  #            linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Year",
                     breaks = unique(tt1.l$year),
                     labels = unique(tt1.l$year)) +
  scale_y_continuous("Rate (%)",
                     limits = c(0, 8.5)) +
  scale_colour_manual(label = c("1 Year",
                                "180 Days",
                                "90 Days",
                                "30 Days"),
                      values = unique(tt1.l$variable)) +
  ggtitle("HF Admission After AMI Discharge") +
  guides(colour = guide_legend(title = "Follow-up"))
graphics.off()

#**********************************************************
# PART III: Rates, no acute CHF prior or at 1st AMI admission----
tmp <- droplevels(subset(case,
                         !(hchf.acute | chf.acute.current)))
summary(tmp)

# 30 days----
t1 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.30))
t1 <- data.table(year = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# Remove total and 2015
t1 <- t1[-c((nrow(t1) - 1):nrow(t1)),]
t1$year <- as.numeric(t1$year)

# 90 days----
t2 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.90))
t2 <- data.table(year = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# Remove total and 2015
t2 <- t2[-c((nrow(t2) - 1):nrow(t2)),]
t2$year <- as.numeric(t2$year)

# 180 days----
t3 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.180))
t3 <- data.table(year = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# Remove total and 2015
t3 <- t3[-c((nrow(t3) - 1):nrow(t3)),]
t3$year <- as.numeric(t3$year)

# 1 year----
t4 <- addmargins(table(tmp$dschyear,
                       tmp$post.chf.acute.dx1.1y))
t4 <- data.table(year = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Remove total and 2015
t4 <- t4[-c((nrow(t4) - 1):nrow(t4)),]
t4$year <- as.numeric(t4$year)

# Combine and plot----
tt1 <- data.table(year = t1$year,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt1
write.csv(tt1, 
          file = "tmp/hf_after_ami_rates_no_hchf.csv",
          row.names = FALSE)
plot(tt1)

tt1.l <- melt.data.table(data = tt1, 
                         id.vars = "year",
                         measure.vars = c(2:5))
tt1.l$variable <- factor(tt1.l$variable,
                         levels = rev(levels(tt1.l$variable)))

# Plot HF rates over time----
tiff(filename = "tmp/hf_after_ami_rates_no_hchf.tiff",
     height = 5,
     width = 6,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tt1.l,
       aes(x = year,
           y = value,
           colour = variable,
           group = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  # geom_vline(xintercept = 2000,
  #            linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Year",
                     breaks = unique(tt1.l$year),
                     labels = unique(tt1.l$year)) +
  scale_y_continuous("Rate (%)",
                     limits = c(0, 8.5)) +
  scale_colour_manual(label = c("1 Year",
                                "180 Days",
                                "90 Days",
                                "30 Days"),
                      values = unique(tt1.l$variable)) +
  ggtitle("HF Admission After AMI Discharge\nNo Prior Or Current ACHF") +
  guides(colour = guide_legend(title = "Follow-up"))
graphics.off()

#**********************************************************
# PART IV: Models----
s1 <- list()

# Model1.30: Acute CHF, 30 days
m1.30 <- glm(post.chf.acute.dx1.30 ~ decade + 
               SEX +
               dschyear + 
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
m1.90 <- glm(post.chf.acute.dx1.90 ~ decade + 
               SEX +
               dschyear + 
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
m1.180 <- glm(post.chf.acute.dx1.180 ~ decade + 
                SEX +
                dschyear + 
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
m1.1y <- glm(post.chf.acute.dx1.1y ~ decade + 
               SEX +
               dschyear + 
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
t.col.names <- c("Age (by Decade)",
                 "Sex (Male)",
                 "Discharge Year",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism")

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
# Plot OR----
tiff(filename = "tmp/hf_after_ami_or.tiff",
     height = 8,
     width = 8,
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
                    ymax = m1.ub,
                    colour = names),
                size = 1.1,
                width = 0.2) +
  geom_point(aes(x = names, 
                 y = m1.cf),
             size = 2) +
  scale_colour_discrete(guide = FALSE) +
  scale_x_discrete("Risk Factors") + 
  scale_y_continuous("Odds Ratios") + 
  ggtitle("Risk of Acute Congestive Heart Failure After 1st Acute MI") +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12))
graphics.off()

#**********************************************************
# PART V: Models, no acute CHF prior or at 1st AMI admission----
tmp <- droplevels(subset(case,
                         !(hchf.acute | chf.acute.current)))
summary(tmp)
s1 <- list()

# Model1.30: Acute CHF, 30 days
m1.30 <- glm(post.chf.acute.dx1.30 ~ decade + 
               SEX +
               dschyear + 
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
m1.90 <- glm(post.chf.acute.dx1.90 ~ decade + 
               SEX +
               dschyear + 
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
m1.180 <- glm(post.chf.acute.dx1.180 ~ decade + 
                SEX +
                dschyear + 
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
m1.1y <- glm(post.chf.acute.dx1.1y ~ decade + 
               SEX +
               dschyear + 
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
t.col.names <- c("Age (by Decade)",
                 "Sex (Male)",
                 "Discharge Year",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism")

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
          file = "tmp/hf_after_ami_or_no_hchf.csv",
          row.names = FALSE)
# Plot OR----
tiff(filename = "tmp/hf_after_ami_or_no_hchf.tiff",
     height = 8,
     width = 8,
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
                    ymax = m1.ub,
                    colour = names),
                size = 1.1,
                width = 0.2) +
  geom_point(aes(x = names, 
                 y = m1.cf),
             size = 2) +
  scale_colour_discrete(guide = FALSE) +
  scale_x_discrete("Risk Factors") + 
  scale_y_continuous("Odds Ratios") + 
  ggtitle("Risk of Acute CHF After 1st Acute MI, No Prior Or Current ACHF") +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12))
graphics.off()

#**********************************************************
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
                 "Discharge Year",
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
                 "Discharge Year",
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