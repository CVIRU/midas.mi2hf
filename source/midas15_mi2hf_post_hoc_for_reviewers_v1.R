# |----------------------------------------------------------------------------------|
# | Project: Heart Failures after MIs in MIDAS                                       |
# | Script: POST HOC analysis per reviewers' requests                                |
# | Authors: Jen Wellings; Davit Sargsyan                                            |   
# | Created: 03/01/2018                                                              |
# |----------------------------------------------------------------------------------|
# Header----
# Save consol output to a log file
# sink(file = "tmp/log_midas15_mi2hf_post_hoc_for_reviewers_v1.txt")
date()

# Load packages
require(data.table)
require(ggplot2)

load("C:/git_local/data/midas.mi2hf/case_02282018.RData")
# Number of patients
nrow(case)

# Remove patients with prior or co-occurung HF (DS, 08/19/2017)----
case <- droplevels(subset(case,
                          !(hchf.acute | chf.acute.current)))
nrow(case)
summary(case)

# Subendocardial infarctions
kable(addmargins(table(sub.ami = case$sub.ami,
                       sub.ami.dx1 = case$sub.ami.dx1)))
  # |      | FALSE|  TRUE|    Sum|
  # |:-----|-----:|-----:|------:|
  # |FALSE | 46749|     0|  46749|
  # |TRUE  |   106| 62871|  62977|
  # |Sum   | 46855| 62871| 109726|

# Censor at 5 years----
case$hf5y <- FALSE
case$hf5y[case$days2post.chf.acute.dx1 < 5*365.25] <- TRUE

# Make agper 10 year
case$decade <- floor(case$AGE/10)

# Part I: all AMIs----
# Analysis, excluding AMI admission----
case$pci5y <- FALSE
case$pci5y[!is.na(case$days2post.pci) &
              case$days2post.pci > 0 &
              case$days2post.pci < 5*365.25] <- TRUE
case$pci5y[case$days2post.pci < case$days2post.chf.acute.dx1] <- FALSE
sum(case$pci)
# 9,336 if excluding AMI admission

case$cabg5y <- FALSE
case$cabg5y[!is.na(case$days2post.cabg) &
              case$days2post.cabg > 0 &
              case$days2post.cabg < 5*365.25] <- TRUE
case$cabg5y[case$days2post.cabg < case$days2post.chf.acute.dx1] <- FALSE
sum(case$cabg5y)
# 3,319 if excluding AMI admission

m2a.5y <- glm(hf5y ~ dschyear +
                decade + 
                SEX +
                HISPAN +
                PRIME +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid + 
                # hpci + 
                # hcabg +
                pci5y +
                cabg5y,
              family = binomial(logit),
              data = case[ADMDAT < "2011-01-01", ])
s1 <- summary(m2a.5y)
s1
data.frame(OR = exp(s1$coefficients[-1, 1]),
           `95% C.I.L.L.` = exp(s1$coefficients[-1, 1] - 1.96*s1$coefficients[-1, 2]),
           `95% C.I.U.L.` = exp(s1$coefficients[-1, 1] + 1.96*s1$coefficients[-1, 2]),
           `p-Value` = round(s1$coefficients[-1, 4], 4))

# dschyear                     0.9506667     0.9409546     0.9604792  0.0000
# decade                       1.3850679     1.3444194     1.4269454  0.0000
# SEXM                         0.8619816     0.8079098     0.9196723  0.0000
# HISPANNon-hispanic           0.9714719     0.8686719     1.0864375  0.6120
# HISPANUnknown                0.8508595     0.7382546     0.9806399  0.0258
# PRIMECOMMERCIAL              0.7533045     0.6926725     0.8192439  0.0000
# PRIMEmedicaid/self-pay/other 1.1430206     0.9996231     1.3069885  0.0506
# hhypTRUE                     1.6515848     1.5237449     1.7901503  0.0000
# hdiabTRUE                    2.0968924     1.9667916     2.2355993  0.0000
# hcldTRUE                     1.4359034     1.0429174     1.9769721  0.0266
# hckdTRUE                     1.8678073     1.6470117     2.1182025  0.0000
# hcopdTRUE                    1.4180614     1.3152772     1.5288778  0.0000
# hlipidTRUE                   0.8325909     0.7806576     0.8879790  0.0000
# pci5yTRUE                    0.6506860     0.5716252     0.7406817  0.0000
# cabg5yTRUE                   1.4311859     1.2247819     1.6723738  0.0000

# Analysis, including AMI admission----
case$pci5y <- FALSE
case$pci5y[!is.na(case$days2post.pci) &
             case$days2post.pci >= 0 &
             case$days2post.pci < 5*365.25] <- TRUE
case$pci5y[case$days2post.pci < case$days2post.chf.acute.dx1] <- FALSE
sum(case$pci)
# 22,792

case$cabg5y <- FALSE
case$cabg5y[!is.na(case$days2post.cabg) &
              case$days2post.cabg >= 0 &
              case$days2post.cabg < 5*365.25] <- TRUE
case$cabg5y[case$days2post.cabg < case$days2post.chf.acute.dx1] <- FALSE
sum(case$cabg5y)
# 8,538

m2a.5y <- glm(hf5y ~ dschyear +
                decade + 
                SEX +
                HISPAN +
                PRIME +
                hhyp + 
                hdiab + 
                hcld +
                hckd + 
                hcopd +
                hlipid + 
                # hpci + 
                # hcabg +
                pci5y +
                cabg5y,
              family = binomial(logit),
              data = case[ADMDAT < "2011-01-01", ])
s1 <- summary(m2a.5y)
s1
data.frame(OR = exp(s1$coefficients[-1, 1]),
           `95% C.I.L.L.` = exp(s1$coefficients[-1, 1] - 1.96*s1$coefficients[-1, 2]),
           `95% C.I.U.L.` = exp(s1$coefficients[-1, 1] + 1.96*s1$coefficients[-1, 2]),
           `p-Value` = round(s1$coefficients[-1, 4], 4))
#                              OR            95%UB         95%LB      p.Value
# dschyear                     0.9469668     0.9372717     0.9567621  0.0000
# decade                       1.3227396     1.2842835     1.3623473  0.0000
# SEXM                         0.8952819     0.8387219     0.9556561  0.0009
# HISPANNon-hispanic           0.9410718     0.8406924     1.0534366  0.2912
# HISPANUnknown                0.8189247     0.7097276     0.9449226  0.0062
# PRIMECOMMERCIAL              0.7618964     0.7000254     0.8292358  0.0000
# PRIMEmedicaid/self-pay/other 1.1132077     0.9724263     1.2743705  0.1200
# hhypTRUE                     1.6598633     1.5307915     1.7998181  0.0000
# hdiabTRUE                    2.1419599     2.0081497     2.2846864  0.0000
# hcldTRUE                     1.3368454     0.9687403     1.8448244  0.0773
# hckdTRUE                     1.8191688     1.6025281     2.0650965  0.0000
# hcopdTRUE                    1.3961141     1.2941594     1.5061009  0.0000
# hlipidTRUE                   0.8566895     0.8029219     0.9140576  0.0000
# pci5yTRUE                    0.2192664     0.1931111     0.2489642  0.0000
# cabg5yTRUE                   0.4379349     0.3769531     0.5087821  0.0000						

# Subendocardial (non-STEMI) vs. transmural (STEMI) AMI----
# a. Unadjusted----
m2a.5y <- glm(hf5y ~ sub.ami.dx1,
              family = binomial(logit),
              data = case[ADMDAT < "2011-01-01", ])
s1 <- summary(m2a.5y)
s1
data.frame(OR = exp(s1$coefficients[-1, 1]),
           `95% C.I.L.L.` = exp(s1$coefficients[-1, 1] - 1.96*s1$coefficients[-1, 2]),
           `95% C.I.U.L.` = exp(s1$coefficients[-1, 1] + 1.96*s1$coefficients[-1, 2]),
           `p-Value` = round(s1$coefficients[-1, 4], 4))
# OR X95..C.I.L.L. X95..C.I.U.L. p.Value
# 1 1.299885      1.222404      1.382277       0

# b. Adjusted----
m2a.5y <- glm(hf5y ~ sub.ami.dx1 +
                dschyear +
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
              data = case[ADMDAT < "2011-01-01", ])
s1 <- summary(m2a.5y)
s1
data.frame(OR = exp(s1$coefficients[-1, 1]),
           `95% C.I.L.L.` = exp(s1$coefficients[-1, 1] - 1.96*s1$coefficients[-1, 2]),
           `95% C.I.U.L.` = exp(s1$coefficients[-1, 1] + 1.96*s1$coefficients[-1, 2]),
           `p-Value` = round(s1$coefficients[-1, 4], 4))
# OR X95..C.I.L.L. X95..C.I.U.L. p.Value
# sub.ami.dx1TRUE              0.9620796     0.9019101     1.0262631  0.2407
# dschyear                     0.9506901     0.9409436     0.9605376  0.0000
# decade                       1.3948955     1.3539021     1.4371302  0.0000
# SEXM                         0.8578817     0.8040769     0.9152867  0.0000
# HISPANNon-hispanic           0.9762358     0.8729922     1.0916895  0.6732
# HISPANUnknown                0.8522724     0.7395327     0.9821991  0.0272
# PRIMECOMMERCIAL              0.7522849     0.6917200     0.8181527  0.0000
# PRIMEmedicaid/self-pay/other 1.1512459     1.0069218     1.3162562  0.0393
# hhypTRUE                     1.6579888     1.5294535     1.7973262  0.0000
# hdiabTRUE                    2.0976852     1.9676201     2.2363480  0.0000
# hcldTRUE                     1.4402187     1.0462457     1.9825457  0.0253
# hckdTRUE                     1.8700025     1.6488679     2.1207941  0.0000
# hcopdTRUE                    1.4189550     1.3160325     1.5299267  0.0000
# hlipidTRUE                   0.8256602     0.7742090     0.8805307  0.0000

# PCA----
m.pca <- prcomp(case[, hchf.chron:decade])
summary(m.pca)

# NEW (11/11/2017): Keep only the most important variables (Javier)----
# Select PC-s to pliot (PC1 & PC2)
choices <- 1:2

nobs.factor <- sqrt(nrow(m.pca$x) - 1)
d <- m.pca$sdev
u <- m.pca$x
v <- m.pca$rotation

# Scores
df.u <- data.table(u[, choices])
# Add grouping variable
df.u$grp <- factor(as.numeric(case$post.chf.acute.dx1) + 1)

# Directions
df.v <- as.data.frame(v[, choices])

# Annotate
df.v$abrvName <- rownames(df.v)

# Loads, PC1
pc12.load <- data.table(Diagnosis = colnames(case[, hchf.chron:decade]),
                        m.pca$rotation[, 1:2])
pc12.load <- melt.data.table(data = pc12.load,
                             id.vars = 1,
                             measure.vars = 2:3,
                             variable.name = "PC",
                             value.name = "Loadings")
pc12.load

p0 <- ggplot(data = pc12.load, 
             aes(x = Diagnosis,
                 y = Loadings)) +
  facet_wrap(~ PC,
             nrow = 2) +
  geom_bar(stat = "identity") +
  ggtitle("PC Loadings") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))
p0
tiff(filename = "tmp/pc.1.2_loadings.tiff",
     height = 5,
     width = 8,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p0)
graphics.off()

# Axis labels
u.axis.labs <- paste(colnames(df.v)[2:3], 
                     sprintf('(%0.1f%% explained var.)', 
                             100*m.pca$sdev[choices]^2/sum(m.pca$sdev^2)))

# Based on Figure p0, keep only a few variables with high loadings in PC1 and PC2----
var.keep.ndx <- which(df.v$abrvName %in% c("decade",
                                           "hdiab",
                                           "hhyp",
                                           "hlipid"))

p1 <- ggplot(data = df.v[var.keep.ndx,],
             aes(x = PC1,
                 y = PC2)) +
  coord_equal() +
  # geom_point(data = df.u,
  #            aes(fill = grp),
  #            shape = 21,
  #            size = 2,
  #            alpha = 0.5) +
  geom_segment(aes(x = 0,
                   y = 0,
                   xend = 10*PC1,
                   yend = 10*PC2),
               arrow = arrow(length = unit(1/2, 'picas')),
               color = "black",
               size = 0.5) +
  geom_text(aes(x = 10*PC1,
                y = 10*PC2,
                label = df.v$abrvName[var.keep.ndx]),
            size = 5,
            hjust = 0.5) +
  scale_x_continuous(u.axis.labs[1]) +
  scale_y_continuous(u.axis.labs[2]) +
  scale_fill_discrete(name = "",
                      labels = c("No AF (DX1-9)",
                                 "AF (DX1-9)")) +
  ggtitle("Biplot of AMI Patients") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 20))
p1

# sink()