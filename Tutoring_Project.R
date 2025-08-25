setwd("~/Desktop/Capstone")

#install.packages('dplyr')
#install.packages('psych')
#install.packages('DescTools')
#install.packages('tidyverse')
library(dplyr)
library(psych)
library(DescTools)
library(car)
library(ROCR)
library(caret)
library(tidyverse)
library(broom)

dat <- read.csv("MATTutoringData.csv", header=TRUE)

dat <- dat[dat$Subject == 'MAT',]

dat[is.na(dat)] <- 0

dat$retakes <- dat$Number.of.times.received.a.grade 

dat$earned.1st <- dat$Earned.Grade.on.1st.attempt.at.Course

dat$earned.1st[dat$earned.1st == 'NO'] <- 0

dat$earned.1st[dat$earned.1st == 'YES'] <- 1


merged_data <- dat %>%
  group_by(Anonymous.ID, CRN, Course.Letter.Grade,
           Course.Numeric.Grade, Subject, Course, 
           Ethnicity.Race, X1st.Gen, EOP, ED, URM, 
           Gender,Accumulated.Hours.F23, ET.Section,
           Accumulated.Hours.S23, Credit.Load.S23,
           Credit.Load.F23, CGPA.S23, CGPA.F23, retakes, 
           earned.1st) %>%
  summarise(hours = sum(Duration..Hours.))

summary(as.factor(merged_data$Course.Letter.Grade))

merged_data$Course.Numeric.Grade[merged_data$Course.Numeric.Grade >= 1.0] <- 1

merged_data$Course.Numeric.Grade[merged_data$Course.Numeric.Grade < 1.0] <- 0

#Q: Are we including writing appointments which aren't tied to classes,
#   and therefore have no class GPA attached?

#A: Take out writing center visits
#   We are only interested in MAT data, for now

freq.id <- table(as.factor(merged_data$Anonymous.ID))
summary(as.factor(freq.id))
summary(as.factor(freq.id))/sum(summary(as.factor(freq.id)))

hours.sum <- Desc(merged_data$hours, plotit = TRUE)
hours.sum

freq.crn <- table(as.factor(merged_data$CRN))
summary(as.factor(freq.crn))
summary(as.factor(freq.crn))/sum(summary(as.factor(freq.crn)))

#Actual freq?? A: Break it down by course, as well as
# if the selected course has an ET
freq.crn <- table(as.factor(merged_data$CRN))
as.factor(freq.crn)
freq.crn/sum(freq.crn)

freq.courseLG <- table(as.factor(merged_data$Course.Letter.Grade))
freq.courseLG
freq.courseLG/sum(freq.courseLG)

freq.success <- table(as.factor(merged_data$Course.Numeric.Grade))
freq.success
freq.success/sum(freq.success)

freq.eth.race <- table(as.factor(merged_data$Ethnicity.Race))
freq.eth.race
freq.eth.race/sum(freq.eth.race)

freq.1stgen <- table(as.factor(merged_data$X1st.Gen))
freq.1stgen
freq.1stgen/sum(freq.1stgen)

freq.EOP <- table(as.factor(merged_data$EOP))
freq.EOP
freq.EOP/sum(freq.EOP)

freq.ED <- table(as.factor(merged_data$ED))
freq.ED
freq.ED/sum(freq.ED)

freq.URM <- table(as.factor(merged_data$URM))
freq.URM
freq.URM/sum(freq.URM)

freq.gender <- table(as.factor(merged_data$Gender))
freq.gender
freq.gender/sum(freq.gender)

freq.ETSec <- table(as.factor(merged_data$ET.Section))
freq.ETSec
freq.ETSec/sum(freq.ETSec)

freq.retakes <- table(as.factor(merged_data$retakes))
freq.retakes
freq.retakes/sum(freq.retakes)

freq.earned <- table(as.factor(merged_data$earned.1st))
freq.earned
freq.earned/sum(freq.earned)

AccHrS.sum <- Desc(merged_data$Accumulated.Hours.S23, plotit = FALSE)
AccHrS.sum

AccHrF.sum <- Desc(merged_data$Accumulated.Hours.F23, plotit = FALSE)
AccHrF.sum

CrLoadS.sum <- Desc(merged_data$Credit.Load.S23, plotit = FALSE)
CrLoadS.sum

CrLoadF.sum <- Desc(merged_data$Credit.Load.F23, plotit = FALSE)
CrLoadF.sum

CGPAS.sum <- Desc(merged_data$CGPA.S23, plotit = TRUE)
CGPAS.sum

CGPAF.sum <- Desc(merged_data$CGPA.F23, plotit = FALSE)
CGPAF.sum

#Desc statistics for duration, final grade, prior grade, subsetted by class
# and ET presence. Graphical rep of each.

summary(as.factor(merged_data$Course))

data_et <- merged_data[merged_data$ET.Section == 1,]

summary(as.factor(data_et$Course))

summary_et <- data_et %>% group_by(Course) %>%
  summarise(observations = length(hours), mean(hours), sd(hours), mean(CGPA.F23), sd(CGPA.F23),
            mean(CGPA.S23), sd(CGPA.S23))

course <- as.factor(data_et$Course)

duration <- data_et$hours
plot(duration, course)

final_spring <- data_et$CGPA.S23
plot(final_spring, course)

final_fall <- data_et$CGPA.F23
plot(final_fall, course)

####

data_n_et <- merged_data[merged_data$ET.Section == 0,] 

summary(as.factor(data_n_et$Course))

summary_n_et <- data_n_et %>% group_by(Course) %>%
  summarise(observations = length(hours), mean(hours), sd(hours), mean(CGPA.F23), sd(CGPA.F23),
            mean(CGPA.S23), sd(CGPA.S23))

course <- as.factor(data_n_et$Course)

duration <- data_n_et$hours
plot(duration, course)

final_spring <- data_n_et$CGPA.S23
plot(final_spring, course)

final_fall <- data_n_et$CGPA.F23
plot(final_fall, course)

#Compare ET presence to both grade and cumm. duration, separating by class,
# in addition to all MAT courses

#All MAT courses:

merged_data <- merged_data[merged_data$Course != 318 
                           & merged_data$Course != 330
                           & merged_data$Course != 258,]

et_status <- as.factor(merged_data$ET.Section)

#numeric#
course_gpa <- as.factor(merged_data$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade (All ET Courses)', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- merged_data$hours
plot(y=duration, x=et_status)

log.duration <- log(merged_data$hours)
plot(y=log.duration, x=et_status)

#By classes with ET presence:

summary(as.factor(data_et$Course))
#101
data.101 <- merged_data[merged_data$Course == 101,]

et_status <- as.factor(data.101$ET.Section)

course_gpa <- as.factor(data.101$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 101', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.101$hours
plot(y=duration, x=et_status)

log.duration <- log(data.101$hours)
plot(y=log.duration, x=et_status)

#102
data.102 <- merged_data[merged_data$Course == 102,]

et_status <- as.factor(data.102$ET.Section)

course_gpa <- as.factor(data.102$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 102', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.102$hours
plot(y=duration, x=et_status)

log.duration <- log(data.102$hours)
plot(y=log.duration, x=et_status)

#104
data.104 <- merged_data[merged_data$Course == 104,]

et_status <- as.factor(data.104$ET.Section)

course_gpa <- as.factor(data.104$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 104', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.104$hours
plot(y=duration, x=et_status)

log.duration <- log(data.104$hours)
plot(y=log.duration, x=et_status)

#106
data.106 <- merged_data[merged_data$Course == 106,]

et_status <- as.factor(data.106$ET.Section)

course_gpa <- as.factor(data.106$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 106', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.106$hours
plot(y=duration, x=et_status)

log.duration <- log(data.106$hours)
plot(y=log.duration, x=et_status)

#120
data.120 <- merged_data[merged_data$Course == 120,]

et_status <- as.factor(data.120$ET.Section)

course_gpa <- as.factor(data.120$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 120', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.120$hours
plot(y=duration, x=et_status)

log.duration <- log(data.120$hours)
plot(y=log.duration, x=et_status)

#158
data.158 <- merged_data[merged_data$Course == 158,]

et_status <- as.factor(data.158$ET.Section)

course_gpa <- as.factor(data.158$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 158', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.158$hours
plot(y=duration, x=et_status)

log.duration <- log(data.158$hours)
plot(y=log.duration, x=et_status)

#208
data.208 <- merged_data[merged_data$Course == 208,]

et_status <- as.factor(data.208$ET.Section)

course_gpa <- as.factor(data.208$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 208', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.208$hours
plot(y=duration, x=et_status)

log.duration <- log(data.208$hours)
plot(y=log.duration, x=et_status)

#220
data.220 <- merged_data[merged_data$Course == 220,]

et_status <- as.factor(data.220$ET.Section)

course_gpa <- as.factor(data.220$Course.Numeric.Grade)

plot(y=course_gpa, x=et_status, main = 'Final Grade 220', ylab = 'Pass/Fail', xlab = 'ET Status')

duration <- data.220$hours
plot(y=duration, x=et_status, main = 'Duration (Hours)', ylab = 'Duration', xlab = 'ET Status')

log.duration <- log(data.220$hours)
plot(y=log.duration, x=et_status, main = 'Log Duration (Hours)', ylab = 'Duration', xlab = 'ET Status')

summary(as.factor(data_et$Course))

summary(as.factor(merged_data$Course))

grade <- data.158$Course.Numeric.Grade

retakes <- data.158$retakes

et <- data.158$ET.Section

prior_gpa <- data.158$CGPA.S23

duration_ <- data.158$hours

mod <- glm(grade~et+duration_+retakes, data = data.158, family = binomial (link = "logit"))

(result <- summary(mod))

(exp(coef(mod))-1)*100

vif(mod)

png(filename = '158diagnostics.png')
par(mfrow=c(2,2))
plot(mod)
dev.off()

D0 <- result$null.deviance

D1 <- result$deviance

df.null <- result$df.null

df.full <- result$df.residual

1-pchisq(D0-D1,df=(df.null-df.full))

pred <- predict(mod, data.158, type='response')

bacc <- 0

for(k in 0:100){
  pred.class <- as.numeric(pred >= k/100)
  
  tp <- sum(pred.class == 1 & pred.class == data.158$Course.Numeric.Grade)
  
  fp <- sum(pred.class == 1 & pred.class != data.158$Course.Numeric.Grade)
  
  tn <- sum(pred.class == 0 & pred.class == data.158$Course.Numeric.Grade)
  
  fn <- sum(pred.class == 0 & pred.class != data.158$Course.Numeric.Grade)
  
  sens <- tp/(tp+fn)
  
  spec <- tn/(tn+fp)
  
  bacc[k] <- (sens+spec)/2
}

bacc

which(bacc==max(bacc))

pred.class <- as.numeric(pred >= which(bacc==max(bacc))/100)

confusionMatrix(factor(pred.class), factor(data.158$Course.Numeric.Grade), positive = '1')

roc.pred <- prediction(pred, data.158$Course.Numeric.Grade)

roc.perf <- performance(roc.pred, 'tpr', 'fpr')

plot(roc.perf, main='158 Model')
abline(0, 1, col='RED')

performance(roc.pred, 'auc')@y.values[[1]]

pred.test <- predict(mod, data.158, type = 'response')

pred.class.test <- as.numeric(pred.test >= which(bacc==max(bacc))/100)

accuracy.1 <- confusionMatrix(factor(pred.class.test), factor(data.158$Course.Numeric.Grade))$overall[1]

###################

grade <- data.106$Course.Numeric.Grade

retakes <- data.106$retakes

et <- data.106$ET.Section

prior_gpa <- data.106$CGPA.S23

duration_ <- data.106$hours

mod <- glm(grade~et+duration_+retakes, data = data.106, family = binomial (link = "logit"))

(result <- summary(mod))

(exp(coef(mod))-1)*100

vif(mod)

png(filename = '106diagnostics.png')
par(mfrow=c(2,2))
plot(mod)
dev.off()

D0 <- result$null.deviance

D1 <- result$deviance

df.null <- result$df.null

df.full <- result$df.residual

1-pchisq(D0-D1,df=(df.null-df.full))

pred <- predict(mod, data.106, type='response')

bacc <- 0

for(k in 0:100){
  pred.class <- as.numeric(pred >= k/100)
  
  tp <- sum(pred.class == 1 & pred.class == data.106$Course.Numeric.Grade)
  
  fp <- sum(pred.class == 1 & pred.class != data.106$Course.Numeric.Grade)
  
  tn <- sum(pred.class == 0 & pred.class == data.106$Course.Numeric.Grade)
  
  fn <- sum(pred.class == 0 & pred.class != data.106$Course.Numeric.Grade)
  
  sens <- tp/(tp+fn)
  
  spec <- tn/(tn+fp)
  
  bacc[k] <- (sens+spec)/2
}

bacc

which(bacc==max(bacc))

pred.class <- as.numeric(pred >= which(bacc==max(bacc))/100)

confusionMatrix(factor(pred.class), factor(data.106$Course.Numeric.Grade), positive = '1')

roc.pred <- prediction(pred, data.106$Course.Numeric.Grade)

roc.perf <- performance(roc.pred, 'tpr', 'fpr')

plot(roc.perf, main='106 Model')
abline(0, 1, col='RED')

performance(roc.pred, 'auc')@y.values[[1]]

pred.test <- predict(mod, data.106, type = 'response')

pred.class.test <- as.numeric(pred.test >= which(bacc==max(bacc))/100)

accuracy.2 <- confusionMatrix(factor(pred.class.test), factor(data.106$Course.Numeric.Grade))$overall[1]

############

grade <- data.220$Course.Numeric.Grade

retakes <- data.220$retakes

et <- data.220$ET.Section

prior_gpa <- data.220$CGPA.S23

duration_ <- data.220$hours

mod <- glm(grade~et+duration_+retakes, data = data.220, family = binomial (link = "logit"))

(result <- summary(mod))

(exp(coef(mod))-1)*100

vif(mod)

png(filename = '220diagnostics.png')
par(mfrow=c(2,2))
plot(mod)
dev.off()

D0 <- result$null.deviance

D1 <- result$deviance

df.null <- result$df.null

df.full <- result$df.residual

1-pchisq(D0-D1,df=(df.null-df.full))

pred <- predict(mod, data.220, type='response')

bacc <- 0

for(k in 0:100){
  pred.class <- as.numeric(pred >= k/100)
  
  tp <- sum(pred.class == 1 & pred.class == data.220$Course.Numeric.Grade)
  
  fp <- sum(pred.class == 1 & pred.class != data.220$Course.Numeric.Grade)
  
  tn <- sum(pred.class == 0 & pred.class == data.220$Course.Numeric.Grade)
  
  fn <- sum(pred.class == 0 & pred.class != data.220$Course.Numeric.Grade)
  
  sens <- tp/(tp+fn)
  
  spec <- tn/(tn+fp)
  
  bacc[k] <- (sens+spec)/2
}

bacc

which(bacc==max(bacc))

pred.class <- as.numeric(pred >= which(bacc==max(bacc))/100)

confusionMatrix(factor(pred.class), factor(data.220$Course.Numeric.Grade), positive = '1')

roc.pred <- prediction(pred, data.220$Course.Numeric.Grade)

roc.perf <- performance(roc.pred, 'tpr', 'fpr')

plot(roc.perf, main='220 Model')
abline(0, 1, col='RED')

performance(roc.pred, 'auc')@y.values[[1]]

pred.test <- predict(mod, data.220, type = 'response')

pred.class.test <- as.numeric(pred.test >= which(bacc==max(bacc))/100)

accuracy.3 <- confusionMatrix(factor(pred.class.test), factor(data.220$Course.Numeric.Grade))$overall[1]

