#!/usr/bin/env Rscript


# load data 
library(readr)
library(survival)
got <- read_csv("got.csv")
hist(got$duration_in_episodes)


# How to make a Kaplan-Meier plot

# the Surv object
surv=Surv(got$duration_in_episodes,got$is_dead) # Surv(time, event_indicator) for right-censored data
fit1=survfit(surv~1)

# fit two groups separately
fit2=survfit(Surv(duration_in_episodes,is_dead)~gender,got) 

# plot
par(mfrow=c(1,2))
plot(fit1)
abline(h=0.8)
abline(v=37)
plot(fit2,col=1:2, lwd=2, mark.time=TRUE,xlab="Time", ylab="Survival")
legend('bottomleft', c("female", "male"),col=1:2, lwd=2, bty='n')

# get statistics
quantile(fit1,0.2)
summary(fit1,time=37)
summary(fit2,time=37)
quantile(fit1,0.5)



# K-M curve: ggplot version
library(survminer)
library(ggfortify)
ggsurvplot(fit2,conf.int = T)



# How to perform log-rank test
survdiff(surv~gender,got)


# How to build a Cox model
mod=coxph(surv~gender+s1_screenTime+s2_screenTime+s3_screenTime+s4_screenTime+s5_screenTime+s6_screenTime+s7_screenTime,got)
# interpretation: 10% increase in X


# Diagnostics
# Schoenfeld
ggcoxdiagnostics(mod, type = 'schoenfeld')
# Deviance
ggcoxdiagnostics(mod, type = 'deviance', linear.predictions = F)




