library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(car)


data <- Data10

survival <- Surv(data$`Survival_Days`, data$Vital_Status)

cox_fit <- coxph(survival ~ Key_Oncogenic_Driver_Event+LSC6_Score, data = data)
summary(cox_fit)

ggsurvplot(
  fit = survfit(cox_fit), 
  data = data,
  title = sprintf('Overall survival (n = %g)',length(survival)),
  xlab = "Days", 
  ylab = "Overall survival probability",
  censor = FALSE)
