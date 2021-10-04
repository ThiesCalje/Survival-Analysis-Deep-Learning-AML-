library(readxl)
library(dplyr)
library(survival)
library(survminer)

data <- Data

survival <- Surv(data$`Survival_Days`, data$Vital_Status)

f1 <- survfit(survival ~ 1, data = data)

plot(f1, 
     xlab = "Days", 
     ylab = "Overall survival probability")

ggsurvplot(
  fit = f1, 
  data = data,
  title = sprintf('Overall survival (n = %g)',length(survival)),
  xlab = "Days", 
  ylab = "Overall survival probability",
  censor = FALSE)

survdiff(survival ~ Immunophenotype, data = data)
