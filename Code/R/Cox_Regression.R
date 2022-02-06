library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(car)


Table_10 <- read_excel("G:/Mijn Drive/TU Delft/BEP/Survival-Analysis-Deep-Learning-AML-/Data/Table 10.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "text", "text", "numeric", "numeric", 
                                             "numeric", "text", "text", "text"))

Data10 <- Table_10[c(1,2,3,4,5,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
Data0 <- subset(Data10, Survival_Days == 0)
Data10 <- subset(Data10, Survival_Days != is.na(Data10['Survival_Days']))
Data10 <- rbind(Data10,Data0)
Data10['Vital_Status'] <-replace(Data10['Vital_Status'], Data10['Vital_Status']== 'Alive','0')
Data10['Vital_Status'] <-replace(Data10['Vital_Status'], Data10['Vital_Status']== 'Deceased','1')
Data10['Vital_Status'] <- as.integer(Data10$Vital_Status)

survival <- Surv(Data10$`Survival_Days`, Data10$Vital_Status)

cox_fit <- coxph(survival ~ Transcriptional_Identity+Key_Oncogenic_Driver_Event, data = Data10)
summary(cox_fit)

ggsurvplot(
  fit = survfit(cox_fit), 
  data = data,
  title = sprintf('Overall survival (n = %g)',length(survival)),
  xlab = "Days", 
  ylab = "Overall survival probability",
  censor = FALSE)
