library(readxl)
library(dplyr)
library(writexl)
Table_16 <- read_excel("C:/Users/thies/Google Drive/TU Delft/BEP/Data/Table 16.xlsx",
                       col_types = c("text", "text", "text", 
                                     "text", "numeric", "text", "text", 
                                     "text", "text", "text", "text", "numeric", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "numeric", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "numeric", "numeric", "numeric", "text", 
                                     "text"))
Table_10 <- read_excel("C:/Users/thies/Google Drive/TU Delft/BEP/Data/Table 10.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", "text", 
                                     "text", "text"))
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
#Data10['Key_Oncogenic_Driver_Event'] <-replace(Data10['Key_Oncogenic_Driver_Event'], Data10['Key_Oncogenic_Driver_Event']== 'Other'| Data10['Key_Oncogenic_Driver_Event']== 'Unknown','NA')
Data10['FAB_Designation'] <-replace(Data10['FAB_Designation'], Data10['FAB_Designation']== 'Unknown','NA')
Data10['Treatment_Protocol'] <- replace(Data10['Treatment_Protocol'], Data10['Treatment_Protocol'] == 'Not Reported', 'NA')
Data10['Treatment_FLT3_Inhibitor'] <- replace(Data10['Treatment_FLT3_Inhibitor'], Data10['Treatment_FLT3_Inhibitor'] == 'Not Reported', 'NA')
Data10['FLT3_ITD_Allelic_Ratio'] <- replace(Data10['FLT3_ITD_Allelic_Ratio'], Data10['FLT3_ITD_Allelic_Ratio'] == 'Not Reported' | Data10['FLT3_ITD_Allelic_Ratio'] == 'Not Determined', 'NA')
Data10['FLT3_ITD_Soft_Clip_Reads'] <- replace(Data10['FLT3_ITD_Soft_Clip_Reads'], Data10['FLT3_ITD_Soft_Clip_Reads'] == 'NA Exon to Exon ITD' , 'NA')
Data10['First_Event'] <- replace(Data10['First_Event'], Data10['First_Event'] == 'No Data', 'NA')
Data10['SCT_in_First_Remission'] <- replace(Data10['SCT_in_First_Remission'], Data10['SCT_in_First_Remission'] == 'No Data' | Data10['SCT_in_First_Remission'] == 'UKN'| Data10['SCT_in_First_Remission'] == 'Unknown', 'NA')


Table_10 <- Table_10[c(1,2,3,5,7,9,15,17,18,19,20,21,22)]
Table_16 <- Table_16[c(1,3,4,10,7,8,20,26,27,28,29,30,31)]

Data <- rbind(Table_10,Table_16)
Data0 <- subset(Data, Survival_Days == 0)
Data <- subset(Data, Survival_Days != is.na(Data['Survival_Days']))
Data <- rbind(Data,Data0)
Data <- subset(Data, Transcriptional_Identity != "none")
Data['Vital_Status'] <-replace(Data['Vital_Status'], Data['Vital_Status']== 'Alive','0')
Data['Vital_Status'] <-replace(Data['Vital_Status'], Data['Vital_Status']== 'Deceased','1')
Data['Vital_Status'] <- as.integer(Data$Vital_Status)

Data_competing_risk <- subset(Data, First_Event != 'No Data' & First_Event != 'Withdrawn from study' & First_Event != 'No Response')

Data_SCT <- subset(Data, SCT_in_First_Remission != 'UKN' & SCT_in_First_Remission != 'Unknown' & SCT_in_First_Remission != 'No Data')
Data_SCT_competing_risk <- subset(Data_competing_risk, SCT_in_First_Remission != 'UKN' & SCT_in_First_Remission != 'Unknown' & SCT_in_First_Remission != 'No Data')

Data_FAB <- subset(Data, FAB_Designation != 'NA' & FAB_Designation != 'Unknown' & FAB_Designation != 'UKN')
Data_FAB_competing_risk <- subset(Data_competing_risk, FAB_Designation != 'NA' & FAB_Designation != 'Unknown' & FAB_Designation != 'UKN')
Data_FAB_SCT <- subset(Data_SCT, FAB_Designation != 'NA' & FAB_Designation != 'Unknown' & FAB_Designation != 'UKN')
Data_FAB_SCT_competing_risk <- subset(Data_SCT_competing_risk, FAB_Designation != 'NA' & FAB_Designation != 'Unknown' & FAB_Designation != 'UKN')

write_xlsx(Data10,"G:/Mijn Drive/TU Delft/BEP/Survival-Analysis-Deep-Learning-AML-/Data/Data_10_processed.xlsx")
