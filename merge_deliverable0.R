library(dplyr)
library(plyr)
#Old data
# pop <- read.csv('C:/Users/msche/OneDrive/Desktop/UCLAQuarterlyReportPopulation.csv', skip = 1)
# 
# arrests <- read.csv('C:/Users/msche/OneDrive/Desktop/UCLAQuarterlyReportArrests.csv')
# 

setwd('C:/Users/Mitchell Schepps/Desktop/nena')
#Load in new data
pop <- read.csv('UCLAQuarterlyReportDecember2019.csv')
#Quarterly report population tab
#intake date 2019, 2018, 2017
#283
pop <- pop[-1,]
arrests <- read.csv('UCLAQuarterlyReportSeptember2019Arrests.csv')

format = read.csv('format.csv', skip = 6)

new = read.csv('supplemental.csv')
#Null and 2020 Client enroll date gone
new$PNumber <- as.character(new$PNumber)
new[new$PNumber == '149117/1540210', 'PNumber'] = '149117'
new[new$PNumber == '2950086/900986', 'PNumber'] = '2950086'
names(pop)[1] = 'PNumber'
names(pop)[3] = 'Age'
names(pop)[4] = 'Gender'
names(pop)[5] = 'Race'
names(pop)[6] = 'Current ORAS Score'
names(pop)[7] = 'Current ORAS Score Date'
names(pop)[8] = 'Randomization Date'
names(pop)[12] = 'Interface Intake Date'

names(abbie.final)[37] = 'Arrest 1 Date'
names(abbie.final)[38] = 'Offense Level Arrest #1 (Mis/Felony)'
names(abbie.final)[39] = 'Arrest 2 Date'
names(abbie.final)[40] = 'Offense Level Arrest #2 (Mis/Felony)'


second_merge = abbie.final[, c(1, 37:40)]
names(second_merge)[1] = 'PNumber'
first_merge = pop[,c(1,3:8, 12)]
third_merge = new[,c(1,3:ncol(new))]

df <- merge(new[,1:2], first_merge, by = 'PNumber')
df <- merge(df, second_merge, by = 'PNumber')
df <- merge(df, third_merge)

names(df)[14] <- 'Client Enrollment Date'
names(df)[15] <- 'Months of Treatment Completed'
names(df)[16] <- 'Client Active Status'
names(df)[17] <- 'Graduated Sanction?'
names(df)[18] <- 'Case Management'
names(df)[19] <- 'Moral Reconation Therapy (MRT)'
names(df)[20] <- 'Triple P (Relationship Skills)'
names(df)[21] <- 'Seeking Safety (Trauma Therapy)'
names(df)[22] <- 'Job Readiness'
names(df)[23] <- 'Other CBT'
names(df)[24] <- 'Case Plan Progress'
names(df)[25] <- 'Case Plan Completion Date'
names(df)[26] <- 'MRT Start Date'
names(df)[27] <- 'MRT Step Achieved'
names(df)[28] <- 'MRT Completion Date'
names(df)[29] <- 'Triple P Start Date'
names(df)[30] <- 'Triple P Modules Completed (Out of 10)'
names(df)[31] <- 'Seeking Safety Start Date'
names(df)[32] <- 'Seeking Safety Modules Completed (Out of 24)'
names(df)[33] <- 'Job Readiness Start Date'
names(df)[34] <- 'Job Readiness Steps Achieved (Out of 7)'
names(df)[35] <- 'Job Readiness Completion Date'
names(df)[36] <- 'Other CBT Start Date'
names(df)[37] <- 'Other CBT Modules Completed'
names(df)[38] <- 'Psychiatric Referral'
names(df)[39] <- 'Substance Use Referral'
names(df)[40] <- 'Graduation Date'
names(df)[41] <- 'Discharge Date'
names(df)[42] <- 'Discharge Reason'
names(df)[43] <- 'Date of most recent SSM'
names(df)[44] <- 'Most Recent Type of SSM'
names(df)[45] <- 'Reason SSM not completed'

write.csv(df, 'deliverable0.csv', row.names = F)

