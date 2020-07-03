library(dplyr)
library(plyr)

setwd('C:/Users/Mitchell Schepps/Desktop/nena/arrests_pop')
# #Load in new data
pop <- read.csv('UCLAQuarterlyReportJune2020Population.csv', skip = 1)
arrests <- read.csv('UCLAQuarterlyReportJune2020Arrests.csv')
# 
# pop <- read.csv('UCLAQuarterlyReportDecember2019Population.csv', skip = 1)
# arrests <- read.csv('UCLAQuarterlyReportDecember2019Arrests.csv')

#Formal quarter dates
dates <- as.Date(c('10-23-2017', '01-01-2018', '04-01-2018', '07-01-2018', '10-01-2018',
                   '01-01-2019', '04-01-2019', '07-01-2019', '10-01-2019',
                   '01-01-2020', '04-01-2020', '07-01-2020', '10-01-2020'), format = '%m-%d-%Y')
#Remove initial date for calculations
dates <- dates[-1]

###############################################################################
#Incorporate new information.
###############################################################################
#Rename columns to match
names(pop)[1] <- 'id'
names(arrests)[1] <- 'id'

#Load in NCC Data
# ncc1 <- read.csv('ncc1_Jan20.csv', skip = 12, header = T)
# ncc2 <- read.csv('ncc2_Jan20.csv', skip = 12, header = T)

#Clean up NCC Data for merging purposes
# ncc1 <- ncc1[,-1]
# ncc2 <- ncc2[,-1]
# ncc1 <- ncc1[1:4,]
# names(ncc1)[1] <- 'id'
# names(ncc2)[1] <- 'id'
# names(ncc1)[5] <- 'ncc_date'
# names(ncc2)[6] <- 'ncc_date'
# ncc <- rbind(ncc1[,c(1,5)], ncc2[,c(1,6)])
# ncc[ncc$id == '749462/169056', 'id'] = '749462'
# ncc[ncc$id == '4852861/4589517', 'id'] = '4852861'
# dup = which(duplicated(ncc$id))
# ncc = ncc[-dup, ]
setwd('C:/Users/Mitchell Schepps/Desktop/nena')
#New ncc
ncc = read.csv('ncc_531.csv', skip = 9)

names(ncc)[1] = 'id'
names(ncc)[9] = 'ncc_date'
ncc = ncc[, c('id', 'ncc_date')]

#Merge population dataset and arrest dataset and ncc dataset
test <- merge(pop, arrests, by = 'id', all.x = T)
test <- merge(test, ncc, by = 'id', all.x = T)
test$ncc_date <- as.Date(test$ncc_date, format = '%m/%d/%Y')

#Format dates properly
test2 <- test
test2$X.5 <- as.Date(test2$X.5, format = '%m/%d/%Y')
test2$X.5 <- test2$X.5 - 1
test2$Intake.Date <- as.Date(test2$Intake.Date, format = '%m/%d/%Y')
test2$Referral.Date <- as.Date(test2$Referral.Date, format = '%m/%d/%Y')
test2$Arrest.Date <- as.Date(test2$Arrest.Date, format = '%m/%d/%Y')

#Sort out cohorts
ash = test2
# test2 = test2[test2$X.5 < as.Date('06/01/2019', format = '%m/%d/%Y'),]
#Potential ids to watch out for.
# test2 <- test2[!is.na(test2$Intake.Date), ]
# test2 <- test2[!test2$id == 3239845,]
# test2 <- test2[!test2$id == 4575647,]

#List of unique ids
unique.ids <- unique(test2$id)

#List for arrest matrix
arrest.matrix = data.frame()

#Make a loop which figures out which quarter in the formal dates the clean quarter if any happened in
abbie <- c()
abbie <- data.frame(abbie)
abbie.final <- c()
abbie.final <- data.frame(abbie.final)
for(i in 1:length(unique.ids)){
  #Extract one person's data to analyze at a time.
  words <- test2[test2$id == unique.ids[i], ]
  
  #Clean quarter dates
  words$X.5 <- as.Date(words$X.5, format = '%Y-%m-%d')
  cq.dates <- unique(words$X.5) + 90*c(0:6)
  #Use NCC dates
  if(!is.na(unique(words$ncc_date))){
    ncc_date = unique(c(words$ncc_date))
    min.ncc.quarter = max(which(ncc_date >= cq.dates))
    cq.dates <- cq.dates[1:min.ncc.quarter]
  }
  
  #Begin creating the data file for the unique individual
  abbie <- data.frame(id = unique(words$id),
                      randomization.date = unique(words$X.5),
                      risk.level = unique(words$Risk.Level),
                      race = unique(words$X.4),
                      day90  = cq.dates[2],
                      day91  = cq.dates[2] + 1,
                      day180 = cq.dates[3],
                      day181 = cq.dates[3] + 1,
                      day270 = cq.dates[4],
                      day271 = cq.dates[4] + 1,
                      day360 = cq.dates[5],
                      day361 = cq.dates[5] + 1,
                      day450 = cq.dates[6],
                      day451 = cq.dates[6] + 1,
                      day540 = cq.dates[7],
                      day541 = cq.dates[7] + 1,
                      day630 = cq.dates[8]
                      # day540 = cq.dates[6] + 90,
                      # day541 = cq.dates[6] + 91,
                      # day630 = cq.dates[6] + 180
  )
  
  #Fix two specific dates
  #I wish there was a better way.
  x = as.Date('2020-01-01', format = '%Y-%m-%d')
  if(any(cq.dates == x)){
    cq.dates[2] = x + 1
  }
  x = as.Date('2019-01-01', format = '%Y-%m-%d')
  if(cq.dates[1] == x){
    cq.dates[1] = x + 1
  }
  x = as.Date('2018-07-01', format = '%Y-%m-%d')
  if(any(cq.dates == x)){
    cq.dates[2] = x + 1
  }
  x = as.Date('2019-07-01', format = '%Y-%m-%d')
  if(any(cq.dates == x)){
    cq.dates[2] = x + 1
  }
  
  x = as.Date('2020-04-01', format = '%Y-%m-%d')
  if(any(cq.dates == x)){
    placement = which(cq.dates == x)
    cq.dates[placement] = x + 1
  }
  x = as.Date('2020-07-01', format = '%Y-%m-%d')
  if(any(cq.dates == x)){
    placement = which(cq.dates == x)
    cq.dates[placement] = x + 1
  }
  
  #Begin potential quarter calculation
  quarters_achieved = c()
  cq.dates.df = data.frame()
  for(i in 1:length(cq.dates)){
    cq.dates.df.1 = data.frame(cq.dates = cq.dates[i],
                               quarter  = max(which(cq.dates[i] > dates)))
    cq.dates.df = rbind(cq.dates.df, cq.dates.df.1)
    cq.dates2 = cq.dates
    cq.dates = cq.dates[-1]
    min.q = max(which(cq.dates[i] > dates))
    if(is.na(cq.dates[i+1])){
      cq.dates[i + 1] = cq.dates[i]
    }
    min.q = ifelse(cq.dates[i+1] - cq.dates[i] == 90, min.q, min.q)
    max.q = min(which(cq.dates[i] < dates))
    quarters_achieved = c(quarters_achieved, min.q)
    cq.dates = cq.dates2
  }
  
  if(length(cq.dates) == 1){
    quarters_achieved = 0
  }
  quarters_achieved = quarters_achieved[!is.na(quarters_achieved)]
  
  abbie$cq1 <- ifelse(1 %in% quarters_achieved, sum(quarters_achieved == 1), 0)
  abbie$cq2 <- ifelse(2 %in% quarters_achieved, sum(quarters_achieved == 2), 0)
  abbie$cq3 <- ifelse(3 %in% quarters_achieved, sum(quarters_achieved == 3), 0)
  abbie$cq4 <- ifelse(4 %in% quarters_achieved, sum(quarters_achieved == 4), 0)
  abbie$cq5 <- ifelse(5 %in% quarters_achieved, sum(quarters_achieved == 5), 0)
  abbie$cq6 <- ifelse(6 %in% quarters_achieved, sum(quarters_achieved == 6), 0)
  abbie$cq7 <- ifelse(7 %in% quarters_achieved, sum(quarters_achieved == 7), 0)
  abbie$cq8 <- ifelse(8 %in% quarters_achieved, sum(quarters_achieved == 8), 0)
  abbie$cq9 <- ifelse(9 %in% quarters_achieved, sum(quarters_achieved == 9), 0)
  abbie$cq10 <- ifelse(10 %in% quarters_achieved, sum(quarters_achieved == 10), 0)
  
  #Strict arrest days array for making abbie
  abbie$num.arrests = 0
  arrests.abbie <- c(words$Arrest.Date)
  arrests.type <- words$Offense.Level
  if(!is.na(arrests.abbie)){
    abbie$num.arrests = length(arrests.abbie)
    if(length(arrests.abbie) == 1){
      abbie$arrest1 <- arrests.abbie
      abbie$arrest1type <- arrests.type
    } else if(length(arrests.abbie) == 2){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
    } else if(length(arrests.abbie) == 3){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
    } else if(length(arrests.abbie) == 4){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
    } else if(length(arrests.abbie) == 5){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest4type <- arrests.type[5]
    } else if(length(arrests.abbie) == 6){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
    } else if(length(arrests.abbie) == 7){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
    } else if(length(arrests.abbie) == 8){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
      abbie$arrest8type <- arrests.type[8]
    } else if(length(arrests.abbie) == 9){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
      abbie$arrest9 <- arrests.abbie[9]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
      abbie$arrest8type <- arrests.type[8]
      abbie$arrest9type <- arrests.type[9]
    } else if(length(arrests.abbie) == 10){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
      abbie$arrest9 <- arrests.abbie[9]
      abbie$arrest10 <- arrests.abbie[10]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
      abbie$arrest8type <- arrests.type[8]
      abbie$arrest9type <- arrests.type[9]
      abbie$arrest10type <- arrests.type[10]
    } else if(length(arrests.abbie) == 11){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
      abbie$arrest9 <- arrests.abbie[9]
      abbie$arrest10 <- arrests.abbie[10]
      abbie$arrest11 <- arrests.abbie[11]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
      abbie$arrest8type <- arrests.type[8]
      abbie$arrest9type <- arrests.type[9]
      abbie$arrest10type <- arrests.type[10]
      abbie$arrest11type <- arrests.type[11]
    } else if(length(arrests.abbie) == 12){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
      abbie$arrest9 <- arrests.abbie[9]
      abbie$arrest10 <- arrests.abbie[10]
      abbie$arrest11 <- arrests.abbie[11]
      abbie$arrest12 <- arrests.abbie[12]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
      abbie$arrest8type <- arrests.type[8]
      abbie$arrest9type <- arrests.type[9]
      abbie$arrest10type <- arrests.type[10]
      abbie$arrest11type <- arrests.type[11]
      abbie$arrest12type <- arrests.type[12]
    }else if(length(arrests.abbie) == 13){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
      abbie$arrest9 <- arrests.abbie[9]
      abbie$arrest10 <- arrests.abbie[10]
      abbie$arrest11 <- arrests.abbie[11]
      abbie$arrest12 <- arrests.abbie[12]
      abbie$arrest13 <- arrests.abbie[13]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
      abbie$arrest8type <- arrests.type[8]
      abbie$arrest9type <- arrests.type[9]
      abbie$arrest10type <- arrests.type[10]
      abbie$arrest11type <- arrests.type[11]
      abbie$arrest12type <- arrests.type[12]
      abbie$arrest13type <- arrests.type[13]
    }
    else if(length(arrests.abbie) == 14){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
      abbie$arrest9 <- arrests.abbie[9]
      abbie$arrest10 <- arrests.abbie[10]
      abbie$arrest11 <- arrests.abbie[11]
      abbie$arrest12 <- arrests.abbie[12]
      abbie$arrest13 <- arrests.abbie[13]
      abbie$arrest14 <- arrests.abbie[14]
      abbie$arrest1type <- arrests.type[1]
      abbie$arrest2type <- arrests.type[2]
      abbie$arrest3type <- arrests.type[3]
      abbie$arrest4type <- arrests.type[4]
      abbie$arrest5type <- arrests.type[5]
      abbie$arrest6type <- arrests.type[6]
      abbie$arrest7type <- arrests.type[7]
      abbie$arrest8type <- arrests.type[8]
      abbie$arrest9type <- arrests.type[9]
      abbie$arrest10type <- arrests.type[10]
      abbie$arrest11type <- arrests.type[11]
      abbie$arrest12type <- arrests.type[12]
      abbie$arrest13type <- arrests.type[13]
      abbie$arrest14type <- arrests.type[14]
    }
  }
  
  #Ever arrested indicator variable
  abbie$ever.arrested <- ifelse(any(is.na(arrests.abbie)), 0, 1)
  
  
  #Which quarter were you arrested in calculation
  arrests = c(words$Arrest.Date)
  arrest.quarter.list = c()
  cq.dates = cq.dates2
  if(is.na(arrests[1])){
    quarters_achieved = quarters_achieved
  } else{
    for(j in 1:length(arrests)){
      arrest.quarter.min = max(which(arrests[j] > cq.dates))
      arrest.quarter.min = cq.dates.df[arrest.quarter.min + 1, 2]
      arrest.quarter.max = min(which(arrests[j] <  cq.dates)) - 1
      arrest.quarter.list = c(arrest.quarter.list, arrest.quarter.min)
    }
  }
  
  #Arrest matrix
  arrest.inter = data.frame(id = unique(words$id))
  arrest.inter$one = sum(arrest.quarter.list == 1)
  arrest.inter$two = sum(arrest.quarter.list == 2)
  arrest.inter$thr = sum(arrest.quarter.list == 3)
  arrest.inter$fou = sum(arrest.quarter.list == 4)
  arrest.inter$fiv = sum(arrest.quarter.list == 5)
  arrest.inter$six = sum(arrest.quarter.list == 6)
  arrest.inter$sev = sum(arrest.quarter.list == 7)
  arrest.inter$eig = sum(arrest.quarter.list == 8)
  arrest.inter$nin = sum(arrest.quarter.list == 9)
  arrest.inter$ten = sum(arrest.quarter.list == 10)
  
  arrest.matrix = rbind(arrest.matrix, arrest.inter)
  #Not sure if this will fix the issue where 2 potential quarters, but both were arrested in.
  duplicated_quarters = quarters_achieved[which(duplicated(quarters_achieved) & quarters_achieved %in% arrest.quarter.list)]
  quarters_achieved = quarters_achieved[!(quarters_achieved %in% arrest.quarter.list)]
  quarters_achieved = c(quarters_achieved, duplicated_quarters)
  
  abbie$pq1 <- ifelse(1 %in% quarters_achieved, sum(quarters_achieved == 1), 0)
  abbie$pq2 <- ifelse(2 %in% quarters_achieved, sum(quarters_achieved == 2), 0)
  abbie$pq3 <- ifelse(3 %in% quarters_achieved, sum(quarters_achieved == 3), 0)
  abbie$pq4 <- ifelse(4 %in% quarters_achieved, sum(quarters_achieved == 4), 0)
  abbie$pq5 <- ifelse(5 %in% quarters_achieved, sum(quarters_achieved == 5), 0)
  abbie$pq6 <- ifelse(6 %in% quarters_achieved, sum(quarters_achieved == 6), 0)
  abbie$pq7 <- ifelse(7 %in% quarters_achieved, sum(quarters_achieved == 7), 0)
  abbie$pq8 <- ifelse(8 %in% quarters_achieved, sum(quarters_achieved == 8), 0)
  abbie$pq9 <- ifelse(9 %in% quarters_achieved, sum(quarters_achieved == 9), 0)
  abbie$pq10 <- ifelse(10 %in% quarters_achieved, sum(quarters_achieved == 10), 0)
  
  
  abbie[abbie$id == 3334613, 'cq5'] = 0
  abbie[abbie$id == 3334613, 'cq6'] = 1
  
  abbie$jan_march18 <- ifelse(abbie$cq1 != 0 & abbie$pq1 != 0, abbie$pq1,
                              ifelse(abbie$cq1 != 0 & abbie$pq1 == 0, 0, NA))
  abbie$apr_june18  <- ifelse(abbie$cq2 != 0 & abbie$pq2 != 0, abbie$pq2,
                              ifelse(abbie$cq2 != 0 & abbie$pq2 == 0, 0, NA))
  abbie$july_sep18  <- ifelse(abbie$cq3 != 0 & abbie$pq3 != 0, abbie$pq3,
                              ifelse(abbie$cq3 != 0 & abbie$pq3 == 0, 0, NA))
  abbie$oct_dec18   <- ifelse(abbie$cq4 != 0 & abbie$pq4 != 0, abbie$pq4,
                              ifelse(abbie$cq4 != 0 & abbie$pq4 == 0, 0, NA))
  abbie$jan_march19 <- ifelse(abbie$cq5 != 0 & abbie$pq5 != 0, abbie$pq5,
                              ifelse(abbie$cq5 != 0 & abbie$pq5 == 0, 0, NA))
  abbie$apr_june19 <- ifelse(abbie$cq6 != 0 & abbie$pq6 != 0, abbie$pq6,
                             ifelse(abbie$cq6 != 0 & abbie$pq6 == 0, 0, NA))
  abbie$july_sep19 <- ifelse(abbie$cq7 != 0 & abbie$pq7 != 0, abbie$pq7,
                             ifelse(abbie$cq7 != 0 & abbie$pq7 == 0, 0, NA))
  abbie$oct_dec19 <- ifelse(abbie$cq8 != 0 & abbie$pq8 != 0, abbie$pq8,
                            ifelse(abbie$cq8 != 0 & abbie$pq8 == 0, 0, NA))
  abbie$jan_mar20 <- ifelse(abbie$cq9 != 0 & abbie$pq9 != 0, abbie$pq9,
                            ifelse(abbie$cq9 != 0 & abbie$pq9 == 0, 0, NA))
  abbie$apr_june20 <- ifelse(abbie$cq10 != 0 & abbie$pq10 != 0, abbie$pq10,
                            ifelse(abbie$cq10 != 0 & abbie$pq10 == 0, 0, NA))

  ncc_date = unique(words$ncc_date)
  abbie$ncc = ncc_date
  abbie.final <- rbind.fill(abbie.final, abbie)
  
}

abbie.final2 <- merge(abbie.final, pop[, c('id', 'X.6', 'Intake.Date')], by = 'id')
indice = which(names(abbie.final2) == 'X.6')
names(abbie.final2)[indice] = 'RandomGroup'
# write.csv(abbie.final, 'ucla_arrest_data_190127.csv', row.names = F)


pfs <- abbie.final2[abbie.final2$RandomGroup == 'Pay for Success',]               
pas <- abbie.final2[abbie.final2$RandomGroup == 'Probation as Usual', ]               

pfs <- pfs[!(pfs$Intake.Date == ' '), ]
pfs <- (pfs[as.Date(pfs$Intake.Date, format = '%m/%d/%Y') <= as.Date('06/30/2020', format = '%m/%d/%Y'),])

# pfs$arrest9 = NULL

write.csv(pfs, 'Payforsuccess_flatfile_June20.csv', row.names = F)

#Create dataframe for final numbers needed.
final <- data.frame(clean = c(),
                    potential = c())
for(i in 1:10){
  word.used.p = paste0('pq', i)
  word.used.c = paste0('cq', i)
  
  df = data.frame(clean = sum(pfs[[word.used.p]]),
                  potential =sum(pfs[[word.used.c]]))
  final = rbind(final, df)
}

###############################################################################
###############################################################################
###############################################################################
#Start of testing to be done
###############################################################################
#Formal z-test
num.arrested.pfs <- sum(pfs$ever.arrested)
num.arrested.pas <- sum(pas$ever.arrested)

prop.test(c(num.arrested.pfs, num.arrested.pas), n = c(nrow(pfs), nrow(pas)))


###############################################################################
#Heatmap
#Heatmap
###############################################################################
arrest.matrix = na.omit(arrest.matrix)

arrest.matrix2 <- merge(arrest.matrix, pop[, c('id', 'X.6')], by = 'id')

arrest.matrix2.pfs = arrest.matrix2[arrest.matrix2$X.6 == 'Pay for Success',]
arrest.matrix2.pau = arrest.matrix2[arrest.matrix2$X.6 == 'Probation as Usual',]
arrest.data.pfs = data.matrix(arrest.matrix2.pfs[, 2:9])
arrest.data.pau = data.matrix(arrest.matrix2.pau[, 2:9])

par(mfrow = c(2, 1))
arrest.heatmap <- heatmap(arrest.data.pfs, Colv=NA, col = cm.colors(256), scale="column")
arrest.heatmap <- heatmap(arrest.data.pau, Colv=NA, col = cm.colors(256), scale="column")

#Arrests per group
pfs.month = colSums(arrest.matrix2.pfs[,2:9])
pau.month = colSums(arrest.matrix2.pau[,2:9])
data = data.frame(month = 1:8,
                  pfs = pfs.month,
                  pau = pau.month)
library(ggplot2)
ggplot(data) + geom_line(aes(y = pau, x = month)) + geom_line(aes(y = pfs, x = month))

#Want to include arrests adjusted for sample size per group.

###############################################################################
#Diagnositics procedures
#Probably old and can be updated.
###############################################################################
which(abbie$day90 - dates <= 0)[1] - 1



test = pfs[!(pfs$id %in% pfs2$id),]

a = abbie.final2[abbie.final2$pq8 == 1, 'id']
b = final2[abbie.final2$pq8 == 1, 'id']
which((b %in% a))
