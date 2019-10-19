library(dplyr)
library(plyr)
#Old data
# pop <- read.csv('C:/Users/msche/OneDrive/Desktop/UCLAQuarterlyReportPopulation.csv', skip = 1)
# 
# arrests <- read.csv('C:/Users/msche/OneDrive/Desktop/UCLAQuarterlyReportArrests.csv')
# 

setwd('C:/Users/Mitchell Schepps/Desktop/nena')
#Load in new data
pop <- read.csv('UCLAQuarterlyReportSeptember2019Population.csv', skip = 1)

arrests <- read.csv('UCLAQuarterlyReportSeptember2019Arrests.csv')
pfs2 <- read.csv('payforsuccess_flatfile_june_190819.csv')
#Formal quarter dates
dates <- as.Date(c('10-23-2017', '01-01-2018', '04-01-2018', '07-01-2018', '10-01-2018',
                   '01-01-2019', '04-01-2019', '07-01-2019', '10-01-2019',
                   '01-01-2020', '04-01-2020', '07-01-2020', '10-01-2020'), format = '%m-%d-%Y')

dates <- dates[-1]
#Rename columns to match
names(pop)[1] <- 'id'
names(arrests)[1] <- 'id'

###############################################################################
#Incorporate new information.
###############################################################################
ncc1 <- read.csv('NCC1_june.csv', skip = 12, header = T)
ncc2 <- read.csv('NCC2_june.csv', skip = 12, header = T)

ncc1 <- ncc1[,-1]
ncc2 <- ncc2[,-1]
ncc1 <- ncc1[1:4,]

names(ncc1)[1] <- 'id'
names(ncc2)[1] <- 'id'
names(ncc1)[5] <- 'ncc_date'
names(ncc2)[5] <- 'ncc_date'

ncc <- rbind(ncc1[,c(1,5)], ncc2[,c(1,5)])


#Merge population dataset and arrest dataset
test <- merge(pop, arrests, by = 'id', all.x = T)
test <- merge(test, ncc, by = 'id', all.x = T)
test$ncc_date <- as.Date(test$ncc_date, format = '%m/%d/%Y')


#Subtract dates to get relevant quarter information.
#Format dates properly
test2 <- test
test2$X.5 <- as.Date(test2$X.5, format = '%m/%d/%Y')
test2$X.5 <- test2$X.5 - 1
test2$Intake.Date <- as.Date(test2$Intake.Date, format = '%m/%d/%Y')
test2$Referral.Date <- as.Date(test2$Referral.Date, format = '%m/%d/%Y')
test2$Arrest.Date <- as.Date(test2$Arrest.Date, format = '%m/%d/%Y')

# test2 <- test2[!is.na(test2$Intake.Date), ]
# test2 <- test2[!test2$id == 3239845,]
# test2 <- test2[!test2$id == 4575647,]
unique.ids <- unique(test2$id)

#Make a loop which figures out which quarter in the formal dates the clean quarter if any happened in
final <- c()
overall <- c()
abbie <- c()
abbie <- data.frame(abbie)
abbie.final <- c()
abbie.final <- data.frame(abbie.final)
for(i in 1:length(unique.ids)){
  zz = i
  #Extract one person's data to analyze at a time.
  words <- test2[test2$id == unique.ids[i], ]
  
  #Make a vector of dates in order with the arrest dates included from their intake date
  #arrest dates
  row <- c(words$Arrest.Date)
  if(is.na(row)){
    row <- min(words$X.5 + 90)
  }
  #Clean quarter dates
  words$X.5 <- as.Date(words$X.5, format = '%Y-%m-%d')
  cq.dates <- words$X.5 + 90*c(0:11)
  if(!is.na(unique(words$ncc_date))){
    ncc_date = unique(c(words$ncc_date))
    min.ncc.quarter = max(which(ncc_date >= cq.dates))
    cq.dates <- cq.dates[1:min.ncc.quarter]
  }
  
  #Begin creating the data file
  abbie <- data.frame(id = unique(words$id),
                      randomization.date = unique(words$X.5),
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
                      day630 = cq.dates[8],
                      day631 = cq.dates[8] + 1
  )
  
  
  x = as.Date('2019-01-01', format = '%Y-%m-%d')
  if(cq.dates[1] == x){
    cq.dates[1] = x + 1
  }
  x = as.Date('2018-07-01', format = '%Y-%m-%d')
  if(any(cq.dates == x)){
    cq.dates[2] = x + 1
  }
  
  
  
  
  # min.q = if(cq.dates[i] > dates[1] & )
  
  quarters_achieved = c()
  cq.dates.df = data.frame()
  for(i in 1:length(cq.dates)){
    cq.dates.df.1 = data.frame(cq.dates = cq.dates[i],
                               quarter  = max(which(cq.dates[i] > dates)))
    cq.dates.df = rbind(cq.dates.df, cq.dates.df.1)
    # if(i == length(cq.dates)){
    #   min.q = max(which(cq.dates[i] >= dates))
    #   quarters_achieved = c(quarters_achieved, min.q)
    # }else{
    
    # add.dates = which(cq.dates[i] == dates)
    # cq.dates[add.dates] = cq.dates[add.dates] + 1
    cq.dates2 = cq.dates
    cq.dates = cq.dates[-1]
    
    
    min.q = max(which(cq.dates[i] > dates))
    # if(any(cq.dates[i] == dates)){
    #   min.q = max(which(cq.dates[i] >= dates))
    # }
    # min.q = ifelse(any(cq.dates[i] == dates), 
    #                max(which(cq.dates[i] >= dates)),
    #                max(which(cq.dates[i] >  dates)))
    # min.q = ifelse(any(cq.dates[i] == dates), max(which(cq.dates[i] - 89 > dates)), min.q)
    if(is.na(cq.dates[i+1])){
      cq.dates[i + 1] = cq.dates[i]
    }
    # min.q = ifelse(cq.dates[i+1] - dates[min.q] >= 90, min.q, 0)
    min.q = ifelse(cq.dates[i+1] - cq.dates[i] == 90, min.q, min.q)
    max.q = min(which(cq.dates[i] < dates))
    # if(i == 1){
    #   min.q = ifelse(any(cq.dates[1] == dates), max(which(cq.dates[i] >= dates)), min.q)
    # }
    quarters_achieved = c(quarters_achieved, min.q)
    cq.dates = cq.dates2
  }
  
  # 
  # if(is.na(ncc_date)){
  #   quarters_achieved = quarters_achieved
  # } else{
  #   ncc_quarter = max(which(ncc_date >= dates))
  #   quarters_achieved = quarters_achieved[quarters_achieved < ncc_quarter]
  # }
  
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
  
  
  testing <- c(cq.dates, row)
  sorted.all <- sort(testing)
  
  
  
  #Strict arrest days array for making abbie
  arrests.abbie <- c(words$Arrest.Date)
  if(!is.na(arrests.abbie)){
    if(length(arrests.abbie) == 1){
      abbie$arrest1 <- arrests.abbie
    } else if(length(arrests.abbie) == 2){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
    } else if(length(arrests.abbie) == 3){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
    } else if(length(arrests.abbie) == 4){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
    } else if(length(arrests.abbie) == 5){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
    } else if(length(arrests.abbie) == 6){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
    } else if(length(arrests.abbie) == 7){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
    } else if(length(arrests.abbie) == 8){
      abbie$arrest1 <- arrests.abbie[1]
      abbie$arrest2 <- arrests.abbie[2]
      abbie$arrest3 <- arrests.abbie[3]
      abbie$arrest4 <- arrests.abbie[4]
      abbie$arrest5 <- arrests.abbie[5]
      abbie$arrest6 <- arrests.abbie[6]
      abbie$arrest7 <- arrests.abbie[7]
      abbie$arrest8 <- arrests.abbie[8]
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
      abbie$arrest12 <- arrests.abbie[13]
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
      abbie$arrest12 <- arrests.abbie[13]
      abbie$arrest12 <- arrests.abbie[14]
    }
  }
  
  
  abbie$ever.arrested <- ifelse(any(is.na(arrests.abbie)), 0, 1)
  
  
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
  
  # for(i in 1:length(arrest.quarter.list)){
  #   q.remove = which(quarters_achieved[i] %in% arrest.quarter.list)[1]
  #   quarters_achieved = quarters_achieved[-q.remove]
  # }
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
  
  abbie$det = unique(words$X.6)
  
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
  # abbie$jan_march18 <- ifelse(abbie$cq1 == 1 & abbie$pq1 == 1, 1,
  #                    ifelse(abbie$cq1 == 1 & abbie$pq1 == 0, 0, NA))
  # abbie$apr_june18 <- ifelse(abbie$cq2 == 1 & abbie$pq2 == 1, 1,
  #                ifelse(abbie$cq2 == 1 & abbie$pq2 == 0, 0, NA))
  # abbie$july_sep18 <- ifelse(abbie$cq3 == 1 & abbie$pq3 == 1, 1,
  #                    ifelse(abbie$cq3 == 1 & abbie$pq3 == 0, 0, NA))
  # abbie$oct_dec18 <- ifelse(abbie$cq4 == 1 & abbie$pq4 == 1, 1,
  #                    ifelse(abbie$cq4 == 1 & abbie$pq4 == 0, 0, NA))
  # 
  ncc_date = unique(words$ncc_date)
  abbie$ncc = ncc_date
  abbie.final <- rbind.fill(abbie.final, abbie)
  
}

abbie.final2 <- merge(abbie.final, pop[, c('id', 'X.6', 'Intake.Date')], by = 'id')

# write.csv(abbie.final, 'ucla_arrest_data_190127.csv', row.names = F)


pfs <- abbie.final2[abbie.final2$X.6 == 'Pay for Success',]               
pas <- abbie.final2[abbie.final2$X.6 == 'Probation as Usual', ]               

pfs <- pfs[!(pfs$Intake.Date == ' '), ]

write.csv(pfs, 'Payforsuccess_flatfile_september19_191018.csv', row.names = F)

#Formal z-test
num.arrested.pfs <- sum(pfs$ever.arrested)
num.arrested.pas <- sum(pas$ever.arrested)

prop.test(c(num.arrested.pfs, num.arrested.pas), n = c(nrow(pfs), nrow(pas)))

pfs.arrests <- pfs[!is.na(pfs$arrest1),]
pas.arrests <- pas[!is.na(pas$arrest1),]

pfs.arrest.diff <- pfs.arrests$arrest1 - pfs.arrests$randomization.date
pas.arrest.diff <- pas.arrests$arrest1 - pas.arrests$randomization.date

mean(pfs.arrest.diff)
sd(pfs.arrest.diff)
mean(pas.arrest.diff)
sd(pas.arrest.diff)

old <- read.csv('PayforSuccess_flatfile_dec_190115.csv')

old$jan_march18 == pfs$jan_march18
old$apr_june18 == pfs$apr_june18
old$july_sep18 == pfs$july_sep18
old$oct_dec18 == pfs$oct_dec18

rashmi3 <- read.csv('rashmi.csv')
rashmi <- read.csv('rashmi_june19.csv')

names(rashmi)[1] <- 'id'
names(rashmi)[2] <- 'cq1'
names(rashmi)[3] <- 'cq2'
names(rashmi)[4] <- 'cq3'
names(rashmi)[5] <- 'cq4'
names(rashmi)[6] <- 'cq5'
names(rashmi)[7] <- 'cq6'

names(rashmi)[8] <- 'pq1'
names(rashmi)[9] <- 'pq3'
names(rashmi)[10] <- 'pq4'
names(rashmi)[11] <- 'pq5'
names(rashmi)[12] <- 'pq6'
names(rashmi)[13] <- 'pq7'
names(rashmi)[14] <- 'pq8'

rashmi$cq1 <- as.numeric(as.character(rashmi$cq1))
rashmi$cq2 <- as.numeric(as.character(rashmi$cq2))
rashmi$cq3 <- as.numeric(as.character(rashmi$cq3))
rashmi$cq4 <- as.numeric(as.character(rashmi$cq4))
rashmi$cq5 <- as.numeric(as.character(rashmi$cq5))
rashmi$cq6 <- as.numeric(as.character(rashmi$cq6))

rashmi$pq1 <- as.numeric(as.character(rashmi$pq1))
rashmi$pq3 <- as.numeric(as.character(rashmi$pq3))
rashmi$pq4 <- as.numeric(as.character(rashmi$pq4))
rashmi$pq5 <- as.numeric(as.character(rashmi$pq5))
rashmi$pq6 <- as.numeric(as.character(rashmi$pq6))
rashmi$pq7 <- as.numeric(as.character(rashmi$pq7))


which(rashmi$cq1 != pfs$jan_march18)
which(rashmi$cq2 != pfs$apr_june18)
which(rashmi$cq3 != pfs$july_sep18)
which(rashmi$cq4 != pfs$oct_dec18)
which(rashmi$cq5 != pfs$jan_march19)
which(rashmi$cq6 != pfs$apr_june19)

test <- pfs[pfs$apr_june18 == 1,]
test <- test[!is.na(test$apr_june18),]
test2 <- rashmi[rashmi$pq3 == 1,]
test2 <- test2[!is.na(test2$pq3),]

which(test2$id %in% test$id)

noob <- pfs[pfs$id %in% c(152977, 4235457, 4320120,
                          532098),]
r3 = rashmi$cq3[c(9, 27, 98, 99)]
r4 = rashmi$cq4[c(9, 27, 98, 99)]

p3 <- noob$cq3
p4 <- noob$cq4

#july_sep18
ind = which(unique.ids == 4320120)
i = ind


# pfs_old = pfs
# pfs_comp = pfs[pfs$id %in% pfs_old$id,]

pfs_comp$id == pfs_old$id
pfs_comp$pq2 == pfs_old$pq2


which(abbie$day90 - dates <= 0)[1] - 1



test = pfs[!(pfs$id %in% pfs2$id),]
