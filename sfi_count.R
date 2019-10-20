library(dplyr)
library(plyr)

setwd('C:/Users/Mitchell Schepps/Desktop/nena')
#Load in new data
pop <- read.csv('UCLAQuarterlyReportSeptember2019Population.csv', skip = 1)
arrests <- read.csv('UCLAQuarterlyReportSeptember2019Arrests.csv')

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
ncc1 <- read.csv('NCC1_sep19.csv', skip = 12, header = T)
ncc2 <- read.csv('NCC2_sep19.csv', skip = 12, header = T)

#Clean up NCC Data for merging purposes
ncc1 <- ncc1[,-1]
ncc2 <- ncc2[,-1]
ncc1 <- ncc1[1:4,]
names(ncc1)[1] <- 'id'
names(ncc2)[1] <- 'id'
names(ncc1)[5] <- 'ncc_date'
names(ncc2)[5] <- 'ncc_date'
ncc <- rbind(ncc1[,c(1,5)], ncc2[,c(1,5)])
ncc[ncc$id == '749462/169056', 'id'] = '749462'
ncc[ncc$id == '4852861/4589517', 'id'] = '4852861'


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

#Potential ids to watch out for.
# test2 <- test2[!is.na(test2$Intake.Date), ]
# test2 <- test2[!test2$id == 3239845,]
# test2 <- test2[!test2$id == 4575647,]

#List of unique ids
unique.ids <- unique(test2$id)

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
  cq.dates <- words$X.5 + 90*c(0:11)
  #Use NCC dates
  if(!is.na(unique(words$ncc_date))){
    ncc_date = unique(c(words$ncc_date))
    min.ncc.quarter = max(which(ncc_date >= cq.dates))
    cq.dates <- cq.dates[1:min.ncc.quarter]
  }
  
  #Begin creating the data file for the unique individual
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
                      day631 = cq.dates[8] + 1,
                      day720 = cq.dates[9],
                      day721 = cq.dates[9] + 1,
                      day800 = cq.dates[10],
                      day801 = cq.dates[10] + 1
  )
  
  #Fix two specific dates
  #I wish there was a better way.
  x = as.Date('2019-01-01', format = '%Y-%m-%d')
  if(cq.dates[1] == x){
    cq.dates[1] = x + 1
  }
  x = as.Date('2018-07-01', format = '%Y-%m-%d')
  if(any(cq.dates == x)){
    cq.dates[2] = x + 1
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

  ncc_date = unique(words$ncc_date)
  abbie$ncc = ncc_date
  abbie.final <- rbind.fill(abbie.final, abbie)
  
}

abbie.final2 <- merge(abbie.final, pop[, c('id', 'X.6', 'Intake.Date')], by = 'id')

# write.csv(abbie.final, 'ucla_arrest_data_190127.csv', row.names = F)


pfs <- abbie.final2[abbie.final2$X.6 == 'Pay for Success',]               
pas <- abbie.final2[abbie.final2$X.6 == 'Probation as Usual', ]               

pfs <- pfs[!(pfs$Intake.Date == ' '), ]

write.csv(pfs, 'Payforsuccess_flatfile_september19_191020.csv', row.names = F)

#Create dataframe for final numbers needed.
final <- data.frame(clean = c(),
                    potential = c())
for(i in 1:7){
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
#Diagnositics procedures
#Probably old and can be updated.
###############################################################################
old = read.csv('Payforsuccess_flatfile_september19_191018.csv')
abbie.final = abbie.final[abbie.final$id %in% pfs$id, ]

abbie.final$cq1 == pfs$cq1
abbie.final$cq2 == pfs$cq2
abbie.final$cq3 == pfs$cq3
abbie.final$cq4 == pfs$cq4
abbie.final$cq5 == pfs$cq5

abbie.final$pq1 == pfs$pq1
abbie.final$pq2 == pfs$pq2
abbie.final$pq3 == pfs$pq3
abbie.final$pq4 == pfs$pq4
abbie.final$pq5 == pfs$pq5

pfs2 <- read.csv('payforsuccess_flatfile_june_190819.csv')

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
