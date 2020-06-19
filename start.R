# library(dplyr)
# library(plyr)
# library(ggplot2)
# 
# setwd('C:/Users/Mitchell Schepps/Desktop/nena')
# # #Load in new data
# pop <- read.csv('UCLAQuarterlyReportMarch2020Population.csv', skip = 1)
# arrests <- read.csv('UCLAQuarterlyReportMarch2020Arrests.csv')
# 
# table(pop$X.6)
# pfs = pop[pop$X.6 == 'Pay for Success',]
# other = pop[pop$X.6 == 'Probation as Usual',]

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
# names(pfs)[1] <- 'id'
# names(other)[1] <- 'id'
# names(arrests)[1] <- 'id'

# #Load in NCC Data
# ncc1 <- read.csv('ncc1_Jan20.csv', skip = 12, header = T)
# ncc2 <- read.csv('ncc2_Jan20.csv', skip = 12, header = T)
# 
# #New ncc
# ncc = read.csv('ncc_420.csv', skip = 9)
# names(ncc)[1] = 'id'
# names(ncc)[8] = 'ncc_date'
# ncc = ncc[, c('id', 'ncc_date')]
# 
# #Merge population dataset and arrest dataset and ncc dataset
pfs_a <- merge(pfs, arrests, by = 'id', all.x = T)
pfs_a <- merge(pfs_a, ncc, by = 'id', all.x = T)
pfs_a$ncc_date <- as.Date(pfs_a$ncc_date, format = '%m/%d/%Y')

other_a <- merge(other, arrests, by ='id', all.x = T)
other_a <- merge(other_a, ncc, by = 'id', all.x = T)
other_a$ncc_date <- as.Date(other_a$ncc_date, format = '%m/%d/%Y')


pfs_pop = colSums(pfs[,c(17:25)])
pfs_clean = colSums(pfs[,c(28:36)])
pfs_a = pfs_pop - pfs_clean
pfs_a <- data.frame(index = c(1:9),
                    arrests = pfs_a)
pfs_pop = data.frame(index = c(1:9),
                  pop = pfs_pop)
ggplot() + geom_line(aes(x = index, y = arrests), data = pfs_a) + geom_line(aes(x = index, y = pop), data = pfs_pop)

other <- pas              
# other <- other[!(other$Intake.Date == ' '), ]
# other <- (other[as.Date(other$Intake.Date, format = '%m/%d/%Y') <= as.Date('03/31/2020', format = '%m/%d/%Y'),])


other_pop = colSums(other[,c(17:25)])
other_clean = colSums(other[,c(28:36)])
other_a = other_pop - other_clean
other_a <- data.frame(index = c(1:9),
                    arrests = other_a)
other_pop = data.frame(index = c(1:9),
                     pop = other_pop)
ggplot() + geom_line(aes(x = index, y = arrests), data = other_a, linetype = 2) + geom_line(aes(x = index, y = pop), data = other_pop, linetype = 2)+ geom_line(aes(x = index, y = arrests), data = pfs_a) + geom_line(aes(x = index, y = pop), data = pfs_pop)

