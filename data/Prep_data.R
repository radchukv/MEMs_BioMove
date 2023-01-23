#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                 Iris Pretzlaff dormouse data                         #
#                                 data preparation                                     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(data.table)
library(lattice)

## 1. data on metabolic rate in both years
data1 <- read.csv(file = here::here('data', 'Iris_1999_17_03_2017_25.4.17.csv'), na.strings = -9999.00)
str(data1)
data1$Tamb <- as.numeric(as.character(data1$Ta_iBut))
data1$Ta_iBut <- data1$Tamb
data1$Tamb <- NULL
data2 <- read.csv(file = here::here('data', 'MR_Haselmaus_2011_12_25.4.17.csv'), na.strings = c(' ', '-9999.0', '-9999', NA))
str(data2)

data1$Date <- as.character(data1$Date)
data1$Time <- as.character(data1$Time)
data1$date_and_time <- as.character(data1$date_and_time)
data1$Note_MR <- as.character(data1$Note_MR)
data1$Note_Temp <- as.character(data1$Note_Temp)
data1$ID_individual <- as.character(data1$ID_individual)

data2$Date <- as.character(data2$Date)
data2$Time <- as.character(data2$Time)
data2$date_and_time <- as.character(data2$date_and_time)
data2$Note_MR <- as.character(data2$Note_MR)
data2$Note_Temp <- as.character(data2$Note_Temp)
data2$ID_individual <- as.character(data2$ID_individual)

data <- rbind(data1, data2)
str(data)
unique(data$ID_individual)


## adding day/time, so that we can calculate the time passed since hibernation
data$check <- strptime(paste(paste(data$Year, data$Month, data$Day, sep = '/'), 
                             paste(data$Hour, data$Minute, sep = ':'), sep = " "), format = 
                         '%Y/%m/%d %H:%M')

data$ID <- substr(data$ID_individual, start=4, stop = nchar(as.character(data$ID_individual)))


Data.T <- data.table(subset(data, select = -check))
dates <- IDateTime(data$check)
data.T <- cbind(Data.T, dates)
setkeyv(data.T, c('ID', 'idate', 'itime'))

df <- data.frame(data.T)
df$time.conv <- as.POSIXct(paste(df$idate, df$itime, sep = ' '), format = '%Y-%m-%d %H:%M:%S')

### check for the min date to use as the start of hibern
check <- df %>%
  group_by(ID, Year) %>%
  summarize(., MinDate = min(idate))

df$beg_date <- as.POSIXct('2009-10-16 00:00:00', format = '%Y-%m-%d %H:%M:%S')
df$beg_date[df$Year %in% c(2011, 2012)] <- as.POSIXct('2011-10-12 00:00:00', format = '%Y-%m-%d %H:%M:%S')
df$HoursBegin <- as.numeric((df$time.conv - df$beg_date))

df$HoursBegin2 <- df$HoursBegin^2
### adding time per ID (for autocor) 
for (i in 1:length(unique(df$ID))){
  df$Timecon[df$ID == unique(df$ID)[i]] <- c(1:nrow(df[df$ID == unique(df$ID)[i], ])) 
}

### temperature is coded as factor
df$Tamb <- df$Ta_iBut
df$Date <- as.Date(paste0(df$Year, '-', df$Month, '-', df$Day), format = '%Y-%m-%d')

### adding a var for the state (levels: 1 for aroused, 0 for torpor) based on energetic_state
df$state <- rep(0, nrow(df))
df$state[df$energetic_state %in% c(-1,1)] <- 1
check <- df[df$Note_MR %in% c('aus Kasten raus', 'Baseline nicht o.k.', 'raus',   'kein Wert','shallow torpor', 'shallow torpor?',
                              'am Ende Messung nicht o.k.', 'baseline nicht o.k.','basline nicht o.k.',   'kein Wert', 'um arousal',
                              'baseline teilw. Negativ', 'Datenlage zu gering', 'kurz raus + rein',
                              'raus + rein, unklar Datenlage', 'rein + raus', 'zu wenig Daten, Null-Linie nicht o.k.'), ]     #''um arousal', 'shallow torpor',
df <- df[! (df$Note_MR %in% c('aus Kasten raus', 'Baseline nicht o.k.', 'raus', 
                              'raus + rein', 'rein', 'verl_sst Kasten', 'rau + rein',     'shallow torpor', 'shallow torpor?',
                              'am Ende Messung nicht o.k.', 'baseline nicht o.k.', 'basline nicht o.k.', 'kein Wert',   'um arousal',
                              'baseline teilw. Negativ', 'Datenlage zu gering', 'kurz raus + rein',   
                              'raus + rein, unklar Datenlage', 'rein + raus', 'zu wenig Daten, Null-Linie nicht o.k.')), ]  #  'um arousal', 'shallow torpor',


## save the dataset
save(df, file = './data/df.Rda')

## Data on body mass
bM <- read.csv(file = here::here('data', 'BMass_25.4.17.csv'))
table(bM$Geschlecht)
unique(bM$Individual)
bM$ID <- bM$Individual
bM$Date <- strptime(bM$Datum, format = '%d/%m/%y')
bM$dates <- IDateTime(bM$Date)
bM$date <- bM$dates[, 1]
levels(bM$Geschlecht) <-  c('m', 'f')

table(bM$Individual)

hist(bM$Body_mass)
# pdf('BodyMass_byDate_perID_twoYears.pdf')
xyplot(Body_mass ~ date|ID, data = bM)
# dev.off()

# pdf('BodyMass_bySex.pdf')
boxplot(Body_mass ~ Geschlecht, data = bM)   ## F lighter than M
# dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####                          Body mass predictors previous month                           ####
df$ID <- factor(df$ID)
nrow(df[df$ID == '14A' , ])   ## & df$Date > as.Date('2010-2-16', format = '%Y-%m-%d')
## subset bM for the dates > "2009-11-20" - otherwise wouldn't work for 1 month averages
#bM_limited <- bM[bM$Date  > strptime('2009/11/20', format = '%Y/%m/%d'), ]

bM_vars <- data.frame(date = numeric(), ID = numeric(), Sex = numeric(), BMass = numeric(),
                      MRmean= numeric(),  Occur_arouse = numeric())

for (i in levels(bM$ID)){
  sub <- bM[bM$ID == i, ]
  for(j in 1:nrow(sub)){
    DMin <- sub$Date[j] - days(30)
    Vars <- df[df$ID == i & df$Date < sub$Date[j] & df$Date > DMin, ]
    MRmean <- mean(Vars$MR_h, na.rm = T)
    #Freq <- mean(Vars$state, na.rm = T)  
    Occur_arouse <- sum(Vars$state, na.rm = T)
    
    dat <- data.frame(date = sub$Date[j], ID = i, Sex = sub$Geschlecht[j], BMass = sub$Body_mass[j],
                      MRmean = MRmean, Occur_arouse = Occur_arouse)
    bM_vars <- rbind(bM_vars, dat)
  }
  
}

bM_vars
bM_Vars <- na.omit(bM_vars)
nrow(bM_Vars)  ## 155

bM_Vars$year <- year(bM_Vars$date)

#hist(bM_Vars$MRmean)
#hist(log(bM_Vars$MRmean), breaks = 5)  ### better, but not excellent....
bM_Vars$logMR <- log(bM_Vars$MRmean)


#hist(bM_Vars$Occur_arouse)
#hist(log(bM_Vars$Occur_arouse))  ## OK
#bM_Vars$logTotArouse <- log(bM_Vars$Occur_arouse)
#bM_Vars <- bM_Vars[is.finite(bM_Vars$logTotArouse), ]
nrow(bM_Vars)   ## 155

#hist(bM_Vars$BMass)  ##OK

## save the dataset
save(bM_Vars, file = './data/bM_Vars.rda')
