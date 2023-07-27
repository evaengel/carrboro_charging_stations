library(lubridate)
library(ggplot2)
#read csv into R
charging_data_original <- read.csv("carrboro_charging_data.csv")
#select variables to be in new dataframe
charging_data <- subset(charging_data_original, select = c(1, 4:7, 10:13, 15, 18, 20:23, 28))
#find indices of rows with charging across EDT/EST
which(charging_data[,3] == "EDT" & charging_data[,5] == "EST")
which(charging_data[,3] == "EST" & charging_data[,5] == "EDT")
#remove rows with charging across EDT/EST
#TELL CARRBORO COUNCIL I REMOVED ROWS WITH SWITCHES ACROSS EDT/EST (ONLY REMOVED 6 ROWS OUT OF 13172)
charging_data <- charging_data[-c(164,166,169,2100,2101,9592),]
#check for missing data 
which(is.na(charging_data))
which(c$Station.Name == "")
#remove two rows with missing data for end date 
c <- charging_data[!(charging_data$End.Date == ""),]
#remove 19 rows with missing data for Ended.By
d <- c[!(c$Ended.By == ""),]
#replace charging_data with d
charging_data <- d
#find number of rows (result: 13145 rows)
nrow(charging_data)

#find time differences 
#earliest start date in dataset is August 30, 2018
#I checked; the newly-calculated time differences are correct 
plug.in.time <- difftime(strptime(charging_data$End.Date, format = "%m/%d/%y %H:%M"), strptime(charging_data$Start.Date, format = "%m/%d/%y %H:%M"), units = "mins")
charging_data2 <- cbind(charging_data, plug.in.time)
sum(as.numeric(plug.in.time))

#calculate charging time
#I checked; the newly-calculated charging times are correct 
charging_time <- as.difftime(charging_data$Charging.Time..hh.mm.ss., units = "mins")
sum(as.numeric(charging_time))

#calculate idle time
#I checked; the newly-calculated idle times are correct 
num1 <- as.numeric(charging_data2$plug.in.time)
num2 <- as.numeric(charging_time)
idle_time <- num1 - num2
#turn all negative idle_times (charging ended before vehicle was fully charged) to 0
idle_time[idle_time < 0] <- 0
#round idle time to nearest minute
idle_time2 <- round(idle_time, digits = 0)
#add idle_time to table
charging_data3 <- cbind(charging_data2, idle_time2)
sum(as.numeric(idle_time2))
#histogram of idle time
hist(charging_data3$idle_time2, breaks = seq(0,3645,by = 15), main = "Idle Time Frequencies", xlim = c(0, 150), xlab = "Idle Time (up to 150 min)", ylab = "Number of Occurences")
hist(charging_data3$idle_time2, breaks = seq(0,3645,by = 15), main = "Idle Time Frequencies", ylim = c(0, 100), xlab = "Idle Time (minutes)", ylab = "Number of Occurrences")
#find percent of plug-in time across dataset that is idle time (27.97%)
sum(as.numeric(charging_data8$idle_time2))/sum(as.numeric(charging_data8$plug.in.time))
#find % of people idling <15 minutes
length(which(idle_time2 < 15))/length(idle_time2)
#find % of people idling <30 minutes
length(which(idle_time2 < 30))/length(idle_time2)
#find % of people idling <60 minutes
length(which(idle_time2 < 60))/length(idle_time2)
#find % of people idling >120 minutes
length(which(idle_time2 > 120))/length(idle_time2)
#find % of people idling >180 minutes
length(which(idle_time2 > 180))/length(idle_time2)
#find % of people idling >300 minutes
length(which(idle_time2 > 300))/length(idle_time2)
#find % of people idling >720 minutes
length(which(idle_time2 > 720))/length(idle_time2)
#find % of people idling >1440 minutes
length(which(idle_time2 > 1440))/length(idle_time2)
#create datatable of idle times over 15 minutes
idle_15 <- idle_time2[which(idle_time2 > 15)]
(sum(idle_15) - (15*length(idle_15)))*0.025 


##remove bottom 5% and/or top 5% of observations from dataset
quantiles <- quantile(charging_data3$idle_time2, c(0.05, 0.95))
remove_bottom <- charging_data3$idle_time2[which(charging_data3$idle_time2 > quantiles[1])]
remove_top <- charging_data3$idle_time2[which(charging_data3$idle_time2 < quantiles[2])]
remove_bottom_and_top <- charging_data3$idle_time2[which(charging_data3$idle_time2 < quantiles[2] & charging_data3$idle_time2 > quantiles[1])]
mean(remove_top)
median(remove_top)


##calculate descriptive statistics for different variables
summary(num2)
hist(num2)

##find number of observations for each charging station 
#rename "TOWN HALL EV / CARRBORO" and "TOC EV STATIONS / CARRBORO" to "TOC EV STATIONS / TOWNHALL"
charging_data3$Station.Name <- gsub("TOWN HALL EV / CARRBORO","TOC EV STATIONS / TOWNHALL", charging_data3$Station.Name)
charging_data3$Station.Name <- gsub("TOC EV STATIONS / CARRBORO","TOC EV STATIONS / TOWNHALL", charging_data3$Station.Name)
#calculate number of uses of each station
y <- as.data.frame(charging_data3$Station.Name, stringsAsFactors = TRUE)
summary(y)

##turn start.date from date format to day format
library(lubridate)
#turns End.Date variable into what number day of year the end of plug in was
end_day <- yday(strptime(charging_data3$End.Date, format = "%m/%d/%y %H:%M"))
charging_data4 <- cbind(charging_data3, end_day)
#make histogram of end_day
summary(charging_data4$end_day)
hist(charging_data4$end_day)
#calculate number of each type of ending of charging
x <- as.data.frame(charging_data4$Ended.By, stringsAsFactors = TRUE)
summary(x)

##idle time summary code
#calculate the number of occurences of idle times at/above a certain amount
length(which(charging_data4$idle_time2 == 0))
length(which(charging_data4$idle_time2 > 30))
length(which(charging_data4$idle_time2 > 60))
length(which(charging_data4$idle_time2 > 120))
length(which(charging_data4$idle_time2 > 240))
length(which(charging_data4$idle_time2 > 300))
length(which(charging_data4$idle_time2 > 480))
length(which(charging_data4$idle_time2 > 720))
length(which(charging_data4$idle_time2 > 1440))
#finds hour of day of end date
hour(strptime(charging_data4$End.Date[1], format = "%m/%d/%y %H:%M"))
##find average idle time for each month
#find month that charging ends 
end_month <- months(strptime(charging_data4$End.Date, format = "%m/%d/%y %H:%M"), abbreviate = TRUE)
charging_data5 <- cbind(charging_data4, end_month)
##find average idle time for each month
month_data <- data.frame(matrix(ncol = 2, nrow = 12))
colnames(month_data) <- c("month","avg_idle_time")
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_data[,1] <- months
month_data[1,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Jan"])
month_data[2,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Feb"])
month_data[3,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Mar"])
month_data[4,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Apr"])
month_data[5,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "May"])
month_data[6,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Jun"])
month_data[7,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Jul"])
month_data[8,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Aug"])
month_data[9,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Sep"])
month_data[10,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Oct"])
month_data[11,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Nov"])
month_data[12,2] <- mean(charging_data5$idle_time2[charging_data5$end_month == "Dec"])
write.csv(month_data, "avg_idle_time_by_month.csv")
#plot histogram for average idle time by month
##MAKE THE PLOT PRETTIER COLORS
ggplot(month_data,aes(x = factor(month, level = months), avg_idle_time))+geom_bar(stat = "identity")+ theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Month", y = "Mean Idle Time (minutes)") + ggtitle("Mean Idle Time by Month (2017-2023)")

#calculate number of charger uses by month
month_data[1,3] <- length(which(charging_data8$end_month == "Jan"))
month_data[2,3] <- length(which(charging_data8$end_month == "Feb"))
month_data[3,3] <- length(which(charging_data8$end_month == "Mar"))
month_data[4,3] <- length(which(charging_data8$end_month == "Apr"))
month_data[5,3] <- length(which(charging_data8$end_month == "May"))
month_data[6,3] <- length(which(charging_data8$end_month == "Jun"))
month_data[7,3] <- length(which(charging_data8$end_month == "Jul"))
month_data[8,3] <- length(which(charging_data8$end_month == "Aug"))
month_data[9,3] <- length(which(charging_data8$end_month == "Sep"))
month_data[10,3] <- length(which(charging_data8$end_month == "Oct"))
month_data[11,3] <- length(which(charging_data8$end_month == "Nov"))
month_data[12,3] <- length(which(charging_data8$end_month == "Dec"))
colnames(month_data) <- c("month","avg_idle_time","charger_usage")
write.csv(month_data, "month_data.csv")
#histogram of charger usage by month 
ggplot(month_data,aes(x = factor(month, level = months), charger_usage))+geom_bar(stat = "identity") + ggtitle("Charger Usage by Month") + labs(y = "Number of Uses", x = "Month")


#finds year that charging ends
end_year <- year(strptime(charging_data5$End.Date, format = "%m/%d/%y %H:%M"))
charging_data6 <- cbind(charging_data5, end_year)
##find average idle time for each year
year_data <- data.frame(matrix(ncol = 2, nrow = 7))
colnames(year_data) <- c("year","avg_idle_time")
years <- c("2017","2018","2019","2020","2021","2022","2023")
year_data[,1] <- years
year_data[1,2] <- mean(charging_data6$idle_time2[charging_data6$end_year == "2017"])
year_data[2,2] <- mean(charging_data6$idle_time2[charging_data6$end_year == "2018"])
year_data[3,2] <- mean(charging_data6$idle_time2[charging_data6$end_year == "2019"])
year_data[4,2] <- mean(charging_data6$idle_time2[charging_data6$end_year == "2020"])
year_data[5,2] <- mean(charging_data6$idle_time2[charging_data6$end_year == "2021"])
year_data[6,2] <- mean(charging_data6$idle_time2[charging_data6$end_year == "2022"])
year_data[7,2] <- mean(charging_data6$idle_time2[charging_data6$end_year == "2023"])
write.csv(year_data, "avg_idle_time_by_year.csv")
#plot histogram for average idle time by year
##MAKE THE PLOT PRETTIER COLORS
ggplot(year_data,aes(x = factor(year, level = years), avg_idle_time))+geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Year", y = "Mean Idle Time (minutes)") + ggtitle("Mean Idle Time by Year") + coord_cartesian(ylim = c(0,50))

###NOT USED
#finds hour that charging ends
end_hour <- hour(strptime(charging_data6$End.Date, format = "%m/%d/%y %H:%M"))
charging_data7 <- cbind(charging_data6, end_hour)
##find average idle time for each hour
hour_data <- data.frame(matrix(ncol = 2, nrow = 24))
colnames(hour_data) <- c("hour","avg_idle_time")
hours <- c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
hour_data[,1] <- hours
hour_data[1,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "0"])
hour_data[2,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "1"])
hour_data[3,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "2"])
hour_data[4,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "3"])
hour_data[5,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "4"])
hour_data[6,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "5"])
hour_data[7,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "6"])
hour_data[8,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "7"])
hour_data[9,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "8"])
hour_data[10,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "9"])
hour_data[11,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "10"])
hour_data[12,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "11"])
hour_data[13,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "12"])
hour_data[14,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "13"])
hour_data[15,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "14"])
hour_data[16,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "15"])
hour_data[17,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "16"])
hour_data[18,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "17"])
hour_data[19,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "18"])
hour_data[20,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "19"])
hour_data[21,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "20"])
hour_data[22,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "21"])
hour_data[23,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "22"])
hour_data[24,2] <- mean(charging_data7$idle_time2[charging_data7$end_hour == "23"])
write.csv(hour_data, "avg_idle_time_by_hour.csv")

#finds when idle time begins
#mark cars that never are idle as beginning to be idle when they take the plug out of their car
idle_start <- strptime(charging_data7$End.Date, format = "%m/%d/%y %H:%M") - as.difftime(charging_data7$idle_time2, units = "mins")
idle_period <- interval(idle_start,strptime(charging_data7$End.Date, format = "%m/%d/%y %H:%M"))
charging_data8 <- cbind(charging_data7, idle_period)
#can't figure out how to see if a certain hour is within an idle_period

#finds day of week that charging ends
end_dayw <- wday(strptime(charging_data7$End.Date, format = "%m/%d/%y %H:%M"), label = TRUE)
charging_data8 <- cbind(charging_data7, end_dayw)
##find average idle time for each day
day_data <- data.frame(matrix(ncol = 2, nrow = 7))
colnames(day_data) <- c("dayw","avg_idle_time")
daysw <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
day_data[,1] <- daysw
day_data[1,2] <- mean(charging_data8$idle_time2[charging_data8$end_dayw == "Sun"])
day_data[2,2] <- mean(charging_data8$idle_time2[charging_data8$end_dayw == "Mon"])
day_data[3,2] <- mean(charging_data8$idle_time2[charging_data8$end_dayw == "Tue"])
day_data[4,2] <- mean(charging_data8$idle_time2[charging_data8$end_dayw == "Wed"])
day_data[5,2] <- mean(charging_data8$idle_time2[charging_data8$end_dayw == "Thu"])
day_data[6,2] <- mean(charging_data8$idle_time2[charging_data8$end_dayw == "Fri"])
day_data[7,2] <- mean(charging_data8$idle_time2[charging_data8$end_dayw == "Sat"])
write.csv(day_data, "avg_idle_time_by_day_of_week.csv")
#plot histogram for average idle time by day
##MAKE THE PLOT PRETTIER COLORS
ggplot(day_data,aes(x = factor(dayw, level = daysw), avg_idle_time))+geom_bar(stat = "identity")+ theme(plot.title = element_text(hjust = 0.5)) + labs(x = "Day of Week", y = "Mean Idle Time (minutes)") + ggtitle("Mean Idle Time by Day of Week (2017-2023)")

##working with idle time lengths
#create dataframe for idle times over 8 hrs
hourse <- charging_data4[which(charging_data4$idle_time2 > 480),]
#find how many involved the car being plugged in overnight (result: 335 occurences)
length(which(yday(strptime(hourse$Start.Date, format = "%m/%d/%y %H:%M")) != yday(strptime(hourse$End.Date, format = "%m/%d/%y %H:%M"))))

#create dataframe for idle times over 5 hrs
hourse1 <- charging_data4[which(charging_data4$idle_time2 > 300),]
#find how many involved the car being plugged in overnight (result: 636 occurences)
length(which(yday(strptime(hourse1$Start.Date, format = "%m/%d/%y %H:%M")) != yday(strptime(hourse1$End.Date, format = "%m/%d/%y %H:%M"))))

#create dataframe for idle times over 4 hrs
hourse2 <- charging_data4[which(charging_data4$idle_time2 > 240),]
#find how many involved the car being plugged in overnight (result: 716 occurences)
length(which(yday(strptime(hourse2$Start.Date, format = "%m/%d/%y %H:%M")) != yday(strptime(hourse2$End.Date, format = "%m/%d/%y %H:%M"))))

#create dataframe for idle times over 2 hrs
hourse3 <- charging_data4[which(charging_data4$idle_time2 > 120),]
#find how many involved the car being plugged in overnight (result: 822 occurences)
length(which(yday(strptime(hourse3$Start.Date, format = "%m/%d/%y %H:%M")) != yday(strptime(hourse3$End.Date, format = "%m/%d/%y %H:%M"))))

##miscellaneous idle time calculations
#create object of only idle times > 0
idle_time_only <- idle_time2[which(idle_time2 > 0)]
#find % of idling that is longer than 15 min (45.32915%)
length(which(idle_time_only > 15))/length(idle_time_only)
#find % of idling that is longer than 1 hr (26.69592%)
length(which(idle_time_only > 60))/length(idle_time_only)
#find % of idling that is longer than 2 hr (17.89342%)
length(which(idle_time_only > 120))/length(idle_time_only)
#find % of idling that is longer than 5 hr (2.219436%)
length(which(idle_time_only > 600))/length(idle_time_only)

##graph idle time by station
#mlk park
mlk <- charging_data8[which(charging_data8$Station.Name == "TOC EV STATIONS / MLKPARK"),]
hist(mlk$idle_time2, breaks = seq(0,1395,by = 15), main = "Idle Time Frequencies - MLK Park", ylim = c(0, 100), xlab = "Idle Time (minutes)", ylab = "Number of Occurrences")
hist(mlk$idle_time2, breaks = seq(0,1395,by = 15), main = "Idle Time Frequencies - MLK Park", xlab = "Idle Time", ylab = "Number of Occurences")
hist(mlk$idle_time2, main = "Idle Time Frequencies - MLK Park", xlim = c(1, 1390), ylim = c(0,100), xlab = "Idle Time (minutes)", ylab = "Number of Occurrences")
hist(mlk$idle_time2, breaks = seq(0,1410,by = 30), main = "Idle Time Frequencies - MLK Park", xlim = c(30, 1410), ylim = c(0, 50), xlab = "Idle Time (minutes)", ylab = "Number of Occurrences")
mean(mlk$idle_time2)
median(mlk$idle_time2)
#town hall
townhall <- charging_data8[which(charging_data8$Station.Name == "TOC EV STATIONS / TOWNHALL"),]
hist(townhall$idle_time2, breaks = seq(0,1995,by = 15), main = "Idle Time Frequencies - Town Hall", ylim = c(0, 100), xlab = "Idle Time (minutes)", ylab = "Number of Occurrences")
hist(townhall$idle_time2, breaks = seq(0,1995,by = 15), main = "Idle Time Frequencies - Town Hall", xlab = "Idle Time", ylab = "Number of Occurences")
hist(townhall$idle_time2, breaks = seq(0,2010,by = 30), main = "Idle Time Frequencies - Town Hall", xlim = c(30, 2010), ylim = c(0,50), xlab = "Idle Time (minutes)", ylab = "Number of Occurences")
mean(townhall$idle_time2)
median(townhall$idle_time2)
#rosemary street
rosemary <- charging_data8[which(charging_data8$Station.Name == "TOC EV STATIONS / 604ROSEMARYPARK"),]
hist(rosemary$idle_time2, breaks = seq(0,3645,by = 15), main = "Idle Time Frequencies - 604 Rosemary", ylim = c(0, 100), xlab = "Idle Time (minutes)", ylab = "Number of Occurrences")
hist(rosemary$idle_time2, breaks = seq(0,3645,by = 15), main = "Idle Time Frequencies - 604 Rosemary", xlab = "Idle Time", ylab = "Number of Occurences")
hist(rosemary$idle_time2, breaks = seq(0,3660,by = 30), main = "Idle Time Frequencies - 604 Rosemary", xlim = c(0, 3660), ylim = c(0,50), xlab = "Idle Time (minutes)", ylab = "Number of Occurences")
mean(rosemary$idle_time2)
median(rosemary$idle_time2)

###graph charger usage by station
##mlk
month_data_mlk <- data.frame(matrix(ncol = 3, nrow = 12))
colnames(month_data_mlk) <- c("month","avg_idle_time","charger_usage")
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_data_mlk[,1] <- months
#mean idle time by month
month_data_mlk[1,2] <- mean(mlk$idle_time2[mlk$end_month == "Jan"])
month_data_mlk[2,2] <- mean(mlk$idle_time2[mlk$end_month == "Feb"])
month_data_mlk[3,2] <- mean(mlk$idle_time2[mlk$end_month == "Mar"])
month_data_mlk[4,2] <- mean(mlk$idle_time2[mlk$end_month == "Apr"])
month_data_mlk[5,2] <- mean(mlk$idle_time2[mlk$end_month == "May"])
month_data_mlk[6,2] <- mean(mlk$idle_time2[mlk$end_month == "Jun"])
month_data_mlk[7,2] <- mean(mlk$idle_time2[mlk$end_month == "Jul"])
month_data_mlk[8,2] <- mean(mlk$idle_time2[mlk$end_month == "Aug"])
month_data_mlk[9,2] <- mean(mlk$idle_time2[mlk$end_month == "Sep"])
month_data_mlk[10,2] <- mean(mlk$idle_time2[mlk$end_month == "Oct"])
month_data_mlk[11,2] <- mean(mlk$idle_time2[mlk$end_month == "Nov"])
month_data_mlk[12,2] <- mean(mlk$idle_time2[mlk$end_month == "Dec"])
#calculate number of charger uses by month
month_data_mlk[1,3] <- length(which(mlk$end_month == "Jan"))
month_data_mlk[2,3] <- length(which(mlk$end_month == "Feb"))
month_data_mlk[3,3] <- length(which(mlk$end_month == "Mar"))
month_data_mlk[4,3] <- length(which(mlk$end_month == "Apr"))
month_data_mlk[5,3] <- length(which(mlk$end_month == "May"))
month_data_mlk[6,3] <- length(which(mlk$end_month == "Jun"))
month_data_mlk[7,3] <- length(which(mlk$end_month == "Jul"))
month_data_mlk[8,3] <- length(which(mlk$end_month == "Aug"))
month_data_mlk[9,3] <- length(which(mlk$end_month == "Sep"))
month_data_mlk[10,3] <- length(which(mlk$end_month == "Oct"))
month_data_mlk[11,3] <- length(which(mlk$end_month == "Nov"))
month_data_mlk[12,3] <- length(which(mlk$end_month == "Dec"))
write.csv(month_data_mlk, "month_data_mlk.csv")
#histogram of charger usage by month 
ggplot(month_data_mlk,aes(x = factor(month, level = months), charger_usage))+geom_bar(stat = "identity") + ggtitle("Charger Usage by Month - MLK Park") + labs(y = "Number of Uses", x = "Month")
##town hall
month_data_townhall <- data.frame(matrix(ncol = 3, nrow = 12))
colnames(month_data_townhall) <- c("month","avg_idle_time","charger_usage")
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_data_townhall[,1] <- months
#mean idle time by month
month_data_townhall[1,2] <- mean(townhall$idle_time2[townhall$end_month == "Jan"])
month_data_townhall[2,2] <- mean(townhall$idle_time2[townhall$end_month == "Feb"])
month_data_townhall[3,2] <- mean(townhall$idle_time2[townhall$end_month == "Mar"])
month_data_townhall[4,2] <- mean(townhall$idle_time2[townhall$end_month == "Apr"])
month_data_townhall[5,2] <- mean(townhall$idle_time2[townhall$end_month == "May"])
month_data_townhall[6,2] <- mean(townhall$idle_time2[townhall$end_month == "Jun"])
month_data_townhall[7,2] <- mean(townhall$idle_time2[townhall$end_month == "Jul"])
month_data_townhall[8,2] <- mean(townhall$idle_time2[townhall$end_month == "Aug"])
month_data_townhall[9,2] <- mean(townhall$idle_time2[townhall$end_month == "Sep"])
month_data_townhall[10,2] <- mean(townhall$idle_time2[townhall$end_month == "Oct"])
month_data_townhall[11,2] <- mean(townhall$idle_time2[townhall$end_month == "Nov"])
month_data_townhall[12,2] <- mean(townhall$idle_time2[townhall$end_month == "Dec"])
#calculate number of charger uses by month
month_data_townhall[1,3] <- length(which(townhall$end_month == "Jan"))
month_data_townhall[2,3] <- length(which(townhall$end_month == "Feb"))
month_data_townhall[3,3] <- length(which(townhall$end_month == "Mar"))
month_data_townhall[4,3] <- length(which(townhall$end_month == "Apr"))
month_data_townhall[5,3] <- length(which(townhall$end_month == "May"))
month_data_townhall[6,3] <- length(which(townhall$end_month == "Jun"))
month_data_townhall[7,3] <- length(which(townhall$end_month == "Jul"))
month_data_townhall[8,3] <- length(which(townhall$end_month == "Aug"))
month_data_townhall[9,3] <- length(which(townhall$end_month == "Sep"))
month_data_townhall[10,3] <- length(which(townhall$end_month == "Oct"))
month_data_townhall[11,3] <- length(which(townhall$end_month == "Nov"))
month_data_townhall[12,3] <- length(which(townhall$end_month == "Dec"))
write.csv(month_data_townhall, "month_data_townhall.csv")
#histogram of charger usage by month 
ggplot(month_data_townhall,aes(x = factor(month, level = months), charger_usage))+geom_bar(stat = "identity") + ggtitle("Charger Usage by Month - Town Hall") + labs(y = "Number of Uses", x = "Month")
##rosemary
month_data_rosemary <- data.frame(matrix(ncol = 3, nrow = 12))
colnames(month_data_rosemary) <- c("month","avg_idle_time","charger_usage")
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_data_rosemary[,1] <- months
#mean idle time by month
month_data_rosemary[1,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Jan"])
month_data_rosemary[2,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Feb"])
month_data_rosemary[3,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Mar"])
month_data_rosemary[4,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Apr"])
month_data_rosemary[5,2] <- mean(rosemary$idle_time2[rosemary$end_month == "May"])
month_data_rosemary[6,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Jun"])
month_data_rosemary[7,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Jul"])
month_data_rosemary[8,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Aug"])
month_data_rosemary[9,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Sep"])
month_data_rosemary[10,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Oct"])
month_data_rosemary[11,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Nov"])
month_data_rosemary[12,2] <- mean(rosemary$idle_time2[rosemary$end_month == "Dec"])
#calculate number of charger uses by month
month_data_rosemary[1,3] <- length(which(rosemary$end_month == "Jan"))
month_data_rosemary[2,3] <- length(which(rosemary$end_month == "Feb"))
month_data_rosemary[3,3] <- length(which(rosemary$end_month == "Mar"))
month_data_rosemary[4,3] <- length(which(rosemary$end_month == "Apr"))
month_data_rosemary[5,3] <- length(which(rosemary$end_month == "May"))
month_data_rosemary[6,3] <- length(which(rosemary$end_month == "Jun"))
month_data_rosemary[7,3] <- length(which(rosemary$end_month == "Jul"))
month_data_rosemary[8,3] <- length(which(rosemary$end_month == "Aug"))
month_data_rosemary[9,3] <- length(which(rosemary$end_month == "Sep"))
month_data_rosemary[10,3] <- length(which(rosemary$end_month == "Oct"))
month_data_rosemary[11,3] <- length(which(rosemary$end_month == "Nov"))
month_data_rosemary[12,3] <- length(which(rosemary$end_month == "Dec"))
write.csv(month_data_rosemary, "month_data_rosemary.csv")
#histogram of charger usage by month 
ggplot(month_data_rosemary,aes(x = factor(month, level = months), charger_usage))+geom_bar(stat = "identity") + ggtitle("Charger Usage by Month - 604 Rosemary") + labs(y = "Number of Uses", x = "Month")

##working with greenhouse gas savings
#calculating length of charging time in minutes
charging_time1 <- hms(charging_data4$Charging.Time..hh.mm.ss.)
charging_time2 <- (period_to_seconds(charging_time1))/60
#find how many kg of greenhouse gases are saved with 1 minute of charging
ghgrate <- (charging_data4$GHG.Savings..kg.)/charging_time2
#find mean ghg savings rate (result is 0.05304402 kg/minute)
mean1 <- mean(ghgrate, na.rm = TRUE)
#find total number of minutes of idle time across entire dataset (result is 630666 minutes)
sum(charging_data4$idle_time2)



