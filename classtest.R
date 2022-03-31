
#Q1
#Reading the csv file
london_crime <- read.csv("london-crime-data.csv", na = "")
london_crime
#Structure of csv file
str(london_crime)
help("paste")
#Concatenating month and year
paste(london_crime$month, london_crime$year, collapse = " / ") 
Date <- paste(london_crime$month, london_crime$year, collapse = " / ") 
london_crime$Date
#Q2
#Renaming the variables

names(london_crime)
names(london_crime)[names(london_crime) == 'borough'] <- 'Borough'
names(london_crime)[names(london_crime) == 'major_category'] <- 'MajorCategory'
names(london_crime)[names(london_crime) == 'minor_category'] <- 'SubCategory'
names(london_crime)[names(london_crime) == 'value'] <- 'Value'
names(london_crime)[names(london_crime) == 'Date'] <- 'CrimeDate'

#Q3
#Date variable
CrimeDate1 <- as.Date(london_crime$CrimeDate, "%m/%d/%y")
CrimeDate1
CrimeDate1 = CrimeDate
london_crime$CrimeDate

#Q4
#Summary
summary(london_crime$borough)
attach(london_crime)
opar <- par(no.readonly = TRUE)
#Plotting


plot(Borough, type = "b", 
     col = "red", 
     lty = 2, pch = 2, 
     lwd = 2, main = "Crime level on Borough ",
     sub = "This graph shows the crime level in borough",
     xlab = "Type of borough",
     ylab = "Crime level",
     
     ylim = c(0, 20))
par(opar)

#Q5
#Converting to numeric
MajorCategory1   <- as.numeric(london_crime$MajorCategory)


help("pie")
#Piechart
pie(MajorCategory1, labels = "major category crimes" ,  edges = 200, radius = 0.8,
    clockwise = FALSE, init.angle = if(clockwise) 90 else 0,
    density = NULL, angle = 45, col = NULL, border = NULL,
    lty = NULL, main = NULL)

#Q6
#Creating region variable
Region <- c("East", "North", "East", "West", "South","North", "South", "West","North",
            "East", "North", "West","North","West", "East","West","West","Central",
            "Central", "East","Central","Central", "South","East", "East","West","Central",
          "South", "Central", "Central", "East","Central")
Borough <- c("Barking and Dagenham","Barnet",
             "Bexley",
             "Brent",
      
             "Bromley",
             "Camden",
             "Croydon",
             "Ealing",
             "Enfield",
             "Greenwich",
             "Hackney",
             "Hammersmith and Fulham Haringey",
             "Haringey",
             "Harrow",
             "Havering",
             "Hillingdon",
             "Hounslow",
             "Islington",
             "Kensington and Chelsea", 
             "Kingston upon Thames", 
             "Lambeth",
             "Lewisham",
             "Merton",
             "Newham",
             "Redbridge",
             "Richmond upon Thames",
             "Southwark",
             "Sutton",
             "Tower Hamlets",
             "Waltham Forest",
             "Wandsworth", 
             "Westminster")

#Creating a newdataframe with region and borough
new_data <- data.frame(Region, Borough)

#Merging two data framnes by Creating a newdataframe where each borough 
# is assigned a region
total <- merge(london_crime, new_data, by="Borough")

sum(!complete.cases(total$Borough, total$Region))
#There are zero null values in regions assigned to borough
dim(total)
#7
#Plotting regions
attach(total)
opar <- par(no.readonly = TRUE)
plot(Region, Value, type = "b", 
     col = "red", 
     lty = 2, pch = 2, 
     lwd = 2, main = " Regionwise crime rate ",
     sub = "Regionwisecrime rate",
     xlab = "Region",
     ylab = "Crime")
par(opar)

#Central region has the highest number of crimes
#West region has the lowest number of crimes

#8
new_data1 <- subset(total, Region =="West" | Region == "Central")
new_data1
#Violence against other person is the highest reported crime in Central and Theft and 
#Handling is the highest reported crime in West

#9
summary(total)
attach(total)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))
#Plotting
plot(Region,
     type = "b", 
     col = "red", 
     lty = 2, pch = 2, 
     lwd = 2, main = " Crime rate in Region",
     sub = "R",
     xlab = "Region",
     ylab = "Crime")
plot(Borough,
     type = "b", 
     col = "red", 
     lty = 2, pch = 2, 
     lwd = 2, main = " Crime rate in Borough",
     sub = "R",
     xlab = "Borough",
     ylab = "Crime")
plot(MajorCategory,
     type = "b", 
     col = "red", 
     lty = 2, pch = 2, 
     lwd = 2, main = " Crime rate",
     sub = "R",
     xlab = "MajorCategory",
     ylab = "Crime")

par(opar)

#10
#Writecsv
write.csv(total, file = "London-crime-modified.csv")

















     
     






