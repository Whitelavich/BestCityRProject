install.packages("maps")
install.packages("plyr")
install.packages("dplyr")
library(maps)
library(plyr)
library(dplyr)
library(ggplot2)

cities <- as.data.frame(us.cities)
cities <- subset(cities, select = c("name", "lat", "long"))
colnames(cities) <- c("City", "lat", "long")
for (index in 1:nrow(cities)) {
    cities[index, "City"] <- tolower(cities[index, "City"])
    }


`Hospital General Information`$CityState <- paste(`Hospital General Information`$City, `Hospital General Information`$State)
hospitalCount <- count(`Hospital General Information`, CityState)
colnames(hospitalCount) <- c("City", "NumHospitals")

for (index in 1:nrow(hospitalCount)) {
    hospitalCount[index, "City"] <- tolower(hospitalCount[index, "City"])
}

final <- merge(cities, hospitalCount, by = "City")

income <- subset(kaggle_income, select = c("State_ab", "City", "Median"))
income$CityState <- paste(income$City, income$State_ab)
income <- subset(income, select = c("CityState", "Median"))
colnames(income) <- c("City", "Median_Income")

income <- income[!duplicated(income$City),]


for (index in 1:nrow(income)) {
    income[index, "City"] <- tolower(income[index, "City"])
}

final <- merge(final, income, by = "City")
report <- subset(report,report$report_year==2015)
crime <- subset(report, select = c("agency_jurisdiction", "crimes_percapita"))
crime[] <- lapply(crime, gsub, pattern = ',', replacement = '')
colnames(crime) <- c("City", "crime_rate")

for (index in 1:nrow(crime)) {
    crime[index, "City"] <- tolower(crime[index, "City"])
}

final <- merge(final, crime, by = "City")

museum <- subset(museums, select = c("Museum.Name", "City..Administrative.Location.", "State..Administrative.Location."))
museum$CityState <- paste(museum$City..Administrative.Location, museum$State..Administrative.Location)
museum <- subset(museum, select = c("Museum.Name", "CityState"))
colnames(museum) <- c("Name", "City")
for (index in 1:nrow(museum)) {
    museum[index, "City"] <- tolower(museum[index, "City"])
}
museumCount <- count(museum, City)
colnames(museumCount) <- c("City", "Number_Of_Museums")
final <- merge(final, museumCount, by = "City")


CountGraph <- ggplot(data = final, aes(x = reorder(City,-NumHospitals))) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     xlab("City") + ylab("Count") +
     ggtitle("Cities by Hospital and Museum Availability") +
     geom_bar(aes(y = Number_Of_Museums, group = 1, color = "Number of Museums"), stat = "identity") +
     geom_bar(aes(y = NumHospitals, group = 2, color = "Number of Hospitals",fill=NULL), stat = "identity") +
     scale_color_manual(values = c("blue", "red"))
     
     
   

CountGraph


#SO heres my thought if we sort by the different column and then average the indexes of the cities we can produce an overall score
#for each city
