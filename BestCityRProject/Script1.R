install.packages("maps")
install.packages("plyr")
install.packages("dplyr")
library(maps)
library(plyr)
library(dplyr)

cities <- as.data.frame(us.cities)
cities <- subset(cities, select = c("name", "lat", "long"))
colnames(cities) <- c("City", "lat", "long")
for (index in 1:nrow(cities)) {
    cities[index, "City"] <- tolower(cities[index, "City"])
    }


`Hospital General Information`$CityState <- paste(`Hospital General Information`$City, `Hospital General Information`$State)
hospitalCount <- count(`Hospital General Information`, "CityState")
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
