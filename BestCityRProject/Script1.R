install.packages("maps")
install.packages("plyr")
install.packages("dplyr")
library(maps)
library(plyr)
library(dplyr)
library(ggplot2)
library(htmltab)


#Load list of us cities
cities <- as.data.frame(us.cities)
cities <- subset(cities, select = c("name", "lat", "long"))
colnames(cities) <- c("City", "lat", "long")
#clean-up cities
for (index in 1:nrow(cities)) {
    cities[index, "City"] <- tolower(cities[index, "City"])
    }

#Hospital Source
`Hospital General Information`$CityState <- paste(`Hospital General Information`$City, `Hospital General Information`$State)
#extract number ofhospitals per city
hospitalCount <- count(`Hospital General Information`, CityState)
#hospital clean-up
colnames(hospitalCount) <- c("City", "NumHospitals")
for (index in 1:nrow(hospitalCount)) {
    hospitalCount[index, "City"] <- tolower(hospitalCount[index, "City"])
}
#Merge hapiness with city list to create final table
final <- merge(cities, hospitalCount, by = "City")

#Income source
income <- subset(kaggle_income, select = c("State_ab", "City", "Median"))
#income clean-up
income$CityState <- paste(income$City, income$State_ab)
income <- subset(income, select = c("CityState", "Median"))
colnames(income) <- c("City", "Median_Income")
#remove duplicated cities
income <- income[!duplicated(income$City),]
#income clean-up
for (index in 1:nrow(income)) {
    income[index, "City"] <- tolower(income[index, "City"])
}
#merge with final table
final <- merge(final, income, by = "City")
#crime source
report <- subset(report, report$report_year == 2015)
#crime clean-up
crime <- subset(report, select = c("agency_jurisdiction", "crimes_percapita"))
crime[] <- lapply(crime, gsub, pattern = ',', replacement = '')
colnames(crime) <- c("City", "crime_rate")
for (index in 1:nrow(crime)) {
    crime[index, "City"] <- tolower(crime[index, "City"])
}
#merge with final table
final <- merge(final, crime, by = "City")

#museum source
museum <- subset(museums, select = c("Museum.Name", "City..Administrative.Location.", "State..Administrative.Location."))
#museum clean-up
museum$CityState <- paste(museum$City..Administrative.Location, museum$State..Administrative.Location)
museum <- subset(museum, select = c("Museum.Name", "CityState"))
colnames(museum) <- c("Name", "City")
for (index in 1:nrow(museum)) {
    museum[index, "City"] <- tolower(museum[index, "City"])
}
#extract number of museums per city
museumCount <- count(museum, City)
colnames(museumCount) <- c("City", "Number_Of_Museums")
#merge with final table
final <- merge(final, museumCount, by = "City")
#calculate score
for (index in 1:nrow(final)) {
    final[index, "CustomScore"] <- as.numeric(final[index, "NumHospitals"]) * as.numeric(final[index, "Median_Income"]) / as.numeric(final[index, "crime_rate"]) * as.numeric(final[index, "Number_Of_Museums"])
}

#hapiness table
happinessTbl <- htmltab::htmltab("https://wallethub.com/edu/happiest-places-to-live/32619/")
#clean-up hapiness
colnames(happinessTbl) <- c("Rank", "City", "Score", "wellbeing_rank", "income_rank", "community_rank")
happinessTbl <- subset(happinessTbl, select = c("Rank", "City", "Score"))
for (index in 1:nrow(happinessTbl)) {
    happinessTbl[index, "City"] <- tolower(happinessTbl[index, "City"])
}
happinessTbl[] <- lapply(happinessTbl, gsub, pattern = ',', replacement = '')
#merge with cities to get lat long
happinessTbl <- merge(happinessTbl, cities, by = "City")

#head and tail to get happiest and saddest
happiestCities <- head(happinessTbl, 20)
saddestCities<-tail(happinessTbl,20)
#graph counted values
CountGraph <- ggplot(data = final, aes(x = reorder(City,-NumHospitals))) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     xlab("City") + ylab("Count") +
     ggtitle("Cities by Hospital and Museum Availability") +
     geom_bar(aes(y = Number_Of_Museums, group = 1, color = "Number of Museums"), stat = "identity") +
     geom_bar(aes(y = NumHospitals, group = 2, color = "Number of Hospitals",fill=NULL), stat = "identity") +
     scale_color_manual(values = c("blue", "red"))
#display graph
CountGraph
#fix value class
final$crime_rate <- as.numeric(final$crime_rate)
#graph crime
crimeGraph <- ggplot(data = final, aes(x = reorder(City,crime_rate), y = crime_rate,group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_line()

crimeGraph


#blank map
worldMap <- map_data("world", c("USA", "hawaii"), xlim = c(-180, -65), ylim = c(19, 72))
?map_data
base_world <- ggplot() + coord_fixed() +
    xlab("") + ylab("") + geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),
                               colour = "black", fill = "light green")
base_world


#each city different color
colors <- rainbow(nrow(happiestCities))
#add cities to map where size = happiness
map_data <-
    base_world +
    geom_point(data = happiestCities, aes(x = long, y = lat,size=Score),color=colors)
map_data
#chart Scores
scoreChart <- ggplot(data = final, aes(x = reorder(City, - CustomScore), y = CustomScore, group = 1), color = "hot pink") + geom_line(color = "hot pink") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
scoreChart


