library(htmltab)
install.packages("tidyverse")
library(tidyverse)
#Source 1
url <- "http://www.city-data.com/top36.html"
medianIncome <- htmltab(url, which = 8)
colnames(medianIncome) <- c("Rank", "City", "Amount")
#Source 2
url <- "http://www.city-data.com/top60.html"
safety <- htmltab(url, which = 8)
colnames(safety) <- c("Rank", "City", "Amount")
#Source 3
url <- "http://www.city-data.com/top2/c543.html"
realEstateTaxes <- htmltab(url, which = 8)
colnames(realEstateTaxes) <- c("Rank", "City", "Amount")
#Source 4
url <- "http://www.city-data.com/top50.html"
recreation <- htmltab(url, which = 8)
colnames(recreation) <- c("Rank", "City", "Amount")
#Source 5
url <- "http://www.city-data.com/top2/h179.html"
smartPeople <- htmltab(url, which = 8)
colnames(smartPeople) <- c("Rank", "City", "Amount")
#Source 6
url <- "http://www.city-data.com/top2/c469.html"
sunshine <- htmltab(url, which = 8)
colnames(sunshine) <- c("Rank", "City", "Amount")
#Source 7
url <-"http://www.city-data.com/top2/c458.html"
stableTemperature <- htmltab(url, which = 8)
colnames(stableTemperature) <- c("Rank", "City", "Amount")
#Source 8
url <- "https://wallethub.com/edu/happiest-places-to-live/32619/"
happy <- htmltab(url, which = 1)
happy <- happy[, 0:2]
colnames(happy)<-c("Rank","City")

#Clean-up

#1
for (index in 1:nrow(medianIncome)) {
    medianIncome[index, "City"] <- str_remove_all(medianIncome[index, "City"], "\\(.{1,}\\)")
   
}
row.names(medianIncome) <- c(1:nrow(medianIncome))

#2
for (index in 1:nrow(safety)) {
    safety[index, "City"] <- str_remove_all(safety[index, "City"], "\\(.{1,}\\)")
}
#3
for (index in 1:nrow(realEstateTaxes)) {
    realEstateTaxes[index, "City"] <- str_remove_all(realEstateTaxes[index, "City"], "\\(.{1,}\\)")
}
#4
for (index in 1:nrow(recreation)) {
    recreation[index, "City"] <- str_remove_all(recreation[index, "City"], "\\(.{1,}\\)")
}
#5
for (index in 1:nrow(smartPeople)) {
    smartPeople[index, "City"] <- str_remove_all(smartPeople[index, "City"], "\\(.{1,}\\)")
}
#6
for (index in 1:nrow(sunshine)) {
    sunshine[index, "City"] <- str_remove_all(sunshine[index, "City"], "\\(.{1,}\\)")
}
#7
for (index in 1:nrow(stableTemperature)) {
    stableTemperature[index, "City"] <- str_remove_all(stableTemperature[index, "City"], "\\(.{1,}\\)")
}
#8
for()

#Merging
#financial group
financial <- merge(medianIncome, realEstateTaxes, by = "City")
#Enviormental Factors
enviornment <- merge(sunshine, stableTemperature, by = "City")
enviornment<-merge(enviornment,recreation,by="City")
#Other
other <- merge(safety, smartPeople, by = "City")

satisfaction <- merge(happy,, by = "City")

happy<-head(happy,20)
ggplot(data = happy, aes(x = reorder(City,c(1:nrow(happy))), y = c(1:nrow(happy)))) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = medianIncome, aes(x = City, y = c(1:nrow(medianIncome)))) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


