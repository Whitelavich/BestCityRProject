library(htmltab)

#Source A
url <- "http://www.city-data.com/top36.html"
medianIncome <- htmltab(url, which = 8)
#Source B
url <- "http://www.city-data.com/top60.html"
safety <- htmltab(url, which = 8)
#Source C
url <- "http://www.city-data.com/top2/c543.html"
realEstateTaxes <- htmltab(url, which = 8)
#Source D
url <- "http://www.city-data.com/top50.html"
recreation <- htmltab(url, which = 8)
#Source E
url <- "http://www.city-data.com/top2/h179.html"
smartPeople <- sunshine <- htmltab(url, which = 8)
#Source F
url <- "http://www.city-data.com/top2/c469.html"
sunshine <- htmltab(url, which = 8)
colnames(sunshine) <- c("Rank", "City", "Amount")
#Source G
url <= "http://www.city-data.com/top2/c458.html"
stableTemperature <- htmltab(url, which = 8)
