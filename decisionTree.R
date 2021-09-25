house = read.csv('c:/house7.csv')
names(house)

#install.packages("party")
library(party)
#ctree(formula,data)

str(house)

house = house[,-(1:2)]
str(house)


#1. formula 생성
