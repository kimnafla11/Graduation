diff_house = read.csv('Output/Data/diff_house.csv')
names(diff_house)
diff_house = diff_house[,2:17]

res = read.csv('Output/Data/전국_rf_residual.csv')
res = res[,-(1)]

mydata = cbind(diff_house[1:90,],res)
mydata = as.data.frame(mydata)

formula = mydata$res~.

library(party)
rtree = ctree(formula, data = mydata)
summary(rtree)
plot(rtree, type = "simple")