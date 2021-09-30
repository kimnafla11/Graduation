house = read.csv('c:/house7.csv')
names(house)
house = house[,-(1:2)]

y = na.omit(house$평균아파트매매가격.전국.)
#y = 184
house = na.omit(house)
nrow(house)

train = house[1:80,]
test = house[-(1:80),]
formula = house$평균아파트매매가격.전국.~.

ctree = ctree(formula, data = house)
plot(ctree, type = "simple")
plot(ctree)
