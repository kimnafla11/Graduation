house = read.csv('c:/house7.csv')
names(house)
house = house[,-(1:2)]
house = na.omit(house)

train = house[1:80,]
test = house[-(1:80),]

library(gbm)
set.seed(123)

model = gbm(train$평균아파트매매가격.전국.~., data = train)
summary(model)

pre = predict(model, test)

plot(test$평균아파트매매가격.전국., type = 'l')
points(pre, type = 'l', col = 'red')

plot(house$평균아파트매매가격.전국., type ='o')
points(model$fit, col='red', type = 'o')
points(x = c(81:110),y = pre, col='magenta', type ='o')
abline(v=c(80),col='blue',lty=2)

