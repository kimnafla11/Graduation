house = read.csv('c:/house7.csv')
names(house)
house = house[,-(1:2)]
house = na.omit(house)

#install.packages("nnet")
library(nnet)
#library(caret)
#library(ROCR)

train = house[1:80,]
test = house[-(1:80),]


#nnet 모델링
#size : hidden node 수
#maxit:반복횟수
#decay : overfitting을 피하기 위해 사용하는 weight decay parameter
#rang : Initial random weight on . defalut0.5

nn_model = nnet(train$평균아파트매매가격.전국.~.,data = train, size = 10, maxit = 10000)


#install.packages("devtools")
library(devtools)
source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn_model)

#install.packages("NeuralNetTools")
library(NeuralNetTools)
garson(nn_model)


summary(nn_model)

predict()
pre = predict(nn_model, train)

plot(test$평균아파트매매가격.전국.,type = 'o')
#실제 데이터 비교해볼라고 plot찍음
points(pre,col='red',type='o')

plot(pre)



model = nnet(train$평균아파트매매가격.전국.~.,data = train, size = 20, maxit = 10000)


pred = predict(model, train[,1:16])







data(iris)
nn.iris <- nnet(Species~., data=iris, size=2, rang=0.1, decay=5e-4, maxit=200)
summary(nn.iris)
table(iris$Species, predict(nn.iris, iris, type="class"))

nrow(iris)
train = iris[1:80,]
test = iris[-(1:80),]
pre = predict(nn.iris, test, type = "class")
actual = test$Species
table(actual,pre)

house = read.csv('c:/house7.csv')
names(house)
house = house[,-(1:2)]
house = na.omit(house)
train = house[1:80,]
test = house[-(1:80),]
str(train)
nn_model = nnet(train$평균아파트매매가격.전국.~train$기준금리...+train$국민총소득.GNI., data = train, size = 10)
summary(nn_model)
plot(nn_model)

predict(nn_model,test)

pre = predict(nn_model, test[])



train = house[1:55,]
test = house[-(1:55),]
nn_model = nnet(train$평균아파트매매가격.전국.~train$기준금리...+train$국민총소득.GNI., data = train, size = 10)
summary(nn_model)
plot(nn_model)

predict(nn_model,test)
