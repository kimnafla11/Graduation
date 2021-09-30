diff_kang = read.csv('Output/Data/diff_kang.csv')
diff_kang = diff_kang[,-1]
names(diff_kang)

library(randomForest)#randomForest()함수 제공


########mw없이RF############
train = diff_kang[1:80,]
test = diff_kang[-(1:80),]
nrow(test)
train = as.data.frame(train)
test = as.data.frame(test)
model = randomForest(train$V17~., data = train)
summary(model)


plot(test$V17,type = 'l')
#실제 데이터 비교해볼라고 plot찍음
points(predict(model,test),col='red',type='l')
#predict(모델, 데이터)
#학습한 모델로 테스트 데이터 넣어서 predict한것을 points로 찍음
pre = predict(model,test)

plot(diff_kang[,17], type ='l')
points(model$predicted, col='red', type = 'l')
points(x = c(81:110),y = pre, col='magenta', type ='l')
abline(v=c(80),col='blue',lty=2)

mse = mean((diff_kang[81:110,17]-pre)^2) #1373.757



#############moving window with Randomforest###########
source("Code/Functions/DIFFRANDOMFOREST_FUNCTION.r")
rmw = rmw(diff_kang,48)
plot(diff_kang[,17],type='l')
points(rmw,type ='l',col='red')

dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
for(i in 12:96){
  source("Code/Functions/DIFFRANDOMFOREST_FUNCTION.r")
  rmw=rmw(diff_kang,i)
  rmw = na.omit(rmw)
  dmwr[i,]= mean((diff_kang[i:109,17]-rmw[i:110])^2)
}
plot(dmwr$V1,type='l',xlim=c(12,96))
min(dmwr[12:96,])
which(dmwr<=22) #optimal ws 39 mse=21.14552

source("Code/Functions/DIFFRANDOMFOREST_FUNCTION.r")
rmw = rmw(diff_kang,39)
plot(diff_kang[,17],type='l')
points(rmw,type ='l',col='red')
abline(v=39, col='blue')


r = diff_kang$V17[39:109]-rmw[39:110]
plot(r, type = 'o')

#################관리한계선#######################
boot_limit = matrix(0,10,1)
#관리한계선을 긋기 위해서 10행 1열 매트릭스만듦

for(i in 1:10){
  boot_limit[i,] = quantile(sample(r,10000,replace=T),0.975)
}
#quantile분위수를 구함, 0.95는 유의수준 0.05로 하겠다는 말씀.
#sample랜덤추출함수, t2_mat데이터로, 10000번 시행, replace=T는 복원추출

mean(boot_limit)
#boot_limit의 평균 값

abline(h=c(mean(boot_limit)),col='red')
#boot_limit의 평균값을 plot에 가로로(h) 찍음



boot_limit2 = matrix(0,10,1)
#관리한계선을 긋기 위해서 10행 1열 매트릭스만듦

for(i in 1:10){
  boot_limit2[i,] = quantile(sample(r,10000,replace=T),0.025)
}
#quantile분위수를 구함, 0.95는 유의수준 0.05로 하겠다는 말씀.
#sample랜덤추출함수, t2_mat데이터로, 10000번 시행, replace=T는 복원추출

mean(boot_limit2)
#boot_limit의 평균 값

abline(h=c(mean(boot_limit2)),col='red')
#boot_limit의 평균값을 plot에 가로로(h) 찍음

