diff_kyeong = read.csv('Output/Data/diff_kyeong.csv')
diff_kyeong = diff_kyeong[,-1]
names(diff_kyeong)


##mw없이 gbm하기##
train = diff_kyeong[1:80,]
test = diff_kyeong[-(1:80),]
train = as.data.frame(train)
test = as.data.frame(test)

library(gbm)#gbm라이브러리 불러오기
set.seed(123)#얘는 계속 돌리면 돌릴 수록 학습되기때매 초기화 필요함


model = gbm(train$V17~., data = train)
modelsumm = summary(model)

pre = predict(model, test)

plot(test$V17, type = 'l')
points(pre, type = 'l', col = 'red')

plot(diff_kyeong[,17], type ='l')
points(model$fit, col='red', type = 'l')
points(x = c(81:110),y = pre, col='magenta', type ='l')
abline(v=c(80),col='blue',lty=2)

mse = mean((diff_kyeong[81:110,17]-pre)^2) #1577.53


###############mw적용한 gbm###
source("Code/Functions/DIFFGBM_FUNCTION.r")
gmw = gmw(diff_kyeong,43)
plot(diff_mat[,17],type='l')
points(gmw,type ='l',col='red')

dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
source("Code/Functions/DIFFGBM_FUNCTION.r")
for(i in 43:96){
  source("Code/Functions/DIFFGBM_FUNCTION.r")
  gmw=gmw(diff_kyeong,i)
  dmwr[i,]= mean((diff_kyeong[i:109,17]-gmw[i:111])^2)
}
plot(dmwr$V1,type='l',xlim=c(43,96),ylim=c(550, 1200))

min(dmwr[43:96,])
which(dmwr<=615) #optimal ws 45 mse=614.5008
source("Code/Functions/DIFFGBM_FUNCTION.r")
gmw = gmw(diff_kyeong, 45)
plot(diff_kyeong[,17],type='l')
points(gmw,type='l',col='red')
abline(v = 45, col='blue')

res = diff_kyeong[45:109,17]-gmw[45:111]
plot(res, type='o')


#################관리한계선#######################
boot_limit = matrix(0,10,1)
#관리한계선을 긋기 위해서 10행 1열 매트릭스만듦

for(i in 1:10){
  boot_limit[i,] = quantile(sample(res,10000,replace=T),0.975)
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
  boot_limit2[i,] = quantile(sample(res,10000,replace=T),0.025)
}
#quantile분위수를 구함, 0.95는 유의수준 0.05로 하겠다는 말씀.
#sample랜덤추출함수, t2_mat데이터로, 10000번 시행, replace=T는 복원추출

mean(boot_limit2)
#boot_limit의 평균 값

abline(h=c(mean(boot_limit2)),col='red')
#boot_limit의 평균값을 plot에 가로로(h) 찍음
