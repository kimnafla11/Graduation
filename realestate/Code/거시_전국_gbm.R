house = read.csv('Data/house11.csv')
house = house[,c(-1,-2)]
house = na.omit(house)
house = as.data.frame(house)
str(house)
library(gbm)#gbm라이브러리 불러오기
set.seed(123)#얘는 계속 돌리면 돌릴 수록 학습되기때매 초기화 필요함

w.s = 0
###############mw적용한 gbm###
house = as.data.frame(house)

w.s = 0

mw = function(stat, w.s){
  stat = house
  tails = w.s
  pre_mat = matrix(0,(nrow(stat)+1),1)
  while(tails<= nrow(stat)){
    window.stat <- stat[(tails-w.s+1):(tails),]
    window.stat = na.omit(window.stat)
    window.gbm = gbm(window.stat$평균아파트매매가격.전국.~.,data = window.stat)
    window.pre = predict(window.gbm, stat[(tails+1),])
    pre_mat[(tails+1),] = window.pre
    tails = tails+1
  }
  return(list=c(pre_mat=pre_mat))
}
gmw = mw(house,43)
plot(house[,17],type='l')
points(gmw,type ='l',col='red')

dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
for(i in 43:96){
  gmw=mw(house,i)
  dmwr[i,]= mean((house[i:109,17]-gmw[i:110])^2)
}
plot(dmwr$V1,type='l',xlim=c(43,96))
min(dmwr[43:96,])
which(dmwr<=4508) #optimal ws 47 mse=4507.576

gmw = mw(house, 47)
plot(house[,17],type='l')
points(gmw,type='l',col='red')
abline(v = 47, col='blue')

res = house[48:109,17]-gmw[48:110]
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
