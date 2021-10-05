house = read.csv('Data/kyeong11.csv')
house = house[,c(-1,-2)]
house = na.omit(house)
library(randomForest)#randomForest()함수 제공


#############moving window with Randomforest###########
w.s = 0
mw = function(stat, w.s){
  stat = house
  tails = w.s
  pre_mat = matrix(0,(nrow(stat)+1),1)
  while(tails<= nrow(stat)){
    window.stat <- stat[(tails-w.s+1):(tails),]
    window.stat = na.omit(window.stat)
    window.rf = randomForest(window.stat$평균아파트매매가격.경기.~., data = window.stat)
    #randomForest
    window.pre = predict(window.rf, stat[(tails+1),])
    pre_mat[(tails+1),] = window.pre
    tails = tails+1
  }
  return(list=c(pre_mat=pre_mat))
}

dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
for(i in 12:96){
  rmw=mw(house,i)
  rmw = na.omit(rmw)
  dmwr[i,]= mean((house[i:104,17]-rmw)^2)
}
plot(dmwr$V1,type='l',xlim=c(12,96))
min(dmwr[12:96,])
which(dmwr<=17393) #optimal ws 12 mse=17392.77

rmw = mw(house,12)
plot(house[,17],type='l')
points(rmw,type ='l',col='red')
abline(v=12, col='blue')


r = house$평균아파트매매가격.경기.[13:104]-rmw[13:104]
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

