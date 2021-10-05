house = read.csv('Data/house11.csv')
house = house[,c(-1,-2)]
#######

nrow(house)
#관측치 갯수 출력242
house = na.omit(house)
#109

###############moving window#########################
house = as.data.frame(house)

w.s = 0

mw = function(stat, w.s){
  
  stat = house
  tails = w.s
  pre_mat = matrix(0,(nrow(stat)+1),1)
  while(tails<= nrow(stat)){
    window.stat <- stat[(tails-w.s+1):(tails),]
    window.stat = na.omit(window.stat)
    window.lm = lm(window.stat$평균아파트매매가격.전국.~.,data = window.stat)
    #linear regression
    window.pre = predict(window.lm, stat[(tails+1),])
    pre_mat[(tails+1),] = window.pre
    tails = tails+1
  }
  return(list=c(pre_mat=pre_mat))
}

dmwr = matrix(0,96,1)
for(i in 12:96){
  lmw=mw(house,i)
  lmw = na.omit(lmw)
  dmwr[i,]= mean((house[i:109,17]-lmw)^2)
}
dmwr = as.data.frame(dmwr)
plot(dmwr$V1,type='l',xlim=c(12,96), ylim=c(30000,150000))
min(dmwr[12:96,])
which(dmwr<=40094) #optimal ws 22 mse=40093.14

lmw = mw(house,22)
plot(house[,17],type='l')
points(lmw,type ='l',col='red')
abline(v=22, col='blue')

r = house$평균아파트매매가격.전국.[23:109]-lmw[23:109]
plot(r, type = 'o')

write.csv(r, file = 'Output/Data/전국_lm_residual.csv')

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

