house = read.csv('c:/house7.csv')
names(house)
#변수명만 출력
house = house[,c(-1,-2)]
house = na.omit(house)
source("C:/Users/kimna/Documents/house/function/DIFFRANDOMFOREST_FUNCTION.r")

diff_mat = matrix(0,nrow(house),ncol(house))
#빈 매트릭스 만듦
#matrix(매트릭스에 들어갈 숫자, 가로줄 수, 세로줄 수)
#nrow(변수명) : 관측치 수 반환
#ncol(변수명) : 변수 수 반환
diff_mat = as.data.frame(diff_mat)
for(i in 1:ncol(house)){
  
  diff_mat[1:nrow(as.matrix(diff(house[,i]))),i]=as.matrix(diff(house[,i]))
}

dmw = rmw(diff_mat,12)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')




dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
diff_mat = as.data.frame(diff_mat)

for(i in 12:96){
  dmw=rmw(diff_mat,i)
  #sum((diff_mat[20:109,17]-pre_mat[20,])^2)
  o = na.omit(diff_mat[i+1:109,17])
  p = na.omit(dmw)
  m = o-p
  #ms = na.omit(m^2)
  #dmwr[i,] = sum(ms)/(109-i)
  #dmwr[i,]=rmse(actual = o, predicted = p)
  dmwr[i,]=(sum((m)^2))/(109-i)
  #dmwr[i,] = (sum((diff_mat[i:109,17]-pre_mat[i,])^2))/(109-i)
}
plot(dmwr$V1,type='l',xlim=c(12,96))
abline(v=c(12))
dmwr
#plot(dmwr$V1,type='l',xlim=c(12,96),ylim=c(3000,45000))
min(dmwr[12:96,])
which(dmwr$V1<=4000)

res = diff_mat$V17[12:109]-dmw
plot(diff_mat$V17[12:109],type='l')
plot(res, type = 'l')







#################관리한계선#######################
boot_limit = matrix(0,10,1)
#관리한계선을 긋기 위해서 10행 1열 매트릭스만듦

for(i in 1:10){
  boot_limit[i,] = quantile(sample(res[49:109],10000,replace=T),0.975)
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
  boot_limit2[i,] = quantile(sample(res[49:109],10000,replace=T),0.025)
}
#quantile분위수를 구함, 0.95는 유의수준 0.05로 하겠다는 말씀.
#sample랜덤추출함수, t2_mat데이터로, 10000번 시행, replace=T는 복원추출

mean(boot_limit2)
#boot_limit의 평균 값

abline(h=c(mean(boot_limit2)),col='red')
#boot_limit의 평균값을 plot에 가로로(h) 찍음
