diff_chung = read.csv('Output/Data/diff_chung.csv')
diff_chung = diff_chung[,-1]
names(diff_chung)

source("Code/Functions/DIFFMOVINGWINDOW_FUNCTION.r")

dmw = lmw(diff_mat,32)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')

dmwr = matrix(0,96,1)
for(i in 12:96){
  source("Code/Functions/DIFFMOVINGWINDOW_FUNCTION.r")
  lmw=lmw(diff_chung,i)
  lmw = na.omit(lmw)
  dmwr[i,]= mean((diff_chung[i:109,17]-lmw)^2)
}
dmwr = as.data.frame(dmwr)
plot(dmwr$V1,type='l',xlim=c(12,96), ylim=c(100,1000))
min(dmwr[12:96,])
which(dmwr<=135) #optimal ws 41 mse=134.019
dmwr[41,]


source("Code/Functions/DIFFMOVINGWINDOW_FUNCTION.r")
lmw = lmw(diff_chung,41)
plot(diff_chung[,17],type='l')
points(lmw,type ='l',col='red')
abline(v=41, col='blue')

r = diff_chung$V17[41:109]-lmw[41:110]
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

