diff_kang = read.csv('Output/Data/diff_kang.csv')
diff_kang = diff_kang[,-1]
names(diff_kang)

source("Code/Functions/DIFFMOVINGWINDOW_FUNCTION.r")

dmw = lmw(diff_mat,32)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')

dmwr = matrix(0,96,1)
for(i in 12:96){
  source("Code/Functions/DIFFMOVINGWINDOW_FUNCTION.r")
  lmw=lmw(diff_kang,i)
  lmw = na.omit(lmw)
  dmwr[i,]= mean((diff_kang[i:109,17]-lmw)^2)
}
dmwr = as.data.frame(dmwr)
plot(dmwr$V1,type='l',xlim=c(12,96), ylim=c(0,400))
min(dmwr[12:96,])
which(dmwr<=28.02848) #optimal ws 69 mse=28.02848
dmwr[69,]


source("Code/Functions/DIFFMOVINGWINDOW_FUNCTION.r")
lmw = lmw(diff_kang,69)
plot(diff_kang[,17],type='l')
points(lmw,type ='l',col='red')
abline(v=69, col='blue')

r = diff_kang$V17[69:109]-lmw[69:110]
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

