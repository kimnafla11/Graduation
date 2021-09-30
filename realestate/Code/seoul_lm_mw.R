seoul = read.csv('Data/seoul.csv')
seoul = seoul[,-(1:2)]
names(seoul)
seoul = na.omit(seoul)

##서울 부동산 차분
diff_seo = matrix(0,nrow(seoul),ncol(seoul))

for(i in 1:ncol(seoul)){
  diff_seo[1:nrow(as.matrix(diff(seoul[,i]))),i]=as.matrix(diff(seoul[,i]))
}

#####차분한 데이터로 lm#########
#1.train/test나눔
nrow(diff_seo)
diff_seo = na.omit(diff_seo)
diff_tr = diff_seo[1:85,]
diff_te = diff_seo[86:109,]
##train/test 데이터 분리 1~85/86~110번째 관측치로

diff_tr = data.frame(diff_tr)
diff_te = data.frame(diff_te)
#lm함수 쓸 땐 data.frame쓰는게 좋음

d_lm = lm(diff_tr$X17~., data = diff_tr)
#diff_tr데이터에 diff_tr의 x17변수를 가지고 선형회귀
#lm(Y~X, data = data)

summary(d_lm)
#lm모델 결과

plot(diff_te$X17,type = 'l')
#실제 데이터 비교해볼라고 plot찍음

points(predict(d_lm,diff_te),col='red',type='l')
#predict(모델, 데이터)
#학습한 모델로 테스트 데이터 넣어서 predict한것을 points로 찍음

d_pre = predict(d_lm,diff_te)

plot(diff_seo[,17],type='l')
points(d_lm$fitted.values,col='red',type='l')
points(x=c(86:109),y=d_pre,col='magenta',type='l')
abline(v=c(85),col='blue',lty=2)


##mse
o = na.omit(diff_seo[86:109,17])
p = na.omit(d_pre)
m = o-p
ans = sqrt((sum((m)^2))/(109-85))##59.40704




############seoul_diff_mw###############
diff_seo = as.data.frame(diff_seo)

diff_seo = na.omit(diff_seo)
stat = 0
w.s = 0

mw = function(stat, w.s){
  
  stat = diff_seo
  #w.s = 30
  #window사이즈 30
  
  
  tails = w.s
  #초기 tails는 w.s부터 시작함
  
  pre_mat = matrix(0,(nrow(stat)+1),1)
  #예측값 저장하는 빈 매트릭스는 관측치+1과 변수1개
  
  
  while(tails<= nrow(stat)){
    #tails가 stat관측치 수까지 반복
    
    window.stat <- stat[(tails-w.s+1):(tails),]
    #window.stat에 stat[1부터:tails,tails ]를 저장
    #과거 30사이즈 데이터만 쓰겠다고
    
    window.stat = na.omit(window.stat)
    #결측치 제거
    
    window.lm = lm(window.stat$V17~.,data = window.stat)
    #linear regression
    #ws만큼데이터로 v17선형예측, 독립변수는 전부 다
    
    window.pre = predict(window.lm, stat[(tails+1),])
    #window.lm모델로 원래데이터 예측
    
    pre_mat[(tails+1),] = window.pre
    #빈 매트릭스에 저장
    
    tails = tails+1
    #tails를 1씩 증가
  }
  return(list=c(pre_mat=pre_mat))
}




####optimal ws residual####
dmwr = matrix(0,96,1) #residual담을 빈 매트릭스
dmwr = as.data.frame(dmwr)
for(i in 12:96){
  dmw=mw(diff_seo,i)
  o = na.omit(diff_seo[i:109,17])
  p = na.omit(dmw)
  m = o-p
  dmwr[i,]=(sum((m)^2))/(109-i)
}



dmwr = as.data.frame(dmwr)
plot(dmwr$V1, type = 'l', xlim = c(12,96), ylim = c(3000,40000))
min(dmwr[12:96,])
which(dmwr<=72)
#optimal ws 45

seo_dmw = mw(diff_seo,44)
plot(diff_seo[,17],type='l')
points(seo_dmw,type ='l',col='red')
abline(v=c(45),col='blue',lty=2)
seo_dmw = as.matrix(seo_dmw)
o = na.omit(diff_seo[45:109,17])
p = na.omit(seo_dmw[45:109])
m = o-p
ans = sqrt((sum((m)^2))/(109-44)) #59.83788





r = diff_mat$V17-dmw
plot(r[45:109], type = 'l')
abline(h=0)





#################관리한계선#######################
boot_limit = matrix(0,10,1)
#관리한계선을 긋기 위해서 10행 1열 매트릭스만듦

for(i in 1:10){
  boot_limit[i,] = quantile(sample(r[49:109],10000,replace=T),0.975)
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
  boot_limit2[i,] = quantile(sample(r[49:109],10000,replace=T),0.025)
}
#quantile분위수를 구함, 0.95는 유의수준 0.05로 하겠다는 말씀.
#sample랜덤추출함수, t2_mat데이터로, 10000번 시행, replace=T는 복원추출

mean(boot_limit2)
#boot_limit의 평균 값

abline(h=c(mean(boot_limit2)),col='red')
#boot_limit의 평균값을 plot에 가로로(h) 찍음
