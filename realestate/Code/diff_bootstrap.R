house = read.csv('c:/house7.csv')

names(house)
#변수명만 출력


house = house[,c(-1,-2)]
#house변수에 1,2열을 빼겠다(숫자 변수만 볼라고)


i=1
diff_mat = matrix(0,nrow(house),ncol(house))
#빈 매트릭스 만듦
#matrix(매트릭스에 들어갈 숫자, 가로줄 수, 세로줄 수)
#nrow(변수명) : 관측치 수 반환
#ncol(변수명) : 변수 수 반환

for(i in 1:ncol(house)){
  
  diff_mat[1:nrow(as.matrix(diff(house[,i]))),i]=as.matrix(diff(house[,i]))
}

diff_mat=na.omit(diff_mat)
diff_mat = as.data.frame(diff_mat)

stat = 0
w.s = 0

mw = function(stat, w.s){
  
  #stat = diff_mat
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

dmw = mw(diff_mat,43)
plot(diff_mat[,17],type='o')
points(dmw,type ='l',col='red')
#mean(sqrt(((dmw[51:109] - diff_mat[51:109,17])^2)))

d_re = diff_mat[,17]-dmw
plot(d_re[49:109],type='o')






boot_limit = matrix(0,10,1)
#관리한계선을 긋기 위해서 10행 1열 매트릭스만듦

for(i in 1:10){
  boot_limit[i,] = quantile(sample(d_re[49:109],10000,replace=T),0.975)
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
  boot_limit2[i,] = quantile(sample(d_re[49:109],10000,replace=T),0.025)
}
#quantile분위수를 구함, 0.95는 유의수준 0.05로 하겠다는 말씀.
#sample랜덤추출함수, t2_mat데이터로, 10000번 시행, replace=T는 복원추출

mean(boot_limit2)
#boot_limit의 평균 값

abline(h=c(mean(boot_limit2)),col='red')
#boot_limit의 평균값을 plot에 가로로(h) 찍음

