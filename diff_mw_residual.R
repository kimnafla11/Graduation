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

###############moving window#########################
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

diff_mat = na.omit(diff_mat)
nrow(diff_mat)
dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
diff_mat = as.data.frame(diff_mat)


#library(Metrics)

i = 1
mw(diff_mat)
#rmse(actual = diff_mat[1:109,17], predicted = pre_mat)

for(i in 12:96){
  dmw=mw(diff_mat,i)
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
plot(dmwr$V1,type='l',xlim=c(12,96),ylim=c(3000,45000))
min(dmwr[12:96,])
which(dmwr$V1<=4000)
#optimal ws 43

