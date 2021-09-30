house = read.csv('c:/house7.csv')

names(house)
#변수명만 출력


house = house[,c(-1,-2)]
house = na.omit(house)

house = as.data.frame(house)

stat = 0
w.s = 0

mw = function(stat, w.s){
  
  #stat = house
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
    
    window.lm = lm(window.stat$평균아파트매매가격.전국.~.,data = window.stat)
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

nrow(house)

omwr = matrix(0,96,1)
omwr = as.data.frame(omwr)


i = 12
#rmse(actual = diff_mat[1:109,17], predicted = pre_mat)

for(i in 12:96){
  omw=mw(house,i)
  #sum((diff_mat[20:109,17]-pre_mat[20,])^2)
  o = na.omit(house[i+1:109,17])
  p = na.omit(omw)
  m = o-p
  #ms = na.omit(m^2)
  #dmwr[i,] = sum(ms)/(109-i)
  #dmwr[i,]=rmse(actual = o, predicted = p)
  omwr[i,]=(sum((m)^2))/(109-i)
  #dmwr[i,] = (sum((diff_mat[i:109,17]-pre_mat[i,])^2))/(109-i)
}
omwr = as.data.frame(omwr)
max(omwr$V1)
which.min(omwr[12:96,])
omwr[31,]
omwr[29,]

mean(omwr[12:96,])
plot(omwr$V1,type='l',xlim=c(12,96),ylim=c(65000,1000000))
min(omwr[12:96,])
which(omwr$V1<=70000)
#optimal ws 31

