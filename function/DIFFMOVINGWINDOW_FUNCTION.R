###############moving window with LM#########################
lmw = function(stat, w.s){
  
  stat = stat
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


rmw = function(stat, w.s){
  
  stat = stat
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
    window.rf = randomForest(window.stat$V17~., data = window.stat)
    #randomForest
    #ws만큼데이터로 v17랜포, 독립변수는 전부 다
    
    window.pre = predict(window.rf, stat[(tails+1),])
    #window.lm모델로 원래데이터 예측
    
    pre_mat[(tails+1),] = window.pre
    #빈 매트릭스에 저장
    
    tails = tails+1
    #tails를 1씩 증가
  }
  return(list=c(pre_mat=pre_mat))
}