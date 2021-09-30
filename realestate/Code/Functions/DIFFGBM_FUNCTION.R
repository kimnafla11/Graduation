###############moving window with RANDOMFOREST#########################
gmw = function(stat, w.s){
  
  stat = stat
  tails = w.s
  
  pre_mat = matrix(0,(nrow(stat)+1),1)
  #예측값 저장하는 빈 매트릭스는 관측치+1과 변수1개
  
  
  while(tails<= nrow(stat)){
    window.stat <- stat[(tails-w.s+1):(tails),]
    window.stat = na.omit(window.stat)
    window.gbm = gbm(window.stat$V17~., data = window.stat)
    #GBM
    
    window.pre = predict(window.gbm, stat[(tails+1),])
    pre_mat[(tails+1),] = window.pre

    tails = tails+1
  }
  return(list=c(pre_mat=pre_mat))
}