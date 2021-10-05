diff_house = read.csv('Output/Data/diff_house.csv')
names(diff_house)
diff_house = diff_house[,-1]

#install.packages("RSNNS")
library(RSNNS)
#install.packages("MASS")
library(MASS)
library(Metrics)
#library(nnet)
###############moving window#########################
diff_house = as.data.frame(diff_house)
diff_house = na.omit(diff_house)

w.s = 0
mw = function(stat, w.s){
  
  stat = diff_house
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
    window.nnet = nnet(window.stat$V17~.,data = window.stat, size = 30, maxit = 10000)
    #nnet
    #ws만큼데이터로 v17선형예측, 독립변수는 전부 다
    
    window.pre = predict(window.nnet, stat[(tails+1),])
    #window.lm모델로 원래데이터 예측
    
    pre_mat[(tails+1),] = window.pre
    #빈 매트릭스에 저장
    
    tails = tails+1
    #tails를 1씩 증가
  }
  return(list=c(pre_mat=pre_mat))
}


set.seed(1234)
train = diff_house[1:80,]
test = diff_house[-(1:80),]
input_train=train[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16")]
output_train=train[,c("V17")]
input_test=test[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16")]
output_test=test[,c("V17")]

mlp=mlp(input_train, output_train, size=c(10,10,10), maxit = 10000)
summary(mlp)
predictions <- predict(mlp,input_test)
plot(diff_house[-(1:80),17],type='l')
points(predictions,type='l')

#nn_model = nnet(train$V17~.,data = train, size = 10, maxit = 10000)
#pre = predict(nn_model, train)

dmwr = matrix(0,96,1)
for(i in 12:96){
  dmw=mw(diff_house,i)
  dmw = na.omit(dmw)
  dmwr[i,]= mean((diff_house[i:109,17]-dmw)^2)
}
dmwr = as.data.frame(dmwr)
plot(dmwr$V1,type='l',xlim=c(12,96), ylim=c(500,2000))
min(dmwr[12:96,])
which(dmwr<=808) #optimal ws 22 mse=1805.358
dmwr[22,]

nmw = mw(diff_house,22)
plot(diff_house[,17],type='l')
points(nmw,type ='l',col='red')
abline(v=22, col='blue')

r = diff_house$V17[88:109]-lmw[88:110]
plot(r, type = 'o')
