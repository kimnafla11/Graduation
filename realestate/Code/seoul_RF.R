seoul = read.csv('Data/seoul.csv')

names(seoul)
seoul = seoul[,-(1:2)]
seoul = na.omit(seoul)


#차분 변수
diff_mat = matrix(0,nrow(seoul),ncol(seoul))
for(i in 1:ncol(seoul)){
  diff_mat[1:nrow(as.matrix(diff(seoul[,i]))),i]=as.matrix(diff(seoul[,i]))
}
diff_mat[,17]





#randomForest(formula, data,ntree,mtry,na.action,importance)

#formula : Y~X형식으로 반응변수와 설명변수 식
#data : 모델 생성에 사용될 데이터 셋
#ntree : 복원추출하여 생성할 트리 수 지정
#mtry : 자식 노드 분류할 변수 수 지정
#na.action : 결측치 제거할 함수 지정
#importance : 분류모델 생성과정에서 중요변수정보제공여부


#패키짖 설치

#install.packages('randomForest')
library(randomForest)#randomForest()함수 제공



####################
train = diff_mat[1:80,]
test = diff_mat[-(1:80),]
nrow(test)
train = as.data.frame(train)
test = as.data.frame(test)
model = randomForest(train$V17~., data = train)
summary(model)


plot(test$V17,type = 'l')
#실제 데이터 비교해볼라고 plot찍음
points(predict(model,test),col='red',type='l')
#predict(모델, 데이터)
#학습한 모델로 테스트 데이터 넣어서 predict한것을 points로 찍음
pre = predict(model,test)

plot(diff_mat[,17], type ='l')
points(model$predicted, col='red', type = 'l')
points(x = c(81:110),y = pre, col='magenta', type ='l')
abline(v=c(80),col='blue',lty=2)




#############moving window with Randomforest###########
diff_mat = as.data.frame(diff_mat)
diff_mat = na.omit(diff_mat)
w.s = 0

mw = function(stat, w.s){
  
  stat = diff_mat
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
    #randomforest
    #ws만큼데이터로 v17선형예측, 독립변수는 전부 다
    
    window.pre = predict(window.rf, stat[(tails+1),])
    #window.lm모델로 원래데이터 예측
    
    pre_mat[(tails+1),] = window.pre
    #빈 매트릭스에 저장
    
    tails = tails+1
    #tails를 1씩 증가
  }
  return(list=c(pre_mat=pre_mat))
}

dmw=mw(diff_mat, 40)
plot(diff_mat$V17, type = 'l')
points(dmw,col='red',type='l')
abline(v = c(40), col = 'blue')







dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
diff_mat = as.data.frame(diff_mat)


#library(Metrics)

i = 1
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
plot(dmwr$V1,type='l',xlim=c(12,96))
min(dmwr[12:96,])
which(dmwr$V1<=1480)
#optimal ws 12

dmw = mw(diff_mat, 12)
plot(diff_mat$V17, type = 'l')
points(dmw , col = 'red', type = 'l')
abline(v = 12, col = 'blue')

r = diff_mat$V17-dmw
plot(r[19:109], type = 'l')
abline(h=0)
