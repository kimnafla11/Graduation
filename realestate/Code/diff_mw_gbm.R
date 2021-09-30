house = read.csv('Data/house7.csv')

names(house)
house = house[,-(1:2)]
house = na.omit(house)


#차분 변수
diff_mat = matrix(0,nrow(house),ncol(house))
for(i in 1:ncol(house)){
  diff_mat[1:nrow(as.matrix(diff(house[,i]))),i]=as.matrix(diff(house[,i]))
}
diff_mat = as.data.frame(diff_mat)
names(diff_mat)

####################
train = diff_mat[1:80,]
test = diff_mat[-(1:80),]
train = as.data.frame(train)
test = as.data.frame(test)
library(gbm)
set.seed(123)


model = gbm(train$V17~., data = train)
summary(model)

pre = predict(model, test)

plot(test$V17, type = 'l')
points(pre, type = 'l', col = 'red')

plot(diff_mat[,17], type ='l')
points(model$fit, col='red', type = 'l')
points(x = c(81:110),y = pre, col='magenta', type ='l')
abline(v=c(80),col='blue',lty=2)



###########mw#################
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
    
    window.gbm = gbm(window.stat$V17~., data = window.stat)
    #randomforest
    #ws만큼데이터로 v17선형예측, 독립변수는 전부 다
    
    window.pre = predict(window.gbm, stat[(tails+1),])
    #window.lm모델로 원래데이터 예측
    
    pre_mat[(tails+1),] = window.pre
    #빈 매트릭스에 저장
    
    tails = tails+1
    #tails를 1씩 증가
  }
  return(list=c(pre_mat=pre_mat))
}
dmw=mw(diff_mat, 43)
plot(diff_mat$V17, type = 'l')
points(dmw,col='red',type='l')
abline(v = c(43), col = 'blue')




dmwr = matrix(0,96,1)
dmwr = as.data.frame(dmwr)
diff_mat = as.data.frame(diff_mat)


#library(Metrics)

i = 1
#rmse(actual = diff_mat[1:109,17], predicted = pre_mat)

for(i in 43:96){
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
plot(dmwr$V1,type='o',xlim=c(43,96))
min(dmwr[43:96,])
which(dmwr$V1<=2189)
#optimal ws 43

dmw = mw(diff_mat, 43)
plot(diff_mat$V17, type = 'l')
points(dmw , col = 'red', type = 'l')
abline(v = 43, col = 'blue')

r = diff_mat$V17-dmw
plot(r[43:109], type = 'o')
abline(h=0)
