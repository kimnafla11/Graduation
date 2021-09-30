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


#house = cbind(house,diff_mat)
#house,diff_mat 나란히 합쳐


names(house)[17]
#17번째 변수명출력

#######

nrow(house)
#관측치 갯수 출력

house$평균아파트매매가격.전국.

house = na.omit(house)

plot(house$평균아파트매매가격.전국.)
####



#####차분한 데이터로 lm#########
#1.train/test나눔
nrow(diff_mat)
diff_mat = na.omit(diff_mat)
diff_tr = diff_mat[1:85,]
diff_te = diff_mat[86:109,]
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

plot(diff_mat[,17],type='l')
points(d_lm$fitted.values,col='red',type='l')
points(x=c(86:109),y=d_pre,col='magenta',type='l')
abline(v=c(85),col='blue',lty=2)

library(Metrics)
rmse(actual = house[86:110,17], predicted = d_pre)
##lm RMSE
#mean(sqrt(((d_pre - house[86:110,17])^2)))


###############moving window#########################
diff_mat = as.data.frame(diff_mat)
diff_mat = na.omit(diff_mat)
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

dmw = mw(diff_mat,12)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
d_re = diff_mat[,17]-dmw
#잔차 구했음
plot(d_re,type='l',ylim=c(-5000,4100))
#잔차 plot
mean(sqrt(((dmw[51:109] - diff_mat[51:109,17])^2)))

dmw = mw(diff_mat,24)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
mean(sqrt((dmw[25:109] - diff_mat[25:109,17])^2))
rmse(predicted = dmw[25:109], actual = diff_mat[25:109,17])
d_re = diff_mat[,17]-dmw
#잔차 구했음
plot(d_re,type='l')
#잔차 plot


#install.packages("Metrics")
#library(Metrics)

#rmse(predicted = dmw[25:109], actual = diff_mat[25:109,17])

dmw = mw(diff_mat,36)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
d_re = diff_mat[,17]-dmw
#잔차 구했음
plot(d_re,type='l')
#잔차 plot
mean(sqrt(((dmw[51:109] - diff_mat[51:109,17])^2)))


dmw = mw(diff_mat,48)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
d_re = diff_mat[,17]-dmw
#잔차 구했음
plot(d_re,type='l')
#잔차 plot
mean(sqrt(((dmw[51:109] - diff_mat[51:109,17])^2)))

dmw = mw(diff_mat,60)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
d_re = diff_mat[,17]-dmw
#잔차 구했음
plot(d_re,type='l')
#잔차 plot
mean(sqrt(((dmw[61:109] - diff_mat[61:109,17])^2)))

dmw = mw(diff_mat,72)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
d_re = diff_mat[,17]-dmw
#잔차 구했음
plot(d_re,type='l')
#잔차 plot
mean(sqrt(((dmw[73:109] - diff_mat[73:109,17])^2)))

dmw = mw(diff_mat,84)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
d_re = diff_mat[,17]-dmw
#잔차 구했음
plot(d_re,type='l')
#잔차 plot
mean(sqrt(((dmw[85:109] - diff_mat[85:109,17])^2)))


dmw = mw(diff_mat,43)
plot(diff_mat[,17],type='l',ylim=c(-100,200))
points(dmw,type ='l',col='red')

