house = read.csv('c:/house7.csv')

names(house)
#변수명만 출력


house = house[,c(-1,-2)]
#house변수에 1,2열을 빼겠다(숫자 변수만 볼라고)


i=1
names(house)[17]
#17번째 변수명출력

#######

nrow(house)
#관측치 갯수 출력

house$평균아파트매매가격.전국.

house = na.omit(house)

plot(house$평균아파트매매가격.전국.)
####



###############moving window#########################
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

#library("ggplot2")
#par(mfrow=c(2,4))############################3

omw = mw(house,12)
plot(house$평균아파트매매가격.전국.,type='l')
points(omw,type ='l',col='red')
mean(sqrt(((omw[51:110] - house[51:110,17])^2)))

omw = mw(house,24)
plot(house$평균아파트매매가격.전국.,type='l')
points(omw,type ='l',col='red')
mean(sqrt(((omw[51:110] - house[51:110,17])^2)))

omw = mw(house,36)
plot(house$평균아파트매매가격.전국.,type='l')
points(omw,type ='l',col='red')
mean(sqrt(((omw[51:110] - house[51:110,17])^2)))

omw = mw(house,48)
plot(house$평균아파트매매가격.전국.,type='l')
points(omw,type ='l',col='red')
mean(sqrt(((omw[51:110] - house[51:110,17])^2)))


#####데이터로 lm#########
#1.train/test나눔
house = na.omit(house)
nrow(house)
h_tr = house[1:85,]
h_te = house[86:110,]

h_lm = lm(h_tr$평균아파트매매가격.전국.~., data = h_tr)
h_pre = predict(h_lm,h_te)
plot(house$평균아파트매매가격.전국.,type='l')
points(h_lm$fitted.values,col='red',type='l')
points(x=c(86:110),y=h_pre,col='magenta',type='l')
abline(v=c(85),col='blue',lty=2)
#rmse(actual=house[86:109,17], predicted = h_pre)

mean(sqrt(((h_pre - house[51:110,17])^2)))



omw = mw(house,31)
plot(house$평균아파트매매가격.전국.,type='l',ylim=c(200,850))
points(omw,type ='l',col='red')
#rmse(actual=house[31:109,17], predicted = omw[31:109])
