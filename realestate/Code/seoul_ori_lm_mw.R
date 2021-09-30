seoul = read.csv('Data/seoul.csv')
seoul = seoul[,-(1:2)]
names(seoul)
seoul = na.omit(seoul)

seoul$평균아파트매매가격.서울.
plot(seoul$평균아파트매매가격.서울., type = 'l')
####



###############moving window#########################
seoul = as.data.frame(seoul)

w.s = 0
mw = function(stat, w.s){
  
  stat = seoul
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
    
    window.lm = lm(window.stat$평균아파트매매가격.서울.~.,data = window.stat)
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



omwr = matrix(0,96,1)
omwr = as.data.frame(omwr)


i = 12
#rmse(actual = diff_mat[1:109,17], predicted = pre_mat)

for(i in 1:96){
  omw=mw(seoul,i)
  #sum((diff_mat[20:109,17]-pre_mat[20,])^2)
  o = na.omit(seoul[i+1:109,17])
  p = na.omit(omw)
  m = o-p
  #ms = na.omit(m^2)
  #dmwr[i,] = sum(ms)/(109-i)
  #dmwr[i,]=rmse(actual = o, predicted = p)
  omwr[i,]=sqrt(sum((m)^2))/(109-i)
  #dmwr[i,] = (sum((diff_mat[i:109,17]-pre_mat[i,])^2))/(109-i)
}
omwr = as.data.frame(omwr)
which.min(omwr[12:96,])
omwr[31,]
omwr[29,]

mean(omwr[12:96,])
plot(omwr$V1,type='l',xlim=c(12,96),ylim=c(0,500))
min(omwr[12:96,])

which(omwr$V1<=61)
#optimal ws 25

plot(seoul$평균아파트매매가격.서울.,type='l')
omw = mw(seoul,25)
points(omw,type ='l',col='red')
mean(sqrt(((omw[25:110] - seoul[25:110,17])^2)))

o = na.omit(seoul[26:109,17])
p = na.omit(omw)
m = o-p
ans = sqrt((sum((m)^2))/(109-25))#540.3433




#####데이터로 lm#########
#1.train/test나눔
seoul = na.omit(seoul)
nrow(seoul)
s_tr = seoul[1:85,]
s_te = seoul[86:110,]

s_lm = lm(s_tr$평균아파트매매가격.서울.~., data = s_tr)
s_pre = predict(s_lm,s_te)
plot(seoul$평균아파트매매가격.서울.,type='l')
points(s_lm$fitted.values,col='red',type='l')
points(x=c(86:110),y=s_pre,col='magenta',type='l')
abline(v=c(85),col='blue',lty=2)
#rmse(actual=house[86:109,17], predicted = h_pre)
o = na.omit(seoul[86:109,17])
p = na.omit(s_pre)
m = o-p
ans = sqrt((sum((m)^2))/(109-85))#231.9099



omw = mw(house,31)
plot(house$평균아파트매매가격.전국.,type='l',ylim=c(200,850))
points(omw,type ='l',col='red')
#rmse(actual=house[31:109,17], predicted = omw[31:109])
