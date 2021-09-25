house = read.csv('c:/house7.csv')

names(house)

house = house[,c(-1,-2)]
i=1
diff_mat = matrix(0,nrow(house),ncol(house))

for(i in 1:ncol(house)){
  a= nrow(as.matrix(diff(house[,i])))
  diff_mat[1:a,i]=as.matrix(diff(house[,i]))
  
  #다변량이라서 i~nrow까지 i번째 열에관측치 수만큼
}
house1 = cbind(house,diff_mat)
difh=na.omit(house1[,18:34])
names(difh)

library(tseries)
acf(difh$`17` , main="ACF", ylab="")
pacf(difh$`17`, main="PACF", ylab="")
mtext("ACF, PACF of Pfizer's Stock",outer=TRUE,cex=2)
adf.test(difh$`1`, k=0)
adf.test(difh$`2`, k=0)
adf.test(difh$`3`, k=0)
adf.test(difh$`4`, k=0)
adf.test(difh$`5`, k=0)
adf.test(difh$`6`, k=0)
adf.test(difh$`7`, k=0)
adf.test(difh$`8`, k=0)
adf.test(difh$`9`, k=0)
adf.test(difh$`10`, k=0)
adf.test(difh$`11`, k=0)
adf.test(difh$`12`, k=0)
adf.test(difh$`13`, k=0)
adf.test(difh$`14`, k=0)
adf.test(difh$`15`, k=0)
adf.test(difh$`16`, k=0)
adf.test(difh$`17`, k=0)
house = na.omit(house)


library("ggplot2")
par(mfrow=c(1,2))
acf(house$평균아파트매매가격.전국. , main="ACF", ylab="")
plot(house$평균아파트매매가격.전국.,type='l')
plot(diff(house$평균아파트매매가격.전국.),type='l')
adf.test(house$평균아파트매매가격.전국., k=0)
#house = na.omit(house)
acf(house$평균아파트매매가격.전국. , main="ACF", ylab="")
acf(diff(house$평균아파트매매가격.전국.) , main="ACF", ylab="")


adf.test(house$KOSPI.평균.,k=0)
plot(house$KOSPI.평균.,type='l')
plot(diff(house$KOSPI.평균.),type='l')

adf.test(house$원.미국달러.매매기준율.평균.원.,k=0)
plot(house$원.미국달러.매매기준율.평균.원.,type='l')
plot(diff(house$원.미국달러.매매기준율.평균.원.),type='l')

library("ggplot2")
par(mfrow=c(1,2))
plot(house$원.미국달러.매매기준율.평균.원,type='l')
abline(lm(house$원.미국달러.매매기준율.평균.원.~house$idx),col='red')
plot(diff(house$원.미국달러.매매기준율.평균.원.),type='l')
abline(h=0,col='red')
adf.test(diff(house$원.미국달러.매매기준율.평균.원.),k=0)

plot(house$국내총생산.시장가격.GDP..십억원.,type='l')
plot(diff(house$국내총생산.시장가격.GDP..십억원.),type='l')
adf.test(diff(house$국내총생산.시장가격.GDP..십억원.),k=0)
difg=diff(house$국내총생산.시장가격.GDP..십억원.)


plot(house$아파트.거래량.매매.전국.,type='l')
plot(diff(house$아파트.거래량.매매.전국.),type='l')
adf.test(house$아파트.거래량.매매.전국.,k=0)
adf.test(diff(house$아파트.거래량.매매.전국.),k=0)

plot(house$평균전세가율.전국.,type='l')
plot(diff(house$평균전세가율.전국.),type='l')
adf.test(house$평균전세가율.전국.,k=0)
adf.test(diff(house$평균전세가율.전국.),k=0)


plot(house$소비자물가지수.2015...100.,type='l')
#abline(lm(house$소비자물가지수.2015...100. ~ house$idx),col='red')
plot(diff(house$소비자물가지수.2015...100.),type='l')
#abline(lm(diff(house$소비자물가지수.2015...100.)~house$idx[1:109]),col='red')
adf.test(diff(house$소비자물가지수.2015...100.),k=0)
adf.test(house$소비자물가지수.2015...100.,k=0)
for(i in 1:17){
  adf.test(difh$`i`,k=0)
  
  #다변량이라서 i~nrow까지 i번째 열에관측치 수만큼
}
