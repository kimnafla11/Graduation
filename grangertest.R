house = read.csv('c:/house7.csv')

names(house)
#변수명만 출력


house = house[,c(-1,-2)]

plot(house$평균아파트매매가격.전국., type='l')
plot(house$평균전세가율.전국., type = 'l')

library(lmtest)
par(mfrow=c(2,1))
plot.ts(house$평균아파트매매가격.전국.)
plot.ts(house$평균전세가율.전국.)

require(forecast)
ndiffs(house$평균아파트매매가격.전국., alpha = 0.05, test=c("kpss"))
ndiffs(house$평균전세가율.전국., alpha = 0.05, test=c("kpss"))

grangertest(house$평균아파트매매가격.전국.~house$평균전세가율.전국., order = 1)
##-> 전세가 먼저

grangertest(house$평균전세가율.전국.~house$평균아파트매매가격.전국., order = 1)
##-> 아파트매매가격이 전세가율에 1개월 인과

grangertest(house$평균아파트매매가격.전국.~house$평균전세가율.전국., order = 6)
### 전세가 6개월 선행(0.02543) ★

grangertest(house$평균아파트매매가격.전국.~house$평균전세가율.전국., order = 7)
###0.03918

grangertest(house$평균아파트매매가격.전국.~house$평균전세가율.전국., order = 8)
##0.03541

grangertest(house$평균아파트매매가격.전국.~house$평균전세가율.전국., order = 9)
###0.1331

grangertest(house$평균아파트매매가격.전국.~house$평균전세가율.전국., order = 10)
##0.1775

grangertest(house$평균아파트매매가격.전국.~house$평균전세가율.전국., order = 11)
##0.1913

grangertest(house$평균아파트매매가격.전국.~house$.,평균전세가율.전국., order = 12)
#0.2443


##grangertest(결과~ 원인, order = lag값)

house= na.omit(house)
cor(house$평균아파트매매가격.전국., house$평균전세가율.전국.)





grangertest(house$평균아파트매매가격.전국.~house$원.미국달러.매매기준율.평균.원., order = 1)

grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 1)
#0.006088 **
grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 2)
#3.799e-06 ***
grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 3)
#5.469e-05 ***
grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 4)
#0.001974 **
grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 5)
#0.002852 **
grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 6)
grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 7)
grangertest(house$평균아파트매매가격.전국.~house$국민총소득.GNI., order = 8)



grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 1)
#0.006229 **
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 2)
#1.558e-05 ***
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 3)
#6.249e-05 ***
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 4)
#0.002455 **
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 5)
#0.002943 **
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 6)
# 0.01621 *
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 7)
#0.03248 *
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 8)
#0.03022 *
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 9)
#0.09298 .
grangertest(house$평균아파트매매가격.전국.~house$국내총생산.시장가격.GDP..십억원., order = 10)
#0.1485



grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 1)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 2)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 3)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 4)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 5)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 6)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 7)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 8)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 9)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 10)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 11)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 12)
grangertest(house$평균아파트매매가격.전국.~house$KOSPI.평균., order = 13)


grangertest(house$평균아파트매매가격.전국.~house$소비자물가지수.2015...100., order = 1)
grangertest(house$평균아파트매매가격.전국.~house$소비자물가지수.2015...100., order = 2)
#3.466e-05 ***
grangertest(house$평균아파트매매가격.전국.~house$소비자물가지수.2015...100., order = 3)
grangertest(house$평균아파트매매가격.전국.~house$소비자물가지수.2015...100., order = 4)
grangertest(house$평균아파트매매가격.전국.~house$소비자물가지수.2015...100., order = 5)
grangertest(house$평균아파트매매가격.전국.~house$소비자물가지수.2015...100., order = 6)
grangertest(house$평균아파트매매가격.전국.~house$소비자물가지수.2015...100., order = 7)




plot(house$평균아파트매매가격.전국., type='l')
plot(house$기준금리..., type = 'l')

grangertest(house$평균아파트매매가격.전국.~house$기준금리..., order = 1)
grangertest(house$평균아파트매매가격.전국.~house$기준금리..., order = 2)
#0.0005475 ***
grangertest(house$평균아파트매매가격.전국.~house$기준금리..., order = 3)
grangertest(house$평균아파트매매가격.전국.~house$기준금리..., order = 4)
grangertest(house$평균아파트매매가격.전국.~house$기준금리..., order = 5)
grangertest(house$평균아파트매매가격.전국.~house$기준금리..., order = 6)
grangertest(house$평균아파트매매가격.전국.~house$기준금리..., order = 7)

grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 1)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 2)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 3)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 4)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 5)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 6)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 7)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 8)
grangertest(house$평균아파트매매가격.전국.~house$아파트.거래량.매매.전국., order = 9)


grangertest(house$평균아파트매매가격.전국.~house$가계대출.조원., order = 1)
grangertest(house$평균아파트매매가격.전국.~house$가계대출.조원., order = 2)
#1.272e-05 ***
grangertest(house$평균아파트매매가격.전국.~house$가계대출.조원., order = 3)
#0.0002659 ***
grangertest(house$평균아파트매매가격.전국.~house$가계대출.조원., order = 4)
grangertest(house$평균아파트매매가격.전국.~house$가계대출.조원., order = 5)
grangertest(house$평균아파트매매가격.전국.~house$가계대출.조원., order = 6)


c_mat = as.matrix(cor(house$평균아파트매매가격.전국., house$기준금리...))
rownames(c_mat) ="평균아파트매매가격"
colnames(c_mat) = "기준금리(%)"

library(corrplot)
corrplot(c_mat,'number')
names(house)



grangertest(house$기준금리...~house$총저축률..., order = 6)
grangertest(house$기준금리...~house$KOSPI.평균., order = 4)
grangertest(house$기준금리...~house$총저축률..., order = 6)
grangertest(house$국내총생산.시장가격.GDP..십억원.~house$KOSPI.평균.,order = 2)
