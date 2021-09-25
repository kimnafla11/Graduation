house = read.csv('c:/house7.csv')

names(house)

house = na.omit(house[,-(1:2)])

par(mfrow=c(1,2))
ts.plot(house$평균아파트매매가격.전국.,type='l')
dif=diff(house$평균아파트매매가격.전국.,type='l')
ts.plot(dif,type='l')

h = ts(house$평균아파트매매가격.전국., start=c(2006,1),frequency = 12)
plot(stl(h, "periodic"))
#stl()함수를 이용하여 시계열 변동요인 시각화(시계열 분해) periodic주기적인



###시계열 분해와 변동요인 제거
h = decompose(h)
#decompose()함수를 이용 시계열 분해
par(mfrow=c(3,1))
plot(house$평균아파트매매가격.전국.-h$seasonal)
plot(house$평균아파트매매가격.전국.-h$trend)
plot(house$평균아파트매매가격.전국.-h$trend-h$seasonal)
#변동요인을 제거