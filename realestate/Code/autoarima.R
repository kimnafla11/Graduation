house = read.csv('c:/house7.csv')

names(house)
#변수명만 출력


house = house[,c(-1,-2)]

library(forecast)

h_ar = auto.arima(house$평균아파트매매가격.전국.)

fitted.values(h_ar)

plot(house$평균아파트매매가격.전국.,type='l')
points(h_ar$fitted,col='red',type='l')

fitted(h_ar)
points(fitted.values(h_ar),col='red',type='l')

plot(forecast(h_ar,3))
