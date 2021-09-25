house = read.csv('c:/house7.csv')
names(house)

tsdata = ts(na.omit(house$평균아파트매매가격.전국.), start=c(2006,1),frequency = 12)
#timeseries데이터 변환 2006년 부터 1월까지

tsdata = diff(tsdata)
#정상성 만족하기 위해 1차 차분
plot(tsdata)

install.packages("forecast")#auto arima하려고 패키지 설치
library(forecast)#라이브러리 불러오기

arima = auto.arima(tsdata) #auto.arima()함수를 이용
arima


model=arima(tsdata, order=c(2,0,1))
model

tsdiag(model)#잔차분석을 통한 arima모형 진단

##arima모델로 예측
fore = forecast(model)#향후 2년예측
par(mfrow=c(1,2))
plot(fore)#향후 2년 예측치 시각화
model2 = forecast(model,h=6)#향후 6개월 예측치 시각화
plot(model2)
