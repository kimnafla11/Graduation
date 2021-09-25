house = read.csv('c:/house7.csv')
names(house)
house = house[,-(1:2)]
house = na.omit(house)
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

#렌포 모델 생성
model = randomForest(house$평균아파트매매가격.전국.~., data = house)
model

#랜포 파라미터 조정
model2 = randomForest(house$평균아파트매매가격.전국.~., data = house, ntree = 300, mtry = 4, na.action= na.omit)
model2                      
#랜포 수 300개, 자식노드 분류 4개, naomit으로 결측치 제거


#중요변수 추출
model3 = randomForest(house$평균아파트매매가격.전국.~., data = house, importance=T, na.action = na.omit)
importance(model3)
varImpPlot(model3)
