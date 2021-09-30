house = read.csv('c:/house7.csv')
names(house)
#변수명만 출력
house = house[,c(-1,-2)]
house = na.omit(house)
source("C:/Users/kimna/Documents/house/function/DIFFMOVINGWINDOW_FUNCTION.r")

diff_mat = matrix(0,nrow(house),ncol(house))
#빈 매트릭스 만듦
#matrix(매트릭스에 들어갈 숫자, 가로줄 수, 세로줄 수)
#nrow(변수명) : 관측치 수 반환
#ncol(변수명) : 변수 수 반환
diff_mat = as.data.frame(diff_mat)
for(i in 1:ncol(house)){
  
  diff_mat[1:nrow(as.matrix(diff(house[,i]))),i]=as.matrix(diff(house[,i]))
}

dmw = mw(diff_mat,32)
plot(diff_mat[,17],type='l')
points(dmw,type ='l',col='red')
