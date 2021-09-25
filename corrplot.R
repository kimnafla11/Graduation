house = read.csv('c:/house7.csv')
names(house)
house = house[,c(-1,-2)]
library(corrplot)

corrplot(cor(na.omit(house)),tl.srt=45)

house = na.omit(house)


difh = matrix(0,nrow(house),ncol(house))

for(i in 1:ncol(house)){
  a= nrow(as.matrix(diff(house[,i])))
  difh[1:a,i]=as.matrix(diff(house[,i]))
  
  #다변량이라서 i~nrow까지 i번째 열에관측치 수만큼
}

corrplot(cor(difh),tl.srt=45)



#install.packages("psych")
library(psych)
na.omit(house)
coh = corr.test(house, use = 'complete',method = 'pearson',adjust = 'none')
coh$p


library(Hmisc)
house = na.omit(house)
rcorr(as.matrix(house),type="pearson")
