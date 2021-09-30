kang = read.csv('Data/kang.csv')
kang = kang[,-(1:2)]
names(kang)
kang = na.omit(kang)

##서울 부동산 차분
diff_kang = matrix(0,nrow(kang),ncol(kang))

for(i in 1:ncol(kang)){
  diff_kang[1:nrow(as.matrix(diff(kang[,i]))),i]=as.matrix(diff(kang[,i]))
}

write.csv(diff_kang,file='Output/Data/diff_kang.csv')
