data(iris)
nn.iris <- nnet(Species~., data=iris, size=2, rang=0.1, decay=5e-4, maxit=200)
summary(nn.iris)
table(iris$Species, predict(nn.iris, iris, type="class"))

nrow(iris)
train = iris[1:80,]
test = iris[-(1:80),]
nn.iris = nnet(Species~., data = iris, size=2, rang=0.1, decay=5e-4, maxit=200)
summary(nn.iris) 
pre = predict(nn.iris, test, type = "class")
actual = test$Species
table(actual,pre)
