#
# Python と R の違い (k-NN 法による分類器)
# https://pythondatascience.plavox.info/python%e3%81%a8r%e3%81%ae%e9%81%95%e3%81%84/k-nn%e6%b3%95
#
#install.packages("kknn")

library(kknn)

data(iris)

trainset <- iris[1:149,]
testset <- iris[150,][-5]

knn.model <- kknn(Species ~ ., trainset, testset)
summary(knn.model)

fitted(knn.model)
