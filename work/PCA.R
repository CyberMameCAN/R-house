iris <- iris
iris$Sepal.Length
View(iris)
x <- iris$Sepal.Length
x
mean(x)
median(x)
cor(iris$Sepal.Length, iris$Petal.Length, method = "pearson")
plot(iris$Sepal.Length, iris$Petal.Length)

# Regression - Linear Regression Model
model <- lm(iris$Pepal.Length ~ iris$Setal.Length, data = iris)
summary(model)

abline(lm(iris$Petal.Length ~ iris$Sepal.Length, data = iris))

# t-検定
data1 <- subset(iris$Petal.Length, iris$Species=="setosa")
data2 <- subset(iris$Petal.Length, iris$Species=="virginica")
t.test(data1, data2)

aov.model <- aov(Petal.Length ~ Species, data=iris)
View(aov.model)
summary(aov.model)

# 図示
#install/packages(pheatmap)
library(pheatmap)
pheatmap(iris[, 1:4],
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         clustering_method = "complete")

# PCA
#install.packages(c("FactMineR", "factoextra"))
library(FactoMineR)
library(factoextra)
iris.pca <- PCA(iris[, 1:4], scale.unit = TRUE, ncp = 5)
fviz_pca_ind(iris.pca, col.ind = iris$Species)
