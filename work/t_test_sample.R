n <- 10000
m <- 170
s <- 5.5

p <- rnorm(n, mean = m, sd = s)

hist(p)

x <- sample(p, 100)
hist(x)

mean(x)
sd(x)


dat <- iris
dim(dat)
head(dat)
str(dat)  # データの全体像の確認

dat1 <- subset(dat, dat$Species=="setosa" | dat$Species=="versicolor")
dim(dat1)
head(dat1)
colnames(dat1)
str(dat1)

### 対応のあるt検定
t.test(dat1$Sepal.Length, dat1$Sepal.Width, paired = TRUE)

### 対応のないt検定
# Welch
t.test(dat1$Sepal.Length ~ dat1$Species)

## 等分散を仮定
# 等分散の検定
var.test(dat1$Sepal.Length ~ dat1$Species)
# (上の結果で等分散は仮定できなかったが、等分散を仮定でのやり方は・・・)
t.test(dat1$Sepal.Length ~ dat1$Species, var.equal=TRUE)
