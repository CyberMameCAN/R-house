#install.packages("datarium")
library(datarium)
mkt = as.data.frame(marketing)
head(mkt, 3)

# 図のパラメータを調整
par(
  las = 1,
  cex.lab = 1.5,
  cex.axis = 1.3
)

fit = lm(formula = sales ~ youtube, data = mkt)
summary(fit)

par(mar = c(4.5, 4.5, 1, 1))
plot(x = mkt$youtube, mkt$sales, xlab = "Youtube [unit: 1000 dollars]", ylab = "Sales")
abline(fit, col="#2792b3", lwd=4)

# 多項式回帰
#fit2 = lm(formula = sales ~ youtube + I(youtube^2), data=mkt)  # 二次関数
fit2 = lm(formula = sales ~ youtube + I(youtube^2) + I(youtube^3), data=mkt)  # 三次関数
summary(fit2)
plot(x=mkt$youtube, mkt$sales, xlab="Youtube [unit: 1000 dollars]", ylab = "Sales")
lines(sort(mkt$youtube), fitted(fit2)[order(mkt$youtube)], col="#2792b3", lwd=3)

install.packages("car")
library(car)

scatterplotMatrix(mkt, spread = F, smooth = F, regLine = list(col = "#2792b3", lwd = 3), col = "black", cex = .5)
cor(mkt)

# 重回帰モデル
#fit = lm(sales ~ youtube + facebook + newspaper, data=mkt)
fit = lm(sales ~ youtube + facebook + youtube:facebook, data=mkt)  # 交互作用
summary(fit)

install.packages("effects")
library(effects)

plot(effect("youtube:facebook", fit, , list(facebook = c(0, 30, 60))), multiline = T)

# 回帰の診断方法
library(car)

fit = lm(sales ~ youtube + facebook + newspaper, data=mkt)
qqPlot(fit, labels=row.names(mkt), id.method="identity", simulate = T)

fitted(fit)[131]
residuals(fit)[131]
rstudent(fit)[131]

# Q-Qプロットから131と6のデータが外れ値であるようなので、除外して再度Q-Qプロット
newfit = lm(sales ~ youtube + facebook + newspaper, data=mkt[-c(6, 131),])
qqPlot(newfit, labels=row.names(mkt), id.method="identity", simulate = T)

# 正規分布を確認するために誤差のヒストグラムを表示
residplot = function(fit, nbreaks=10) {
  z = rstudent(fit)
  hist(z, breaks=nbreaks, freq=F,
       xlab = "Studentized Residual", main = "Distribution of Errors")
  rug(jitter(z), col = "brown")
  curve(dnorm(x, mean=mean(z), sd = sd(z)), add = T, col = "blue", lwd = 2)
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2)
  legend("topleft", legend=c("Normal Curve", "Kernel Density Curve"), lty=1:2,
         col=c("blue", "red"), cex=.7)
}
residplot(fit)

# 独立性
car::durbinWatsonTest(fit) # p値>0.05なので自己相関があるとは言えない -> 独立性を示唆

# 線形性
par(mar=c(4.5, 4.5, 3, 1))
car::crPlots(fit)

# 等分散性
car::ncvTest(fit)
par(mar=c(4.5, 4.5, 2, 1))
spreadLevelPlot(fit)

# 診断
install.packages("gvlma")
library(gvlma)

summary(gvlma(fit))

# 多重共線性
car::vif(fit)
car::vif(fit) > 10  # 多重共線性の問題があるか

# 【異常値】
# 外れ値
car::outlierTest(fit)

# 高レバレッジ点
hat.plot = function(fit) {
  p = length(coefficients(fit))
  n = length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p / n, col = "red", lty = 2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

# 影響値
cutoff = 4 / (nrow(mkt) - length(fit$coefficients) - 2)
par(mar = c(6,5,2,1))
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

require(car)
avPlots(fit, ask = FALSE, id.method = "identify")

influencePlot(fit, id.method = "identify", main = "Influence Plot")

# 【対処方法】
# データの除外

# 変数変換
summary(powerTransform(mkt$sales))

# 無変換
fit = lm(sales ~ youtube + facebook + newspaper, data = mkt[-c(6, 131), ])
residplot(fit)
# 指数変換
fit = lm(sales^0.6 ~ youtube + facebook + newspaper, data = mkt[-c(6, 131), ])
residplot(fit)
# 平方根変換
fit = lm(sqrt(sales) ~ youtube + facebook + newspaper, data = mkt[-c(6, 131), ])
residplot(fit)
# 対数変換
fit = lm(log(sales) ~ youtube + facebook + newspaper, data = mkt[-c(6, 131), ])
residplot(fit)

library(car)
boxTidwell(sales ~ youtube + facebook + newspaper, data = mkt[-c(6, 128, 131), ])

# 指数変換はyoutube:0.47, FB:1.2, NP:6.2ですれば良いとわかるが、
# p値<0.05を満たしているのはfacebookだけ
fit = lm(sales ~ youtube + facebook + newspaper, data = mkt[-c(6, 131), ])
summary(fit)$adj.r.squared

fit = lm(sales ~ sqrt(youtube) + facebook + newspaper, data = mkt[-c(6, 131), ])
summary(fit)$adj.r.squared
# 変数変換したモデルの方がR^2値が大きいので、回帰モデルの当てはまりが良くなっている

# 変数の追加または削除

# 異なる分析方法を用いる

# 【ベストな回帰モデルを選ぶ】
# AICを用いた変数選択
# AICの値が小さいほど良いモデル（予測誤差が小さい）とされる
fit1 = lm(sales ~ youtube + facebook + newspaper, data = mkt[-c(6, 131),])
fit2 = lm(sales ~ youtube + facebook, data = mkt[-c(6, 131),])
fit3 = lm(sales ~ sqrt(youtube) + facebook, data = mkt[-c(6, 131),])
AIC(fit1, fit2, fit3)
# fit3がもっとも最良であると考えられる

# MASSパッケージのstepAIC()で最もAICが小さくなるモデルを探す
library(MASS)
fit = lm(sales ~ youtube + facebook + newspaper, data = mkt[-c(6, 131),])
stepAIC(fit, direction = "backward")

install.packages("leaps")
library(leaps)
leaps = regsubsets(sales ~ youtube + facebook + newspaper, data = mkt[-c(6, 131),], nbest = 3)
plot(leaps, scale = "adjr2")
