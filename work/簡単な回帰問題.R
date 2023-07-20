# 回帰分析

tall <- c(154, 160, 163, 169, 172, 180, 184)
comein <- c(22, 25, 30, 24, 24, 29, 35)

A = data.frame(tall, comein)
summary(A)


# 帰無仮設：傾き0となる
# 対立仮設：ならない

t.test(x=A$tall, y=A$comein, conf.level=0.95, var.equal=TRUE)

fit = lm(formula = A$comein ~ A$tall)  # lm(目的変数 ~ 説明変数)
# p値が小さいほど、この係数は有効と判断できる
summary(fit)

plot(x=A$tall, y=A$comein, xlab = "身長", ylab = "収入", xlim = c(145, 200), ylim = c(10, 40))
abline(fit, col="#2792b3", lwd=2)

coefficients(fit)  # 切片と傾き
cor(A$comein, A$tall)  # 相関係数

pre = predict(fit)    # 予測値
res = residuals(fit)  # 残渣
img = data.frame(A, pre, res)
summary(img)
str(img)  # データ構造の確認
class(img)  # データ型の確認 #> data.frame
class(tall) #> numeric
mode(img)

#
# 高校生のためのRによる回帰分析
# http://www.f.waseda.jp/takezawa/math/joho/regression.pdf
#
# Rのデータセットを呼び出し
datasets::cars
str(cars)
class(cars)
y = cars$dist
x = cars$speed

cor(x, y)
fit = lm(formula = y ~ x )
coefficients(fit)
summary(fit)
nlm1 = nls(y ~ a + b*x + c*x^2 + d*x^3, start = c(a=1, b=1, c=1, d=1), trace = T)
summary(nlm1)
# p値を見るとaとcの値が比較的高い -> 無視してみる
# 再モデル化
nlm2 = nls(y ~ b*x + d*x^3, start = c(b=1, d=1), trace = T)
summary(nlm2)

plot(x, y)
lines(x, fitted(nlm1), lty=1, col="#ff0000")
lines(x, fitted(nlm2), lty=1, col="#0000cc")


#
#
datasets::airquality
str(airquality)
# 欠損値の削除
table(is.na(airmiles))
airq = na.omit(airquality)
round(cor(airq), 3)  # 小数第3位
# まずはグラフ
pairs(airq)
# 多変量モデルの作成
lm1 = lm(Solar.R ~ Ozone + Wind + Temp, data=airq)
summary(lm1)
# 回帰診断図
par(mfrow=c(2,2))  # 2x2図を指定
plot(lm1)
# Solar.R と Ozone に着目する
lm = lm(airq$Solar.R ~ airq$Ozone)
summary(lm)
plot(airq$Ozone, airq$Solar.R)
abline(lm, lwd=2)

