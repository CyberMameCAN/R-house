#df = c(56, 48, 78, 81, 86, 71, 72, 88, 46, 47, 89, 58, 43, 79, 48, 41)
#boxplot(df)
#sort(df)

x = c(19.2,18.8,20.1,20.7,19.5,21.0,22.6,21.6,22.9,23.6)
y = c(37.8,38.5,39.1,40.1,42.4,43.5,45.5,45.0,47.0,47.5)
df = data.frame(x, y)
summary(df)
plot(df)

fit = lm(y ~ x)
abline(fit, col="blue")
summary(fit)


# 以下、自分で計算してみる
x_mean = mean(x)
residuals(fit)  # 残差
coef(fit)  # 決定係数
s = sum((x - x_mean)^2)  # 偏差平方和
dev = deviance(fit) # 残差平方和
s2 = dev / (length(x) - 1 - 1)  # 残差の自由度　データ数 - 1 - 説明変数の数
s2  # シグマ2乗

t = 2 / sqrt(s2 / s)
t
# t分布表より5%有効を考えると-2.306 < t < 2.306であるため、帰無仮設は棄却域にある
