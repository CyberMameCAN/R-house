df <- airquality
View(df)

plot(df)
#attach(df)  # いちいちdfと書かなくて済む
a <- lm(Ozone ~ Solar.R+Wind+Temp)  # lm(目的変数 ~ 説明変数) 回帰分析
summary(a)
a

# CSVで出力
coe <- a$coefficients  # 回帰係数を抽出
aic <- AIC(a)  # AICを抽出
N <- length(df$Ozone)  # データの総数
result <- cbind(coe, aic, N)  # 結果をまとめる

# Ozon = 0.06 * Solar.R - 3.33 * Wind + 1.65 * Temp - 64.34
0.06 * 299 - 3.33 * 8.6 + 1.65 * 65 - 64.34  # -> 32.212

names(df)
df$Ozone
mean(df$Ozone, na.rm = TRUE)
v <- var(df$Ozone, na.rm = TRUE)
sqrt(v)
sd(df$Ozone, na.rm = TRUE)  # 標準偏差の関数

df_rmna <- df %>% 
  drop_na(Ozone)
View(df_rmna)

oz <- df_rmna$Ozone
te <- df_rmna$Temp
f <- lm(oz ~ te)
print(f)
plot(oz, te)
abline(f, col="blue")
