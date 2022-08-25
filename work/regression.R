df <- airquality
View(df)

plot(df)
attach(df)  # いちいちdfと書かなくて済む
a <- lm(Ozone ~ Solar.R+Wind+Temp)  # lm(目的変数 ~ 説明変数) 回帰分析

# Residuals ：         残差（予測値と観測値の差）
# Median ：            中央値
# Coefficients ：      係数（aとかbのこと）
# Estimate ：          推定値（aとかbの推定値のこと）
# Std.Error（Standard Error） ： 標準誤差
# t value ：            t値
# Pr(>|t|) ：           p値
# Intercept ：          切片
# Signif. codes ：      有意性
# Residual standard error ： 残差の標準誤差
# degrees of freedom ： 自由度
# Multiple R-squared ： 決定係数
# Adjusted R-squared ： 自由度修正済み決定係数
# F-statistic ：        F検定
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

#
# https://www.youtube.com/watch?v=626cH__LBI8&list=PLrfN_7wvptdU6-tkZzykDu8cA_hVjabKp&index=12
# マジでゼロから始める統計生活！第六回『Rで回帰分析！lm関数を使ってみよう！！』※説明欄も見てね。
#
dat <- iris
head(dat)
colnames(dat)
dim(dat)
str(dat)

# がくの長さをがくの幅で説明する単回帰分析
# 標準化
dat_z <- apply(dat[, 1:4], 2, scale)
head(dat_z)
str(dat_z)
# 確認すると、dat_zはDataFrameではない。lmはDataFrameを使うので変換する必要がある。
dat_z <- data.frame(dat_z)
str(dat_z)

res1 <- lm(Sepal.Length ~ Sepal.Width, data=dat_z)
summary(res1)

## 重回帰分析
# がくの長さを、がくの幅と花弁の長さと幅で説明する
res2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=dat_z)
summary(res2)

# 目的変数を除く、残りの変数全部が説明変数なら y ~ . でイケる
res3 <- lm(Sepal.Length ~ ., data=dat_z)
summary(res3)

# 交互作用項は、アスタリスク(*)で繋げる
# 重回帰分析で交互作用を検討する時は、
# 変数の標準化ないし中心化を忘れずに（平均を0にしておけばオケ）。
# 多重共線性に引っかかる。
res4 <- lm(Sepal.Length ~ Sepal.Width*Petal.Length*Petal.Width, data=dat_z)
summary(res4)
# 一番大きい交互作用項を書くだけで、
# それ以下の項は全て組み込まれる。
# 今回は3要因の交互作用項を書いたので、
# それ以下の項、つまり2要因の交互作用項3つと、
# 単体の項は自動的に組み込まれた。

# 交互作用項入りのモデルで、説明率（決定係数）は増加したのか？
# 階層的重回帰分析（っぽいこと）をしてみる。
anova(res1, res2)
# この2つのモデルには優位な差があった、と言える。
# F(2, 146)=436.17, p<.001

# 3要因の交互作用モデル
anova(res2, res4)
# F(4, 142)=1.6164, p=.173
# 上記の結果、res1 < res2 = res4
# 交互作用項を入れても説明率は変わらないことが分かった

#
# https://www.youtube.com/watch?v=71KI4MKmy7Y&list=PLrfN_7wvptdU6-tkZzykDu8cA_hVjabKp&index=13&t=310s
# マジでゼロから始める統計生活！パス解析編『R　lavaanパッケージでパス解析をやってみよう！』※説明欄も見てね。
#
# 共分散構造分析、構造方程式モデリング
#install.packages("lavaan")
library(lavaan)

dat1 <- HolzingerSwineford1939
head(dat1)

# パス解析の練習用データ
dat2 <- PoliticalDemocracy
head(dat2)
# Factor1 -> x1, x2, x3
# Factor2 -> y1, y2, y3, y4
# Factor3 -> y5, y6, y7, y8
# Factor1はFactor2, 3を説明するモデル
# Factor2はFactor3   を説明するモデル

# モデルの定期
model1 <- {
  "
  # 洗剤変数の定義
  # =~ で洗剤変数を定義できる
  # 左辺に因子名を、右辺に項目を、+で繋ぐ
  pow =~ x1 + x2 + x3
  dem1 =~ y1 + y2 + y3 + y4
  dem2 =~ y5 + y6 + y7 + y8
  
  # 回帰式(潜在変数感の関連)
  # 目的変数 ~ 説明変数 と記述する
  # 重回帰モデルなら、説明変数を + で繋ぐ
  dem1 ~ pow
  dem2 ~ pow + dem1
  "
}

# SEMの実行
res2 <- sem(model1, data = dat2)
summary(res2, fit.measures="TRUE") # 適合度指標の表示オプション

# モデルの修正
modindices(res2)  # 修正指標の表示
# mi の項目に注目するのが良い
modindices(res2)[order(modindices(res2)$mi, decreasing = TRUE), ]

# パス図の書き方
#install.packages("semPlot")
library(semPlot)

semPaths(res2, "par")
