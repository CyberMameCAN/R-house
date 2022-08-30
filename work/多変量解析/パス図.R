#
# https://www.youtube.com/watch?v=5nwdnFQ5KPc&t=27s
# 【Rで多変量解析#13】パス解析
#
library(dplyr)
library(ggplot2)
library(lavaan)
library(semPlot)

set.seed(111)
線形代数 <- rnorm(100, 50, 10) %>%
  round()
微分積分 <- rnorm(100, 50, 10) %>%
  round()
機械学習 <- rnorm(100, 0.7*線形代数+0.3*微分積分, 5) %>%
  round()
NN <- rnorm(100, 0.2*線形代数+0.8*微分積分, 5) %>%
  round()
data <- data.frame(線形代数, 微分積分, 機械学習, NN)
data <- apply(data, 2, scale)

data %>%
  head()

# 分析結果
# 関係の設定
data.model <-'
機械学習 ~ 線形代数+微分積分
NN ~ 線形代数+微分積分'
model <- sem(data.model, data=data)
summary(model)
par(family="HiraKakuProN-W3")
# パス図の描画
semPaths(model, "model", "est", rotation = 2,
         mar = c(5, 8, 5, 14), # 余白の指定
         sizeMan = 15,
         sizeLat = 15,
         style = "lisrel",
         curve = 3,
         nCharNodes = 4
         )
