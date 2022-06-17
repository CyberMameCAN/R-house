#install.packages("ineq")
library(ineq)

income <- c(0, 0, 0, 600)
Lc(income, plot = TRUE)  # ローレンツ曲線
Gini(income)  # ジニ係数の計算 0.75 * 1 / 2 * 2

# ポアソン分布
# 一定期間の中で稀にしか起こらない現象を起こす対象を大量に観測した時に、その現象が起きた回数が従う分布
# Ex1) 1年間にプロイセン陸軍の兵士が馬に蹴られて死んだ数
# Ex2) 1時間あたりの電話がかかってくる件数

# 死亡者数
nDie <- 0:6
# 日数
nDay <- c(50, 150, 145, 90, 45, 15, 5)
# 期待値の計算
lambda <- sum(nDie * nDay) / sum(nDay)  #-> 1.99
# lambdaが最も分布に相応しい値と考えた時の分布
pois.density.exp <- dpois(nDie, lambda)
# 期待度数
pois.exp <- pois.density.exp * sum(nDay)
# プロット（赤が観測値、青が期待値）
yrange <- range(nDay, pois.exp)
plot(nDie, nDay, ylim=yrange, type="l", col="red", ylab="Day")
title("Comparison between acutual and expectaion")
par(new=T)
plot(nDie, pois.exp, ylim=yrange, type="l", col="blue", ylab="")
par(new=F)

# 期待度数が５未満のセルはまとめる
pois.exp[length(pois.exp)] <- sum(nDay) - sum(pois.exp[1:6])
# カイ２乗検定(nDayが観測値、pがデータが従うとされる確率分布)
chisq.test(nDay, p=pois.exp/sum(nDay))
# chisq.testを使わないで検定統計量を求める
chisq.stat <- sum((nDay - pois.exp)^2/pois.exp)
print(chisq.stat)
