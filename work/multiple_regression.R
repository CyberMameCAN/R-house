# Rと重回帰分析
# https://www1.doshisha.ac.jp/~mjin/R/Chap_15/15.html

体重<- c(50,60,65,65,70,75,80,85,90,95)
身長<- c(165,170,172,175,170,172,183,187,180,185)
ウエスト<- c(65,68,70,65,80,85,78,79,95,97)

taikei2 <- data.frame(体重, 身長, ウエスト)
taikei2

# 相関行列と対散布図を求める
round(cor(taikei2), 4)
pairs(taikei2, pch=21, bg="red", cex=2)

# 体重を目的変数にする重回帰分析をする
(taikei2.lm <- lm(体重~., data=taikei2))

# 当てはまりの良さを確認
summary(taikei2.lm)

par(mfrow=c(2,2), oma=c(1,1,2,1), mar=c(4,4,2,1))
plot(taikei2.lm, pch=21, col=2, cex=1.5)

# 相互作用モデル

(taikei2.lm2<-lm(体重~.^2, data=taikei2))
summary(taikei2.lm2)

plot(taikei2.lm2, pch=21, bg=2, col=2, cex=1.5)

# 変数・モデルの選択
# AIC
data(attitude)
attitude[1,]
