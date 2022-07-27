# Rと推定
# https://www1.doshisha.ac.jp/~mjin/R/Chap_11/11.html

X = rnorm(300, 170, 5)  # N(170, 5^2)のデータを300個
Z = (X - mean(X)) / sqrt(var(X))
mean(Z)

kekka = matrix(0, 1000, 300)
for(i in 1:1000) { kekka[i,] = rnorm(300, 170, 5)}
temp = apply(kekka, 1, mean)
mean(temp)
var(temp)  # 5^2 / 300 とほぼ同じ値

# 区間推定

# 確率変数 k が1、2、3、…、30をとる二項分布 B (50,0.3) のグラフを作成し、
# さらに同じの座標上で平均が15 = 50*0.3、分散が10.5 = 50*0.3*0.7である正規分布のグラフを作成
x = 0:30
plot(x, dbinom(x, 50, prob = 0.3), type = "h")
sd1 = sqrt(50 * 0.3 * 0.7)
curve(dnorm(x, mean=0.3*50, sd=sd1), add=T)

# Ex) 1000人に対して調査を行った結果、現政権に対する支持率が45％だとする。
#     有意水準5％ (α = 0.05) の場合の母比率の信頼区間の計算結果を次に示す。

n = 1000
a = 0.05 / 2
p = 0.45
z = abs(qnorm(a))
p - z * sqrt(p * (1 - p) / n)  #-> 約42%
p + z * sqrt(p * (1 - p) / n)  #-> 約48%
# 母集団の支持率はおよそ42~48%と推測される。
