# N(170, 5^2)
μ = 170
σ = 5
n = 300

X = rnorm(n, μ, σ)  # 正規分布に従うn個の数値を返す
Z = (X - mean(X))/ sqrt(var(X))
mean(Z)
var(Z)
# ここまで確認作業

# 
result = matrix(0, 1000, 300)
for(i in 1:1000) {
  result[i,] = rnorm(n, μ, σ) 
}
temp = apply(result, 1, mean)
mean(temp)
var(temp)  # σ^2 / n に近い値が出ている
# つまり、正規分布から抽出した標本平均は正規分布に従う、ということ(定理)

# 区間推定
# 有意水準α=0.05, n=10のt分布
qt(0.05/2, 10-1)

# 母比率の信頼区間
# 試行、実験、調査などで、ある観測項目が現れるか、現れないかに関する結果は二項分布B(n, p)に従う。
x = 0:30
plot(x, dbinom(x, 50, prob=0.3), type="h")
sd1 = sqrt(50*0.3*0.7)
curve(dnorm(x, mean=0.3*50, sd=sd1), add=T)  # 標本サイズが大きい場合（50個）、正規分布に非常に近い

# Ex) 1000人に対して行った調査で、ある政権の支持率が45%だとする。有意水準5%(α=0.05)の場合の母比率の信頼区間の計算
z = abs(qnorm(0.05/2))
under = 0.45 - z * sqrt(0.45*(1-0.45)/1000)
upper = 0.45 + z * sqrt(0.45*(1-0.45)/1000)
under = round(under, 4)
upper = round(upper, 4)
sprintf("母集団の支持率はおおよそ %.4f 〜 %.4f の間と推測", under, upper)

# 以下テスト
integrate(dnorm,-Inf,Inf) # 積分

