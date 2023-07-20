# Rと検定
# https://www1.doshisha.ac.jp/~mjin/R/Chap_12/12.html

(172.58 - 170.7) / sqrt(34.27/25)

# Ex) 平成15年度文部科学省の調査によると全国の17際男子の平均身長は170.7cmである。
# 某高校17際の男子25人について調査を行ったところ次に示すデータが得られたとする。
x <- c(171.6, 173.6, 167.6, 169.1, 183.0, 173.7, 168.3, 169.9, 182.3, 166.0, 172.8, 184.1, 158.9, 168.1, 168.5, 175.3, 179.6, 170.7, 173.1, 173.6, 169.1, 167.9, 177.8, 171.8, 178.1)
t.test(x, mu=170.7, alternative = "two.sided")

# 母比率の検定
# Ex) 現政権の支持率に関する調査では、現政権の支持率は45％であったが、新た(2回目)の調査では1000人の中で470人が現政権を支持するという結果が得られたとする。
#     2回の調査の現政権に対する支持率が同じであると言えるであろうか？
prop.test(470, 1000, p=0.45)

# 2標本の場合 平均の差の検定
x <- c(1,2,3,4,5)
y <- c(1,5,9,13,17)
var.test(x, y)

data(sleep)
sleep
var.test(extra ~ group, data=sleep)
# var.test()の結果より、2標本の平均の差の検定を行う際に等分散の設定を「var.equal = TRUE」にする根拠となる。
t.test(extra ~ group, var.equal = TRUE, data=sleep)

# 2標本の場合 比率の差の検定
# Ex) 現政権の支持率に関する2回の調査で、第1回では1000人の中450人が、第2回では1000人の中470人が支持するデータが得られたとする。
prop.test(c(450, 470), c(1000, 1000))

# カイ2乗検定
muna <- matrix(c(101,120,70,35,153,162,88,46,89,135,78,24), 3, 4, byrow=T)
muna
chisq.test(muna)


# スタビジ
# https://toukei-lab.com/t-test
# 
# この会社が製造しているバニラアイスの内容量は200mlに設定しているはずです。
# ある日、社長が「200mlになっていない気がする」と生産管理の担当者に伝えました。
# そこで、生産管理の担当者であるAさんとBさんは本当に200mlになっているかどうかを確かめることにしました。
# そして、製造した製品の中から無作為に10個ほど選んで内容量を測ったところ次のようになりました。
# 
# 205,198,197,208,204,202,207,199,207,203(ml)
# このデータから平均値を計算するとx¯=203でした。さて、設定は200mlからずれているのでしょうか。

b <- c(205,198,197,208,204,202,207,199,207,203)
mu <- 200  # 帰無仮設
t.test(b, mu=mu, alternative = "two.side", coef.level=0.95, var.equal = T)  # alternative=c("two.sided", "less", "greater")
# 結果は「帰無仮説を棄却」

# 2標本の平均の差
x1 <- c(-1, 2, 3, -4, 5, -4, 7, 9, 2, -3)
x2 <- c(-2, 4 ,6, -2, 6, -6, 4, 8, 5, 5)
t.test(x1, x2, var.equal = T)  # var.equal = 等分散性を仮定(Tを付けることで自由度dfが整数になった)
t.test(x1, x2, var.equal = F)  # ウェルチ Welch
var.test(x1, x2, var.equal = T)  # F検定
cor.test(x1, x2)  # 無相関検定(デフォルト：ピアソンの〜)
chisq.test(x1, x2)  # 帰無仮設「x1とx2に関係はない」p値は35%なので棄却できない

# その2
set.seed(1)
man1 <- 20
mu1 <- 85.4
x1 <- round(rnorm(20, mu1, sd=6), 1)
man2 <- 20
mu2 <- 81.2
x2 <- round(rnorm(man2, mu2, sd=12), 1)

library(ggplot2)

sample_data <- data.frame(x1, x2)
ggplot(data = sample_data) +
  geom_bar(mapping = aes(x=c(seq(1, man1)), y = x1), stat = "identity")
  
ggplot(data = sample_data) +
  geom_bar(mapping = aes(x=c(seq(1, man2)), y = x2), stat = "identity")

t.test(x1, x2, var.equal = T)

# カイ2乗検定(2群に有意な差があるかどうか？)
# 期待値と実際の値のズレが誤差内といえるかどうか
kai <- matrix(c(15, 5, 13, 7), 2, 2, byrow=T)
kai
chisq.test(kai)


# 独立性の検定
tab2 <- table(dat2)
chisq.test(ch1)


# 母分散の区間推定
install.packages('EnvStats')
library(EnvStats)

# x1 <- c(5.1, 4.7, 5.3, 5.2, 5.5, 4.8)
x1 <- c(84, 110, 79, 105, 131, 91)
# p <- c(0.3, 0.5, 0.1, 0.1)
varTest(x1)

ci <- 0.95
mu <- 9.9
ro <- 0.25
num <- 10
ch1 <- round(rnorm(num, mean = mu, sd = sqrt(ro)), 2)
mean(ch1)
varTest(x1)

