# 2標本による検定
# H0: 分散が等しい
# H1: 分散が等しくない

a = c(12.3, 12.9, 13.0, 13.4, 12.5, 13.1, 15.0, 12.8)
b = c(11.0, 11.4, 12.1, 11.5, 12.3, 12.4, 11.8, 11.6)

# 基礎調査
length(a)
sum(a)
mean(a)
var(a)
sd(a)
median(a)
# --
length(b)
sum(b)
mean(b)
var(b)
sd(b)
median(b)

# グラフ化
bunsan = data.frame(A=factor(c(rep("a",length(a)), rep("b", length(b)))),
                     No=factor(rep(1:length(a), 2)),
                     y=c(a, b))
boxplot(y~A, data=bunsan, col="lightblue")

# (要約)
#summary(aov(y~ A+No, bunsan))
t.test(a, b, var.equal = TRUE)

alpha = 0.05
na <- length(a)
nb <- length(b)
t.alpha.2.n <- qt(alpha / 2, df = na + nb - 2)
t.alpha.2.p <- qt(1 - alpha / 2, df = na + nb - 2)

# ウェルチの検定(異分散)
res_t = t.test(a, b, var.equal = FALSE)


# tの求め方(計算)
# 対応のない2群、母分散未知・小標本・等分散
Ua = sum((a - mean(a))^2)
Ub = sum((b - mean(b))^2)
U2 = (Ua + Ub) / (na + nb - 2)
T = (mean(a) - mean(b)) / sqrt(U2 * (1/na + 1/nb))
T

names(res_t)
res_t$statistic  # t値
res_t$parameter  # 自由度
res_t$p.value    # p値


# 対応ある2標本
before <- c(591, 615, 602, 618, 596)
after  <- c(585, 590, 583, 594, 589)

t.test(before, after, paired = TRUE)

diff <- after - before
t.test(diff, rep(0, 5), var.equal = FALSE)
