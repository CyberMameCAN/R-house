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
res_t

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
# H0: 前後の平均が等しい
# H1: 前後の平均が等しくない

before <- c(591, 615, 602, 618, 596)
after  <- c(585, 590, 583, 594, 589)

t.test(before, after, paired = TRUE) 

# 手計算
diff <- before - after

mu_a <- mean(diff)
T <- mu_a / (sqrt(var(diff)/length(diff)))
T
t4_0025 <- 2.776  # 自由度4の5%有意水準、t分布・両側
mu_a - t4_0025 * sqrt(var(diff)/length(diff))
mu_a + t4_0025 * sqrt(var(diff)/length(diff))

# よって帰無仮設は棄却される（平均が等しいとは言えない）


t.test(diff, rep(0, 5), var.equal = FALSE)


x <- c(19.1, 20.8, 21.4, 17.8, 21.0, 20.4, 17.6, 19.7, 18.6)
y <- c(21.4, 20.7, 19.4, 23.1, 21.5, 21.0, 22.9, 19.9, 21.3, 18.8)
t.test(y, x, var.equal = FALSE)

d1 <- c(77, 45, 54, 53, 43)
d2 <- c(38, 49, 22, 42, 65, 34)
# H_0: sigma_1 == sigma_2, H_1: sigma_1 > sigma_2
sprintf("[mean] d1= %.1f, d2= %.1f", mean(d1), mean(d2))
sprintf("[var]  d1= %.3f, d2= %.3f", var(d1), var(d2))
#t.test(d1, d2, conf.level=0.95, var.equal = TRUE, alternative="greater")
t.test(d1, d2, conf.level=0.95, var.equal = TRUE)

prop.test(470, 1000, p=0.45, alternative ="two.side")

prop.test(1000, 5000, p=540/3000)
