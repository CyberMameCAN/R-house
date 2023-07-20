library(tidyverse)
library(ggplot2)

# 一日の死亡者数
n_die <- 0:6
# 日数
n_day <- c(50, 150, 145, 90, 45, 15, 5)
# 期待値
lambda <- sum(n_die * n_day) / sum(n_day)

# ラムダが最も分布に相応しい値と考えた時の分布
pois.density.exp <- dpois(n_die, lambda)
# 期待度数
pois.exp <- pois.density.exp * sum(n_day)

# プロット（赤：観測値、青：期待値）
yrange <- range(n_day, pois.exp)

df <- data.frame(n_day, n_die, pois.exp, stringsAsFactors = F)
# df <- melt(t(tmp[, 2:length(tmp)]))
# df <- melt(t(tmp))
df

g <- ggplot(data=df, aes(x=n_die))
g <- g + geom_point(aes(y=n_day), alpha=0.6)
g <- g + geom_point(aes(y=pois.exp), alpha=0.3)
g <- g + geom_line(aes(y=n_day, colour="n_day"))
g <- g + geom_line(aes(y=pois.exp, colour="pois.exp"))
g <- g + labs(title = "Comparison between acutual and expectation",
              x = "死亡者数",
              y = "日数")
g <- g + scale_color_hue(name = "凡例", labels = c(n_day = "観測値", pois.exp ="期待値"))
g <- g + ylim(0, 160)
plot(g)


# plot(n_die, n_day, ylim=yrange, col="red", ylab="Day")
# title("Comparison between acutual and expectation")
# par(new=T)
# plot(n_die, pois.exp, ylim=yrange, col="blue", ylab="")
# par(new=F)

# 適合度検定（カイ2乗検定）
pois.exp[length(pois.exp)] <- sum(n_day) - sum(pois.exp[1:6])
chisq.test(n_day, p=pois.exp/sum(n_day))

pois.exp
