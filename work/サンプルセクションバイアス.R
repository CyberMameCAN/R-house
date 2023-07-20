#ディレクトリの設定
#setwd("somewhere")

install.packages('texreg')

#パッケージの読み込み
library(tidyverse)
library(texreg)

r2norm <- function(n, mu, sigma, rho) {
  tmp <- rnorm(n) 
  x   <- mu+sigma*tmp
  y   <- rho*x + sqrt(1-rho^2)*rnorm(n)
  return(data.frame(x=x,y=y))
}

set.seed(12345); population <- r2norm(100000, 0, 1, 0.3)

#偏差値の作成
population$hensachi <- population$x * 10 + 50

#偏差値70以上なら合格、能力2以上なら不合格
population$pass <- ifelse(population$y >= 2, "pass" , "fail")

#能力2以上の集団を作る
sample <- subset(population, y >= 2)

#全体サンプルにおける散布図
p1 <- ggplot(population, aes(x = hensachi, y = y)) + 
  geom_point(size = 0.5) + 
  xlim(10,90) + 
  ylim(-4,4) + 
  xlab("Hensachi") + 
  ylab("Ability") + 
  stat_smooth(method = "lm", se = FALSE, size = 1)

#偏差値70以上／未満で集団を分割したときの散布図
p2 <- ggplot(population, aes(x = hensachi, y = y, group = pass, color = pass)) + 
  geom_point(size = 0.5) + 
  xlim(10,90) + 
  ylim(-4,4) + 
  xlab("Hensachi") + 
  ylab("Ability") + 
  stat_smooth(method = "lm", se = FALSE, color = "Black", size = 1)

#回帰分析
reg1 <- lm(data = population, y ~ hensachi)
reg2 <- lm(data = sample, y ~ hensachi)

#能力2を閾値とした場合
population$pass2 <- ifelse(population$hensachi >= 70, "pass" , "fail")
sample2 <- subset(population, hensachi >= 70)

#能力2以上／未満で集団を分割したときの散布図
p3 <- ggplot(population, aes(x = hensachi, y = y, group = pass2, color = pass2)) + 
  geom_point(size = 0.5) + 
  xlim(10,90) + 
  ylim(-4,4) + 
  xlab("Hensachi") + 
  ylab("Ability") + 
  stat_smooth(method = "lm", se = FALSE, color = "Black", size = 1)

#回帰分析
reg3 <- lm(data = sample2, y ~ hensachi)

#png形式でggplotの図を出力
ggsave(file = "plot1.png", plot = p1, width = 5, height = 4)
ggsave(file = "plot2.png", plot = p2, width = 6, height = 4)
ggsave(file = "plot3.png", plot = p3, width = 6, height = 4)

#HTML形式で表を出力（texreg使用）
regs123 <- list(reg1, reg3, reg2)
htmlreg(regs123, digits = 4)
