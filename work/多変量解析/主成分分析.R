#
# 【Rで多変量解析#5】主成分分析
# https://www.youtube.com/watch?v=5ModcVDwuTg&t=0s
#
library(dplyr)
library(ggplot2)

# 2変数
set.seed(111)
英語 <- rnorm(100, 50, 10) %>%
  round()
数学 <- rnorm(100, 英語, 10) %>%
  round()
data <- data.frame(英語, 数学)
data %>%
  head()

data %>%
  ggplot() + 
  geom_point(aes(英語, 数学)) +
  theme_classic(base_family = "HiraKaKuPro-W3") +
  theme(text = element_text(size=30)) +
  labs(title = "データプロット")

model <- prcomp(data, scale = TRUE)
summary(model)
model

# 主成分得点
z <- model$rotation[,1] %*%
  t(scale(data)) %>%
  round(2) %>%
  t() %>%
  as.data.frame()
colnames(z) <- "z"
z %>%
  head()

# 多変量
set.seed(11)
英語 <- rnorm(100, 50, 10) %>%
  round()
数学 <- rnorm(100, 50, 10) %>%
  round()
国語 <- rnorm(100, 英語, 5) %>%
  round()
理科 <- rnorm(100, 数学, 5) %>%
  round()
社会 <- rnorm(100, 国語, 3) %>%
  round()
data <- data.frame(英語, 数学, 国語, 理科, 社会)
data %>% head()

model <- prcomp(data, scale=TRUE)
summary(model)
biplot(model, family="HirakakuPro-W3")

# 主成分得点
z <- t(model$rotation[, 1:2]) %*%
  t(scale(data)) %>%
  round(2) %>%
  t() %>%
  as.data.frame()
colnames(z) <- c("z1", "z2")
z %>%
  head()
