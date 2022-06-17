library(tidyverse)
View(starwars)

glimpse((starwars))
class(starwars$eye_color)
#unique(starwars$eye_color)
#rle(starwars$eye_color)
table(starwars$eye_color)  # 出現頻度

eyes = c("black", "blue", "blue-gray", "brown", "dark", "gold", "green, yellow",
         "hazel", "orange", "pink", "red", "red, blue", "unknown", "white", "yellow")
eyes_count = c(10, 19, 1, 21, 1, 1, 1, 3, 8, 1, 5, 1, 3, 1, 11)

eyes_df = data.frame(eyes, eyes_count)
#duplicated(eyes_df)
eyes_df %>% 
  distinct() %>% 
  View()
n = sum(eyes_df$eyes_count)
chisq.test(eyes_count)

#install.packages("pwr")
library(pwr)
# 効果量(w)の計算
p0 = c(1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15, 1/15)  # 期待比率(15のカテゴリーだから)
p1 = eyes_count / n
w = ES.w1(p0, p1)  # 効果量
w  # 効果量ｗの評価：大=0.5, 中=0.3, 小=0.1

#
# χ2乗検定
# https://data-science.gr.jp/implementation/ist_r_chi_squared_test.html#:~:text=R%20%E3%81%AB%E3%81%A6%E3%82%AB%E3%82%A4%E4%BA%8C%E4%B9%97,%E3%82%92%E6%8C%87%E3%81%99%E5%A0%B4%E5%90%88%E3%81%8C%E5%A4%9A%E3%81%84%EF%BC%8E
#
# 適合度検定
# 帰無仮説 (H0) は，観測された頻度分布と期待される頻度分布に差はない (適合している) 
vx = c(58,99,32,48)
chisq.test(x=vx, p=c(0.3,0.5,0.1,0.1))
# p値は有意水準を大きく下回るので帰無仮設を棄却

# 独立性検定
# 変数A = [43, 20, 28], 変数B = [15, 31, 11]
# 帰無仮説 (H0) は，変数Aと変数Bに関連性はない (独立である) 
vx = matrix(c(43, 20, 28, 15, 31, 11), nrow=2, byrow=T)
chisq.test(vx)
# p値は有意水準を大きく下回るので，帰無仮説を棄却
# 変数AとBは何らかの関係性があると結論する