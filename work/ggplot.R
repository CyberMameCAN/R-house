# 参考
# ggplotを使用してデータを視覚化(Rプログラミング)
# https://www.youtube.com/watch?v=Er-tXfGkL08

library(tidyverse)
#data()  # いろいろなデータの種類が見れる
View(starwars)

starwars %>% 
  filter(hair_color == "black" | hair_color == "brown") %>% 
  drop_na(sex) %>% 
    ggplot(aes(hair_color, fill=sex)) +
    geom_bar(position = "dodge", alpha = 0.8) + # dodge ,fill とかグラフを変えてみる
    theme_bw() +                                # +を付けたり付けなかったりを試す
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Gender and Hair Colour",
         x = "Hair Colour",
         y = "Number")

starwars %>% 
  filter(hair_color %in% c("black", "brown")) %>% 
  drop_na(sex) %>% 
  ggplot(aes(x = sex)) +
  geom_bar(aes(fill=sex), alpha=0.7) +
  facet_wrap(~ hair_color) +
  # theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = "Gender and Hair Colour",
       x = "Hair Colour",
       y = "Number")

# ファセット関数
# facet_grid()  # 行と列それぞれに分ける要素を指定して, 行と列ごとに区分け
# facet_wrap()  # 要素を指定すると, 勝手にそれを2次元に区分け

str(diamonds)
p <- ggplot(diamonds, aes(x=carat, y=price))
p + geom_point(aes(colour=clarity)) + facet_grid(cut ~ color)

p <- ggplot(diamonds, aes(x=carat, y=price))
p + geom_point(aes(colour=clarity)) + facet_wrap(~ color, ncol=2)
