# 参考
# ggplotを使用してデータを視覚化(Rプログラミング)
# https://www.youtube.com/watch?v=Er-tXfGkL08

library(tidyverse)
#data()  # いろいろなデータの種類が見れる
View(starwars)

starwars %>% 
  filter(hair_color == "black" |
           hair_color == "brown") %>% 
  drop_na(sex) %>% 
  ggplot(aes(hair_color, fill=sex))+
  geom_bar(position = "dodge", alpha = 0.8)+ # dodge ,fill とかグラフを変えてみる
  theme_bw()+                                # +を付けたり付けなかったりを試す
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Gender and Hair Colour",
       x = "Hair Colour",
       y = "Number")

starwars %>% 
  filter(hair_color %in% c("black", "brown")) %>% 
  drop_na(sex) %>% 
  ggplot(aes(x = sex))+
  geom_bar(aes(fill=sex), alpha=0.5)+
  facet_wrap(~ hair_color)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  labs(title = "Gender and Hair Colour",
       x = "Hair Colour",
       y = "Number")
