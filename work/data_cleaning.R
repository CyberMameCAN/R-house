# Explore
# Clean
# Manipulate
# Describe and summarise
# Visualise
# Analyse

###############################################
############### CLEANING DATA #################
# https://www.youtube.com/watch?v=sV5lwAJ7vnQ #
###############################################

install.packages("tidyverse")
library(tidyverse)
data()  # いろいろなデータの種類が見れる
view(starwars)

# Variable type

glimpse(starwars)  # カラムの情報、各データの姿？
class(starwars$gender)  # "character"
unique(starwars$gender)  # "masculine" "feminine"  NA

starwars$gender <- as.factor(starwars$gender)  # fillna
class(starwars$gender)  # "factor"

levels(starwars$gender)

starwars$gender <- factor((starwars$gender),
                          levels = c("masculine",
                                     "feminine"))
levels(starwars$gender)

# Select variables

names(starwars)  # カラム表示

starwars %>%   # Shift + Ctrl + m で %>% 入力
  select(name, height, ends_with("color")) %>%  # 正規表現？終わりがcolorになっているカラムを指定
  names()

unique(starwars$hair_color)

starwars %>% 
  select(name, height, ends_with("color")) %>% 
  filter(hair_color %in% c("blond", "brown") &
           height < 180)

# Missing data

# mean(starwars$height)  # NAがあるから駄目
mean(starwars$height, na.rm = TRUE)

starwars %>% 
  select(name, gender, hair_color, height)
  
starwars %>% 
  select(name, gender, hair_color, height) %>% 
  na.omit()

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(complete.cases(.))

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.))

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) %>% 
  drop_na(height)  # heightのNAを削除

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  drop_na(height) %>% 
  View()

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color = replace_na(hair_color, "none")) # noneで置き換え

starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color2 = replace_na(hair_color, "none")) # 新しくhair_color2へnoneを置き換えて

# Duplicates

Names <- c("Peter", "John", "Andrew", "Peter")
Age <- c(22, 33, 44, 22)

friends <- data.frame(Names, Age)
# friends[ , ]
# duplicated(friends)  # FALSE FALSE FALSE  TRUE

friends[!duplicated(friends), ]

friends %>%
  distinct() %>% 
  View()

# Recoding variables
starwars %>% 
  select(name, gender) %>% 
  mutate(gender = recode(gender,
                         "masculine" = 1,
                         "feminine" = 2)) %>%   # ダミー変数化？
  View()
