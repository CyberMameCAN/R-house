# pipe %>% でエラーが出るのでインストールすれば解決するらしい
# [エラー内容] could not find function "%>%"
# インストール時のメッセージで「No」を選択するとインストールが成功した。
install.packages("dplyr")
install.packages("reshape2")
install.packages("rmarkdown")

library(readr)
library(ggplot2)
library(ggsci)
library(reshape2)
library(dplyr)

# race_data <- read_csv("project/Keiba/lap-time.csv")
race_data <- read_csv("project/Keiba/takaraduka-lap-time.csv")
#View(race_data)
glimpse(race_data)

get_max_halon_number <- function(race_data, col_number=19) {
  # 数値のみ抜き出す
  # Ex) "last10F" >>> 10へ変換する 
  tmp_ <- colnames(race_data)[col_number]
  tmp_ <- gsub("last", "", tmp_)
  tmp_ <- gsub("F", "", tmp_)
  return(as.numeric(tmp_))
}

columns <- c("馬名")
num <- get_max_halon_number(race_data)
for(i in seq(num, 1)) {
  #zを作って、それぞれのx1の集計を入れる
  #assign(paste("last", i, "F", sep=""), i*i)
  columns <- append(columns, paste("last", i, "F", sep=""))
  # columns <- append(columns, i)
}

preprocessing = function(horse_name) {
  a_horse <- filter(race_data, 馬名==horse_name)[columns]
  # columns[2:length(columns)]
  # a_horse <- race_data[1:3, columns]
  
  df <- melt(t(a_horse[, 2:length(a_horse)]))
  df$halon_name = rep(num:1, 3)
  
  halon <- num:1
  # df <- data.frame(
  #   halon_name=halon,
  #   # one_halon_time=t(a_horse[2, 2:length(a_horse)])
  #   one_halon_time=t(a_horse[, 2:length(a_horse)])
  # )
  # df
  
  # x <- matrix(t(a_horse[, 2:length(a_horse)]), ncol=3)
  # rownames(x) <- 17:1
  # colnames(x) <- c("前走", "2走前", "3走前")
  # y <- melt(x)
  # colnames(y) <- c("day", "treat", "weight")
  # y
  
  return(df)
}


figout = function(df, pic_save=FALSE) {
  g <- ggplot(data=df, mapping=aes(x=halon_name, y=value, group=Var2, color=Var2))
  # g <- g + xlim(17, 1)
  # g <- g + scale_x_continuous()
  g <- g + scale_x_reverse(breaks=rep(1:num, 1))
  g <- g + ylim(13.5, 10.0)
  # g <- g + scale_y_reverse()
  # g <- g + scale_y_continuous(breaks=seq(10.0, 13.5, length=8), limits=c(13.5, 10.0))
  g <- g + geom_point(alpha=0.3, aes(colour=factor(Var2)))
  g <- g + geom_line(aes(colour=factor(Var2)))
  # g <- g + geom_line(aes(y=one_halon_time.1), color="#009900")
  # g <- g + geom_line(aes(y=one_halon_time.2), color="#006699")
  # g <- g + geom_line(aes(y=one_halon_time.3), color="#990066")
  # g <- g + scale_color_nejm()
  g <- g + geom_smooth(method="lm", linetype = "dashed", size=0.5, se=FALSE, aes(colour=factor(Var2)))
  g <- g + scale_color_manual(values=c("red", "deepskyblue3", "#666666"),
                              labels=c("前走", "2走前", "3走前"),
                              name = "過去レース")
  # g <- g + scale_fill_manual(                  # fillの色をマニュアル指定する（バー内部の色）
  #               labels = c(1 = "前走",          # レジェンドの表記を変更（"="を使い間違いをなくす）
  #                          2 = "2走前",
  #                          3 = "3走前",
  #                          "Missing"),
  #               name = "過去レース"                  # レジェンドタイトル
  #               # na.value = "grey"                 # 欠測地の色を指定
  #   )
  g <- g + labs(title = paste("<過去3走ラップタイム> ", horse_name),
                x = "残りハロン >>ゴール側 >>",
                y = "1F タイム(秒)")
  g <- g + theme_grey()
  plot(g)
  if(pic_save == TRUE) {
    ggsave(paste("project/out/", "LT", horse_name, ".png", sep=""), dpi = 216)
  }
}

for(horse_name in c(unique(race_data$馬名))) {
  print(horse_name)
  # horse_name <- "イクイノックス"
  df_laptime = preprocessing(horse_name)
  figout(df_laptime, pic_save = TRUE)
}

z1 <- scale(race_data$last1F)  # 正規化
z1
mean(z1)
sd(z1)

# mutate() データセットに新たに変数を追加する関数
race_data %>% 
  as_tibble() %>%  # tibble型のデータフレームに変換（なくても集計可）
  rowwise() %>%
  c(last17F)
  # mutate(
  #   gender = paste("性別", "今回年齢", sep="")
  # )
race_data$gender <- paste(race_data$"性別", race_data$"今回年齢", sep="")
select(.data=race_data, c(columns))

head(race_data)
str(race_data)
race_data$"last17F"
race_data$last16F

seq(5, 1)
rep(1, 5)
rep(17:1, 3)

lap_time <- race_data[columns]
lap_time

nrow(race_data)
class(race_data)
sapply(race_data[columns], class)

h <- seq(18, 3)
lap <- race_data[1, h]
lap
y <- as.vector(lap[16:1])
y
halon <- 16:1
df <- data.frame(
  halon_name=halon,
  one_halon_time=t(y)
)
df
#g <- ggplot(data=lap, mapping=aes(x=halon, y=y))
g <- ggplot(data=df, mapping=aes(x=halon_name, y=one_halon_time))
#g <- g + geom_point()
g <- g + geom_line(size=1)
g <- g + scale_color_nejm()
plot(g)

class(y)
melt(y)

as.vector(lap[16:1])
halon


x <- data.frame(
  day  = 1:10,
  weight = c(120, 135, 145, 158, 173, 184, 198, 214, 209, 210)
)

g <- ggplot(x, aes(x = day, y = weight))
g <- g + geom_line()
plot(g)

a <- c(1, 2, 3)
a <- a + 1
a



x <- matrix(c(120, 118, 123, 120, 121, 119, 118, 121, 120, 120,
              121, 135, 145, 158, 173, 184, 198, 214, 209, 212,
              121, 130, 141, 148, 157, 168, 177, 189, 201, 210,
              119, 120, 120, 123, 125, 127, 141, 163, 180, 224,
              120, 128, 137, 144, 153, 163, 171, 179, 187, 199), ncol = 5)
x
rownames(x) <- 1:10
x
colnames(x) <- c("A", "B", "C", "D", "E")
x
y <- melt(x)
y
class(y)
colnames(y) <- c("day", "treat", "weight")
y

g <- ggplot(y, aes(x = day, y = weight, color = treat))
g <- g + geom_line()
g <- g + scale_color_nejm()
plot(g)

df_ <- data.frame(
  group    = c("A", "A", "A", "B", "B", "B"),
  subgroup = c("u", "v", "w", "u", "v", "w"),
  value    = c(1.1, 2.3, 2.1, 1.8, 2.2, 1.9)
)
df_
as.factor(df_$subgroup)

data <- data.frame(x1 = 1:5,    # Create example data
                  x2 = 6:10,
                  x3 = 11:15)
data
data1 <- data
for(i in 1:ncol(data1)) {
  # print(data[, i])
  data1[ , i] <- data1[ , i] + 10
}

data1
ncol(data1)

install.packages("palmerpenguins")
library('palmerpenguins')

ggplot(data=penguins) +
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species)) + 
  labs(title="Palmer Penguins: Body Mass vs. Flipper Length", subtitle="Sample of Three Penguin Species",
       caption="Data collected by Dr. Kristen Gorman") +
  annotate("text", x=220, y=3500, label="The Gentoos are the largest", color="purple", fontface="bold", size=4.5, angle=25)

p <- ggplot(data=penguins) +
  geom_point(mapping=aes(x=flipper_length_mm, y=body_mass_g, color=species)) + 
  labs(title="Palmer Penguins: Body Mass vs. Flipper Length", subtitle="Sample of Three Penguin Species",
       caption="Data collected by Dr. Kristen Gorman")
p +   annotate("text", x=205, y=3600, label="The Gentoos are the largest", color="purple", fontface="bold", size=4.5, angle=25)

ggplot(data = penguins) +
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color=species))
