race_data <- read.csv("~/project/trains_race.csv")
head(race_data, 3)

race_data[1:2,]
race_data[, 4]

handred <- subset(race_data, race_data['adjustment_time']>=100)
head(handred[, 0:10])

stem(race_data$fur_color)

st <- race_data[, 3:27]
#row.names(st) <- race_data[, 2]
nrow(race_data)
ncol(race_data)
race_data$adjustment_time
# 行名・列名
colnames(race_data)
rownames(race_data)[1:10]  # あまり使わないか
# 型の確認
class(race_data$adjustment_time)
# is.character() -> boolとかある
# 一度に確認
sapply(race_data, class)
# 型のキャスト
as.character(race_data$adjustment_time)
# 欠損値
notanum <-is.na(race_data$target_rank)
notanum

# unique()
unique(race_data$target_rank)
# value_counts()みたいな感じ
table(race_data$adjustment_time)
# dropna()
na.omit(race_data)

# Rのインデックス開始は 「1」スタート
race_data[0, ]  # カラム名が返ってきた
race_data[1,] # 1行目が返ってきた

Sys.Date()
Sys.time()

# d <- dist(st)

summary(race_data)
str(race_data)
mean(race_data$weight_ratio)

c(43, 56, 12 )
mdy(10212023)

z <- c(rep(10, 3))  # 10を3個並べる
z


# データの作成
set.seed(1)
c1 <- round(rnorm(6, 19.6, sd=sqrt(0.26)), 1)
var(c1)
mean(c1)
t.test(c1, mu=20, alternative = "two.sided", conf.level = 0.95)
