states <- read.csv("~/project/trains_race.csv")
head(states, 3)

states[0:2,]
states[, 4]

handred <- subset(states, states['adjustment_time']==100)
head(handred[, 0:10])

st <- states[, 3:27]
#row.names(st) <- states[, 2]
nrow(states)
ncol(states)
states$adjustment_time
# 行名・列名
colnames(states)
rownames(states)[1:10]  # あまり使わないか
# 型の確認
class(states$adjustment_time)
# is.character() -> boolとかある
# 一度に確認
sapply(states, class)
# 型のキャスト
as.character(states$adjustment_time)
# 欠損値
notanum <-is.na(states$target_rank)
notanum[notanum]
# unique()
unique(states$target_rank)
# value_counts()みたいな感じ
table(states$adjustment_time)
# dropna()
na.omit(states)

# Rのインデックス開始は 「1」スタート
states[0, ]  # カラム名が返ってきた
states[1,] # 1行目が返ってきた

Sys.Date()
Sys.time()

#d <- dist(st)

summary(states)
str(states)
mean(states$weight_ratio)

c(43, 56, 12 )
mdy(10211020)

z <- c(rep(10, 3))  # 10を3個並べる
z
