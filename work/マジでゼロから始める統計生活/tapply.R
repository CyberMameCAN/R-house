# https://www.youtube.com/watch?v=17L2k7xQOD0&list=PLrfN_7wvptdU6-tkZzykDu8cA_hVjabKp&index=7
# マジでゼロから始める統計生活！番外編『これができれば行列計算が楽になる！Rをもっと使いこなそう！！』
#
# 要約：apply()で合成変数を作り、tapply()でその記述統計を行う
set.seed(111)
ID <- 1:100
項目1 <- round(rnorm(100, 30, 5), 0)
項目2 <- round(rnorm(100, 60, 10), 0)
項目3 <- round(rnorm(100, 50, 20), 0)

dat <- data.frame(ID=ID, 項目1=項目1, 項目2=項目2, 項目3=項目3, 性別=NA)
dat[1:50, 5] <- 1
dat[51:100, 5] <- 2

# データの確認
head(dat)
colnames(dat)
dim(dat)
str(dat)

dat$合計得点 <- NA
dat$合計得点 <- apply(dat[, 2:4], 1, sum)

# tapply()の使い方
# tapply(データ型, カテゴリ変数名, 関数)
tapply(dat$項目1, dat$性別, mean)

tapply(dat$合計得点, dat$性別, mean)
