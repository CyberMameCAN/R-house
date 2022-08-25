# https://www.youtube.com/watch?v=U1OSnDw58Tc&list=PLrfN_7wvptdU6-tkZzykDu8cA_hVjabKp&index=6
# マジでゼロから始める統計生活！第四回『相関分析』※説明欄もみてね。
# https://www.youtube.com/watch?v=PnyUDr4ns3E&list=PLrfN_7wvptdU6-tkZzykDu8cA_hVjabKp&index=10
# マジでゼロから始める統計生活！第五回『標準化得点の作り方』※説明欄もみてね。
#
# 上記をハンズオン
#
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

# 相関係数の算出
cor(dat$項目1, dat$項目2)
# 検定を行う
cor.test(dat$項目1, dat$項目2) # method="pearson" ピアソンの積率相関係数の無相関検定を行う
cor.test(dat$項目1, dat$項目2, method="kendall") # ケンドールの順位相関係数の無相関検定を行う
cor.test(dat$項目1, dat$項目2, method="spearman") # スピアマンの順位相関係数の無相関検定を行う
# 相関行列を算出する
colnames(dat)

cor(dat[, 2:4])
#install.packages("psych")
library(psych)
corr.test(dat[, 2:4])


# 2変数の関連
cor(dat$項目1, dat$項目2)
# 範囲指定
cor(dat[, 2:4])

# 無相関係数
# 2変数
cor.test(dat$項目1, dat$項目2)

corr.test(x=dat[, 2:4])

# 標準化得点の作り方
# 1つの変数編
項目1z <- scale(dat$項目1)
mean(項目1z)
sd(項目1z)

# 複数の変数編
項目z <- apply(dat[, 2:4], 2, scale)  # 2:列ごとを指定 scale: 標準化したい関数
apply(項目z, 2, mean)
apply(項目z, 2, sd)  # 不偏標準偏差

dat$項目1z <- NA # 空データの項目を作成する
dat$項目2z <- NA
dat$項目3z <- NA

head(dat)

colnames(dat)
dat[, 6:8] <- apply(dat[, 2:4], 2, scale) # applyで一括処理
head(dat)

round(apply(dat[, c(2:4, 6:8)], 2, mean), 2)
round(apply(dat[, c(2:4, 6:8)], 2, sd), 2)
