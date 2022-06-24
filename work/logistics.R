# ロジスティック回帰のお勉強
# [Rによる回帰分析の実施手順を紹介](https://www.gixo.jp/blog/3200/)

library(MASS)
head(birthwt)
View(birthwt)

# low    新生児の体重が2.5kg未満か否か(2.5kg未満が1)
# age    母親の年齢
# lwt    母親の体重(ポンド単位)
# smoke  母親の喫煙有無(喫煙有りが1)
# ptl    母親の早産経験の有無(経験有りが1)
# ht     母親の高血圧の有無(有りが1)
# ui    母親の子宮神経過敏の有(有りが1)

str(birthwt)  # データ構造の確認

sample_logi = birthwt[, c(1,2,3,5,6,7,8)]  # 使う特徴量だけ選択
str(sample_logi)
attach(sample_logi)

# 体重をポンド -> kgへ変換
lwt = lwt * 0.454

# ここからロジスティック回帰
output.glm = glm(low~., family=binomial, data=sample_logi)   # binomial => 二項分布
summary(output.glm)

# 影響のある特徴量を調べる
exp(output.glm$coefficients)
