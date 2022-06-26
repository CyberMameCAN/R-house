# - [【Rでベイズ統計モデリング#1】Stanのインストールと基礎](https://www.youtube.com/watch?v=Q7404MnA9Os&t=0s)

#install.packages("rstan", repos="https://cloud.r-project.org", dependencies=TRUE)
library(rstan)

# C++の確認
pkgbuild::has_build_tools(debug=TRUE)

# Stanの効率化
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

# sampleデータ
set.seed(1)
data = rnorm(100, 50, 10)  # 100個のデータ、50平均、10標準偏差
dlist = list(N=100, y=data)

# Stanの利用
mcmc_result = stan(file="1.sample.stan", data=dlist, seed=1)
summary(mcmc_result)
