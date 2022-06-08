# 分析していく
file = "http://htsuda.net/stats/dataset/overclaiming.csv"
dat = read.table(file=file, header = T, sep = ",", stringsAsFactors = F)
head(dat)                 

# グラフ化
plot(x=dat$confidence, dat$overclaiming)

install.packages("cat")
library(cat)
scatterplotMatrix(subset(dat, select = -id), regLine = list(col="#2792b3"), col = "black", cex = .5)

fit = lm(overclaiming ~ confidence + accuracy, data = dat)
summary(fit)

order1 = subset(dat, order == 1)
order2 = subset(dat, order == 2)
t.test(x = order1$overclaiming, y = order2$overclaiming, paired = F)

fit1 = lm(overclaiming ~ confidence + accuracy, data = order1)
summary(fit1)$coefficients

fit2 = lm(overclaiming ~ confidence + accuracy, data = order2)
summary(fit2)$coefficients

fit = lm(overclaiming ~ accuracy + FINRA, data = dat)
summary(fit)$coefficients
cor.test(dat$confidence, dat$FINRA) # 後半の相関に関しては同じ結果を確認

fit = lm(overclaiming ~ confidence + accuracy + FINRA, data = dat)
summary(fit)$coefficients

library(car)

fit = lm(overclaiming ~ confidence + accuracy + FINRA, data = dat)
qqPlot(fit, labels = row.names(dat), id.method = "identity", simulate = T)

# 線形性の確認
car::crPlots(fit)

# 等分散性を確認
car::ncvTest(fit)
spreadLevelPlot(fit)

# 多重共線性を確認
car::vif(fit)  # その説明変数についても値は2未満であるので、多重共線性問題はない事が確認できる
