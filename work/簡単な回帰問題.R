tall <- c(154, 160, 163, 169, 172, 180, 184)
comein <- c(22, 25, 30, 24, 24, 29, 35)

A = data.frame(tall, comein)
A

# 帰無仮設：傾き0となる
# 対立仮設：ならない

t.test(x=A$tall, y=A$comein, conf.level=0.95, var.equal=TRUE)

fit = lm(formula = A$comein ~ A$tall)
summary(fit)

plot(x=A$tall, y=A$comein, xlab = "身長", ylab = "収入", xlim = c(145, 200), ylim = c(10, 40))
abline(fit, col="#2792b3", lwd=2)

coefficients(fit) # 切片と傾き
