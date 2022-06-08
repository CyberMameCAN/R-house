df <- airquality
View(df)

plot(df)
#attach(df)  # いちいちdfと書かなくて済む
a <- lm(Ozone ~ Solar.R+Wind+Temp)
summary(a)

# Ozon = 0.06 * Solar.R - 3.33 * Wind + 1.65 * Temp - 64.34
0.06 * 299 - 3.33 * 8.6 + 1.65 * 65 - 64.34  # -> 32.212

names(df)
df$Ozone
mean(df$Ozone, na.rm = TRUE)
v <- var(df$Ozone, na.rm = TRUE)
sqrt(v)

df_rmna <- df %>% 
  drop_na(Ozone)
View(df_rmna)

oz <- df_rmna$Ozone
te <- df_rmna$Temp
f <- lm(oz ~ te)
print(f)
plot(oz, te)
abline(f, col="blue")
