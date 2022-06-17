states <- read.csv("~/project/trains_race.csv")

st <- states[, 3:27]
row.names(st) <- states[, 2]

d <- dist(st)

summary(states)
str(states)
mean(states$weight_ratio)
