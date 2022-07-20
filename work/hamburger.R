# ハンバーガー統計学にようこそ！
# http://kogolab.chillout.jp/elearn/hamburger/

## 1. 平均と分散

# ランダムな数値を作る
# a =  round(rnorm(30, mean=170, sd=5), 1)

id  = c(1:49)
potate_len = c(3.5, 4.2, 4.9, 4.6, 2.8, 5.6, 4.2, 4.9, 4.4, 3.7,
              3.8, 4.0, 5.2, 3.9, 5.6, 5.3, 5.0, 4.7, 4.0, 3.1,
              5.8, 3.6, 6.0, 4.2, 5.7, 3.9, 4.7, 5.3, 5.5, 4.7,
              6.4, 3.8, 3.9, 4.2, 5.1, 5.1, 4.1, 3.6, 4.2, 5.0,
              4.2, 5.2, 5.3, 6.4, 4.4, 3.6, 3.7, 4.2, 4.8)
sum(potate_len)
mean(potate_len)
var(potate_len)
sd(potate_len)
median(potate_len)

### 度数分布を作る
table(potate_len)
h = hist(potate_len)
h$breaks  # 階級を区切る
h$counts  # 度数
n <- length(h$counts) # 階級の数
class_names <- NULL # 階級の名前格納用
for( i in 1:n ) {
  class_names[i] =paste(h$breaks[i], "～", h$breaks[i+1])
}
frequency_table = data.frame(class=class_names, frequency=h$counts)
frequency_table

#### 階級のとり方
h <- hist(potate_len, breaks=seq(2, 7, 0.5))
h

## 2. 信頼区間
# (ポテト編)

id = c(0:9)
samp = c(47,51,49,50,49,46,51,48,52,49)
u = mean(samp)
U2 = var(samp)  # Rの分散は「不変分散」。標本分散を求めたい時は手動で計算。
sd(samp)
# 不変分散 n-1で割った分散のあれ

n = 10 - 1  # 自由度

### 区間推定

length(samp)
### 信頼区間

# t値=2.262 (自由度9, 95%信頼区間の時)
t_val = 2.262
# 母平均 +- 2.262 * 標本標準誤差
upper_limit = u + t_val * sqrt(U2/length(samp))
bottom_limit = u - t_val * sqrt(U2/length(samp))
sprintf("%.02f 〜 %.02f", bottom_limit, upper_limit) # 95％の信頼区間

# (チキン編)

chicken = c(568, 530, 581, 554, 536, 518, 564, 552)
chicken_len = length(chicken)
id = c(1:chicken_len)

u = mean(chicken)
U2 = var(chicken)
n = chicken_len - 1
t_val95 = 2.365
t_val99 = 3.499
sqrt(U2/(n+1))

upper_limit = u + t_val95 * sqrt(U2/chicken_len)
bottom_limit = u - t_val95 * sqrt(U2/chicken_len)
sprintf("95％信頼区間：%.02f 〜 %.02f", bottom_limit, upper_limit) # 95％の信頼区間

upper_limit = u + t_val99 * sqrt(U2/chicken_len)
bottom_limit = u - t_val99 * sqrt(U2/chicken_len)
sprintf("99％信頼区間：%.02f 〜 %.02f", bottom_limit, upper_limit) # 95％の信頼区間

## 3. カイ２乗検定
# 帰無仮設：「ワクワクとモグモグに差はない」

#potate  = c(420, 280)
#chicken = c(180, 120)
#E_table <- data.frame(potate,chicken)
#rownames(E_table) = c('wakuwaku', 'mogumogu')
#E_table$wakuwaku
# 期待度数(理論度数)
temp_wakuwaku = c(420, 180)
temp_mogumogu = c(280, 120)
# 観測度数
wakuwaku = c(435, 165)
mogumogu = c(265, 135)
# カイ2乗値の算出
approval = (wakuwaku[1] - temp_wakuwaku[1])^2 / temp_wakuwaku[1] +
           (wakuwaku[2] - temp_wakuwaku[2])^2 / temp_wakuwaku[2] +
           (mogumogu[1] - temp_mogumogu[1])^2 / temp_mogumogu[1] +
           (mogumogu[2] - temp_mogumogu[2])^2 / temp_mogumogu[2]
approval  #-> 4.464286
# 自由度 (行-1) * (列-1) よって n = 1
# 95%信頼区間は3.84以下
# 帰無仮説は棄却される -> 「ワクワクとモグモグに差はある」

# ハンバーガーを追加する
# 条件: 有意水準1%
# 観測度数
wakuwaku = c(435, 165, 650)
mogumogu = c(265, 135, 350)
# 期待度数(理論度数)
total_sum = sum(wakuwaku) + sum(mogumogu)
material_sum = wakuwaku + mogumogu
temp_wakuwaku = c(material_sum[1]*sum(wakuwaku)/total_sum, material_sum[2]*sum(wakuwaku)/total_sum, material_sum[3]*sum(wakuwaku)/total_sum)
temp_mogumogu = c(material_sum[1]*sum(mogumogu)/total_sum, material_sum[2]*sum(mogumogu)/total_sum, material_sum[3]*sum(mogumogu)/total_sum)

approval = (wakuwaku[1] - temp_wakuwaku[1])^2 / temp_wakuwaku[1] +
           (wakuwaku[2] - temp_wakuwaku[2])^2 / temp_wakuwaku[2] +
           (wakuwaku[3] - temp_wakuwaku[3])^2 / temp_wakuwaku[3] +
           (mogumogu[1] - temp_mogumogu[1])^2 / temp_mogumogu[1] +
           (mogumogu[2] - temp_mogumogu[2])^2 / temp_mogumogu[2] +
           (mogumogu[3] - temp_mogumogu[3])^2 / temp_mogumogu[3]
approval  #-> 9.90
# 自由度
n = (3 - 1) * (3 - 1)
# 自由度4の有意水準1%での信頼区間は13.28以下
# よって帰無仮設は採択 -> 「ワクワクとモグモグに差は無い」

## 差の信頼区間
waku_jk = c(70, 75, 70, 85, 90, 70, 80, 75)
mogu_jk = c(85, 80, 95, 70, 80, 75, 80, 90)
mean_waku_jk = mean(waku_jk)
mean_mogu_jk = mean(mogu_jk)
ro2_waku_jk = sum((waku_jk - mean_waku_jk)^2)/length(waku_jk)  # 標本分散を求めたかった
ro2_mogu_jk = sum((mogu_jk - mean_mogu_jk)^2)/length(mogu_jk)  # 標本分散を求めたかった
#var_waku_jk = var(waku_jk)  # 不変分散
#var_mogu_jk = var(mogu_jk)

sum2_waku_jk = ro2_waku_jk * length(waku_jk)  # 平均からの偏差の平方和
sum2_mogu_jk = ro2_mogu_jk * length(mogu_jk)

temp_ro2_jk = (sum2_waku_jk + sum2_mogu_jk) / ((length(waku_jk)-1) + (length(mogu_jk)-1))  # 推定母分散
S2 = sqrt( temp_ro2_jk * (1/length(waku_jk) + 1/length(mogu_jk)) )
S2  #-> 3.88 差の標本標準誤差

# 差の信頼区間＝標本平均の差 ± t × 差の標本標準誤差
n = length(waku_jk) - 1 + length(mogu_jk) - 1  # 自由度
# 自由度14軒、t分布は両側検定を考えて、2.145
t = 2.145
upper_limit = (mean_waku_jk - mean_mogu_jk) + t * S2
bottom_limit = (mean_waku_jk - mean_mogu_jk) - t * S2
# ANS 95%信頼区間: -13.326 〜 3.326
#     マイナス寄りに大きく寄っているので、モグモグの評価が高いと分かる
#     しかし、0が範囲に含まれていることから、評価が同等となる可能性もあるため、帰無仮設は棄却できない

## t検定
