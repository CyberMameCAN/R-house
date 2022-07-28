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
U2 = var(samp)  # Rの分散は「不偏分散」。標本分散を求めたい時は手動で計算。
sd(samp)
# 不偏分散 n-1で割った分散のあれ

n = 10 - 1  # 自由度

### 区間推定

length(samp)
### 信頼区間

# t値=2.262 (自由度9, 95%信頼区間の時)
#t_val = 2.262
t_val = abs(qt(0.05/2, n))
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
#t_val95 = 2.365
t_val95 = abs(qt(0.05/2, n))
#t_val99 = 3.499
t_val99 = abs(qt(0.01/2, n))
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
#var_waku_jk = var(waku_jk)  # 不偏分散
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
waku_jk
mean_waku_jk
ro2_waku_jk
temp_ro2_jk
S2

# 計算結果より
t_val = (mean_waku_jk - mean_mogu_jk) / S2
t_val  #-> -1.288
# 自由度14、信頼区間95%の範囲は -2.145 〜 2.145
# よって帰無仮設は棄却できない -> 「優位な差はない」を否定できない

# (ポテト編)
waku_jk = c(80, 75, 80, 95, 90, 80, 85, 90)
mogu_jk = c(75, 65, 80, 85, 75, 80, 80, 70)

mean_waku_jk = mean(waku_jk)
mean_mogu_jk = mean(mogu_jk)

ro2_waku_jk = sum((waku_jk - mean_waku_jk)^2)/length(waku_jk)  # 標本分散を求めたかった
ro2_mogu_jk = sum((mogu_jk - mean_mogu_jk)^2)/length(mogu_jk)  # 標本分散を求めたかった

sum2_waku_jk = ro2_waku_jk * length(waku_jk)  # 平均からの偏差の平方和
sum2_mogu_jk = ro2_mogu_jk * length(mogu_jk)

temp_ro2_jk = (sum2_waku_jk + sum2_mogu_jk) / ((length(waku_jk)-1) + (length(mogu_jk)-1))  # 推定母分散
S2 = sqrt( temp_ro2_jk * (1/length(waku_jk) + 1/length(mogu_jk)) )  # 差の標本標準誤差

# t値
t_val = (mean_waku_jk - mean_mogu_jk) / S2
t_val  #->2.463
# 自由度14、信頼区間95%の範囲は -2.145 〜 2.145
# よって、帰無仮設は棄却 -> 「優位な差はない」を否定できる

## t検定
# (同時に2つを評価)
waku_jk = c(90, 75, 75, 75, 80, 65, 75, 80)
mogu_jk = c(95, 80, 80, 80, 75, 75, 80, 85)
	
mean_waku_jk = mean(waku_jk)
mean_mogu_jk = mean(mogu_jk)

ro2_waku_jk = sum((waku_jk - mean_waku_jk)^2) / length(waku_jk)  # 標本分散
ro2_mogu_jk = sum((mogu_jk - mean_mogu_jk)^2) / lenght(mogu_jk)  # 標本分散

diff_jk = waku_jk - mogu_jk
mean_diff_jk = mean(diff_jk)
ro2_diff_jk = sum( (diff_jk - mean_diff_jk)^2 ) / length(diff_jk)

t_val = mean_diff_jk / sqrt(ro2_diff_jk/(length(diff_jk) - 1))
t_val  # -2.966
# 自由度7、信頼区間95%の範囲は、-2.365 〜 2.365
# よって、帰無仮設は棄却 -> 「優位な差がない」を否定できる

# 5.4　実践編：新作バーガー！
before = c(65, 70, 85, 70, 80, 80, 80, 70, 80, 70)
after  = c(80, 90, 80, 90, 90, 85, 80, 90, 85, 80)

mean_before = mean(before)
mean_after  = mean(after)

ro2_before = sum((before - mean_before)^2) / length(before)
ro2_after  = sum((after  - mean_after )^2) / length(after)

diff = before - after
diff
mean_diff = mean(diff)
ro2_diff = sum((diff - mean_diff)^2) / length(diff)

t_val = mean_diff / sqrt(ro2_diff/(length(diff) - 1))
t_val  #-> 3.586
# 自由度7、信頼区間95%の範囲は、-2.365 〜 2.365
# よって帰無仮設は棄却 -> 「優位な差がない」を否定できる

## 6. 分散分析（１要因）
# グループ数が３つ以上になったバイの分析手法

wakuwaku = c(80, 75, 80, 90, 95, 80, 80, 85, 85, 80, 90, 80, 75, 90, 85, 85, 90, 90, 85, 80)
mogumogu = c(75, 70, 80, 85, 90, 75, 85, 80, 80, 75, 80, 75, 70, 85, 80, 75, 80, 80, 90, 80)
pakupaku = c(80, 80, 80, 90, 95, 85, 95, 90, 85, 90, 95, 85, 98, 95, 85, 85, 90, 90, 85, 85)

mean_waku = mean(wakuwaku)
mean_mogu = mean(mogumogu)
mean_paku = mean(pakupaku)
# 平方和(群内)
se_waku = sum((wakuwaku - mean_waku)^2)
se_mogu = sum((mogumogu - mean_mogu)^2)
se_paku = sum((pakupaku - mean_paku)^2)
# 分散(群内)
ro2_waku = se_waku / length(wakuwaku)
ro2_mogu = se_mogu / length(mogumogu)
ro2_paku = se_paku / length(pakupaku)

ro_waku = sqrt(ro2_waku)
ro_mogu = sqrt(ro2_mogu)
ro_paku = sqrt(ro2_paku)

# 帰無仮説は「3つのお店のポテトの評価（母集団）の平均に差はない」

wa_mo_pa = c(wakuwaku, mogumogu, pakupaku)
mean_wa_mo_pa = mean(wa_mo_pa)
# 平方和(群間、水準間)
se_wa_mo_gu = sum((wa_mo_pa - mean_wa_mo_pa)^2)
# 分散(群間、水準間)
ro2_wa_mo_pa = se_wa_mo_gu / length(wa_mo_pa)

ro_wa_mo_pa = sqrt(ro2_wa_mo_pa)

# 全体(残差)の平方和
ro2_all = ro2_wa_mo_pa * length(wa_mo_pa)

wa = sum(mean_waku - mean_wa_mo_pa)^2 * length(wakuwaku)
mo = sum(mean_mogu - mean_wa_mo_pa)^2 * length(mogumogu)
pa = sum(mean_paku - mean_wa_mo_pa)^2 * length(pakupaku)

se_waku + se_mogu + se_paku + wa + mo + pa #-> 2494.183  分散(群間)に等しい

# 分散分析表を作る
# 群間(水準間)
n_e = 3  # ワクワク・モグモグ・パクパク
U2_e = (wa + mo + pa) / (n_e - 1)
# 群内(残差)
n_g = (length(wakuwaku) - 1) + (length(mogumogu) - 1) + (length(pakupaku) - 1)
U2_g = (se_waku + se_mogu + se_paku) / n_g

f_value = U2_e / U2_g  #-> 12.22 
# F分布表より自由度57, 自由度2はダイタイ3.150
# 12.22とを比べると、棄却域に入っている -> 少なくとも１つの組み合わせ間に差がある

# aovで分散分析表
potate_data = data.frame(A=factor(c(rep("wakuwaku", 20),rep("mogumogu", 20), rep("pakupaku", 20))), X=c(wakuwaku, mogumogu, pakupaku))
boxplot(X~A,data=potate_data, col="lightblue")

# 分散分析表が出力される
summary(aov(X~A, data=potate_data))

## 7. 分散分析（２要因）

crispy_spycy    = c(65, 85, 75, 85, 75, 80, 90, 75, 85, 65, 75, 85, 80, 85, 90)
crispy_regular  = c(65, 70, 80, 75, 70, 60, 65, 70, 85, 60, 65, 75, 70, 80, 75)
regular_spycy   = c(70, 65, 85, 80, 75, 65, 75, 60, 85, 65, 75, 70, 65, 80, 75)
regular_regular = c(70, 70, 85, 80, 65, 75, 65, 85, 80, 60, 70, 75, 70, 80, 85)

# 平均
mean_crispy_spycy    = mean(crispy_spycy)
mean_crispy_regular  = mean(crispy_regular)
mean_regular_spycy   = mean(regular_spycy)
mean_regular_regular = mean(regular_regular)
# 平方和(群内)
se_crispy_spycy    = sum((crispy_spycy - mean_crispy_spycy)^2)
se_crispy_regular  = sum((crispy_regular - mean_crispy_regular)^2)
se_regular_spycy   = sum((regular_spycy - mean_regular_spycy)^2)
se_regular_regular = sum((regular_regular - mean_regular_regular)^2)
# 分散
ro2_crispy_spycy    = se_crispy_spycy / length(crispy_spycy)
ro2_crispy_regular  = se_crispy_regular / length(crispy_regular)
ro2_regular_spycy   = se_regular_spycy / length(regular_spycy)
ro2_regular_regular = se_regular_regular / length(regular_regular)
# 標準偏差
ro_crispy_spycy    = sqrt(ro2_crispy_spycy)
ro_crispy_regular  = sqrt(ro2_crispy_regular)
ro_regular_spycy   = sqrt(ro2_regular_spycy)
ro_regular_regular = sqrt(ro2_regular_regular)

# クリスピーでの集計
crispy = c(crispy_spycy, crispy_regular) 
mean_crispy = mean(crispy)
se_crispy   = sum((crispy - mean_crispy)^2)
ro2_crispy  = se_crispy / length(crispy) 
ro_crispy   = sqrt(ro2_crispy)
# 普通味の衣
regular      = c(regular_spycy, regular_regular)
mean_regular = mean(regular)
se_regular   = sum((regular - mean_regular)^2)
ro2_regular  = se_regular / length(regular) 
ro_regular   = sqrt(ro2_regular)

# 辛口味での集計
spycy = c(crispy_spycy, regular_spycy)
mean_spycy = mean(spycy)
se_spycy   = sum((spycy - mean_spycy)^2)
ro2_spycy  = se_spycy / length(spycy)
ro_spycy   = sqrt(ro2_spycy)
# 普通味での集計
nonspycy      = c(crispy_regular, regular_regular)
mean_nonspycy = mean(nonspycy)
se_nonspycy   = sum((nonspycy - mean_nonspycy)^2)
ro2_nonspycy  = se_nonspycy / length(nonspycy)
ro_nonspycy   = sqrt(ro2_nonspycy)

# 味勝負　全体での計算
mean_taste_total = (mean_crispy + mean_regular + mean_spycy + mean_nonspycy) / 4
se_taste_total   = (se_crispy + se_regular + se_spycy + se_nonspycy) / 4
ro2_taste_total  = (ro2_crispy + ro2_regular + ro2_spycy + ro2_nonspycy) / 4
ro_taste_total   = (ro_crispy + ro_regular + ro_spycy + ro_nonspycy) / 4

# 要因1によるズレ
factor1 = (mean_crispy - mean_taste_total)^2 * length(crispy) + 
  (mean_regular - mean_taste_total)^2 * length(crispy)
# 要因2によるズレ
factor2 = (mean_spycy - mean_taste_total)^2 * length(spycy) + 
  (mean_nonspycy - mean_taste_total)^2 * length(spycy)
# 交互作用によるズレ

# (未完//)

# aovで分散分析表
bunsan2 = data.frame(A=factor(c(rep("wakuwaku",20), rep("mogumogu",20), rep("pakupaku",20))), No=factor(rep(1:20, 3)),y=c(wakuwaku, mogumogu, pakupaku))
bunsan2

summary(aov(y~ A+No, bunsan2))
# 「A」の行が水準・群に関する分散分析の統計検定量
# Pr(.F)の値がは順分小さい -> よって因子Aの水準・群の平均値は有意水準0.05で差があると言える
# 「No」の行が各お店間に関する分散分析の統計検定量
# Aほどではないが0.0031となり、有意水準0.05で差があると言える
