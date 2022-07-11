# サルでもわかる待ち行列 よりRでコーディング
# http://objectclub.jp/technicaldoc/monkey/s_wait 
#
# ある医院では、患者が平均10分間隔でランダムに訪ねてくる。
# 医者は一人(M/M/1)であり、一人の患者の診断および処方の時間の平均は平均8分の指数分布であった。
# このとき、患者が診断を受け始めるまでの純粋待ち時間は何分か。
# 
# 待ち時間 = ρ / (1 - ρ) 人・分 
#
# プログラムはM/M/2のときなどの事も考えてコーディングした。
#

queueing = function(ta, ts, win=1) {
  λ = 1 / ta
  
  u = (1 / ts) * win
  
  ρ = λ / u   # 混雑度
  
  wait_man = ρ^win / (1 - ρ^win)  # 待ち人
  wait_time = wait_man * ts
  turn_around_time = wait_time + ts
  
  return(c(wait_man, wait_time, turn_around_time))
}

ta = 10     # 平均到着時間
ts = 8      # 平均サービス時間
win = 1     # M/M/*
kekka = queueing(ta, ts, win)
sprintf("M/M/%dモデル 待ち %.2f (人), %.3f (分), ターンアラウンドタイム %.1f (分)",
        win, kekka[1], kekka[2], kekka[3])

