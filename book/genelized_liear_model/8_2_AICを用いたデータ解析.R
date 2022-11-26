# ***********************************************************************************************
# Title   : 平均・分散から始める一般化線形モデル入門
# Chapter : 8 情報理論と統計学
# Theme   : 2 AICを用いたデータ分析
# Date    : 2022/11/26
# Page    : P310 - P319
# URL     : https://logics-of-blue.com/
# ***********************************************************************************************


# ＜概要＞
# - AICとは｢モデルの悪さ｣を示す指標
#   --- 予測された確率分布とのズレを用いている
#   --- 統計的検定が必要ないくらいまで使い勝手の良い指標


# ＜目次＞
# 0 準備
# 1 ベースモデルの構築
# 2 AICに指数減少法のモデル選択
# 3 AICによるステップワイズ法のモデル選択
# 4 総当たり法によるステップワイズ法のモデル選択


# 0 準備 -------------------------------------------------------------------------------

# ＜ポイント＞
# - データ内容は以下のとおり
#   --- sell       : 売上高
#   --- experience : チラシを配る店員さんの経験年数（単位：年）
#   --- n.sheets   : 配ったチラシの枚数（単位：枚）
#   --- time       : チラシを配った時間帯（昼/夜）
#   --- sex        : チラシを配った店員の性別（男/女）


# ライブラリ
library(tidyverse)
library(MuMIn)
library(broom)
library(modelsummary)


# データロード
d5 <- read_csv("csv/data1_typeII_anova.csv")

# データ確認
d5 %>% head()
d5 %>% glimpse()


# 1 ベースモデルの構築 -------------------------------------------------------------

# フルモデル
# --- 4つの変数を一つにまとめて統計モデリングする
sellModelFull <- lm(sell ~., data = d5)

# NULLモデル
# --- 説明変数が何もないモデル
sellModelNull <- lm(sell ~ 1, data=d5)

sellModelFull %>% tidy()

# モデル比較
modelsummary(list(Full_Model = sellModelFull,
                  Null_Model = sellModelNull), type = "text")

# AICの計算
AIC(sellModelFull, sellModelNull)


# 2 AICに指数減少法のモデル選択 -----------------------------------------------------


# モデル変数選択
# --- 変数減少法
modelStep <- sellModelFull %>% step(direction = "backward")

# 変数選択の結果
# --- 最良モデルのlmオブジェクトが出力される
modelStep %>% print()
modelStep %>% class()

# フルモデルを更新
# --- experienceを除外
# --- ベンチマークモデル
sellModel2 <- sellModelFull %>% update(~.-experience)

# モデル比較
modelsummary(list(Full_Model  = sellModelFull,
                  Step_Model  = modelStep,
                  Sell_Model2 = sellModel2), type = "text")


# AICの差分の確認
AIC(sellModelFull) - AIC(sellModel2)


# 3 AICによるステップワイズ法のモデル選択 ---------------------------------------------

# ＜ポイント＞
# - ステップワイズ法は指数増減法とも呼ばれる
#   --- 指数減少法よりも探索範囲が広い（データ量が増えると時間がかかる）


# モデル変数選択
# --- ステップワイズ法
sellModelStepWise <- sellModelFull %>% step(direction = "both")

# モデル比較
modelsummary(list(Full_Model  = sellModelFull,
                  Step_Model  = modelStep,
                  Setp_Model2 = sellModelStepWise,
                  Sell_Model2 = sellModel2), type = "text")


# 4 総当たり法によるステップワイズ法のモデル選択 -----------------------------------------

# 総当たり法
# --- オプション変更しないとdredge()が動かない
options(na.action = "na.fail")
resultDredge <- sellModelFull %>% dredge(rank = "AIC")

# 結果確認
resultDredge %>% print()
resultDredge %>% glimpse()

# deltaによる絞り込み
# --- AIC最小からの差が2未満のAICを持つものだけを取得する
resultDredge %>% subset(delta < 2)


# 5 変数選択のカスタマイズ -------------------------------------------------------------

# step関数
# --- 「どうしても入れたい変数」を指定する
stepExperience <- sellModelFull %>% step(scope = list(lower = ~ 1 + experience))

# dredge関数
# --- 「どうしても入れたい変数」を指定する
options(na.action = "na.fail")
resultDredgeExperience <- sellModelFull %>% dredge(rank="AIC", subset = ~ experience)
stepBestModel <- get.models(resultDredgeExperience, 1)[[1]]

# モデル比較
modelsummary(list(Full_Model  = sellModelFull,
                  Step_Model1 = stepExperience,
                  Setp_Model2 = stepBestModel), type = "text")
