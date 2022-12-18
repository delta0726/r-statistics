# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 3 重回帰分析
# Theme   : 現象を説明・予測する統計モデルを作る
# Date    : 2022/12/17
# Page    : P68 - P91
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ概要と分析目的の確認
# 2 重回帰モデルの構築
# 3 多重共線性のチェック
# 4 偏回帰係数の信頼区間
# 5 標準偏回帰係数の算出
# 6 質的変数を含む重回帰分析
# 7 AICとBICによるモデル評価
# 8 切片と編回帰係数の検定


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(modelsummary)
library(car)


# データロード
csdat <- read_csv("csv/chap03/customer_satisfaction.csv")

# データ確認
csdat %>% print()
csdat %>% glimpse()


# 1 データ概要と分析目的の確認 ----------------------------------------------------

# ＜データ概要＞
# - フィットネスジムのアンケート調査

# ＜データ項目＞
# - 店舗番号： no
# - 顧客数： client_n
# - 立地満足度： location
# - 設備満足度： facility
# - 店舗面積満足度： area
# - トレーナー満足度： trainer
# - トレーナー数： trainer_n
# - 接客研修： training_flag
# - 入会特典： privilege_flag

# ＜分析目的＞


# 2 重回帰モデルの構築 ----------------------------------------------------------

# モデル構築
res1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)

# モデルサマリー
res1 %>% summary()

# モデル比較
list(res1 = res1) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 3 多重共線性のチェック ----------------------------------------------------------

# 説明変数間の相関係数
csdat %$% cor(trainer, trainer_n)

# モデル構築
# --- 相関の高い変数を含む
resm1 <- lm(client_n ~ trainer, data = csdat)
resm2 <- lm(client_n ~ trainer + trainer_n, data = csdat)

# モデルサマリー
# --- モデル2は標準誤差がとても大きくなっている
list(model1 = resm1, model2 = resm2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")

# VIFの算出
# --- 10を超えると多重共線性が疑われる
resm2 %>% vif()

# 2で作成したモデルのVIF
# --- 問題なし
res1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)
res1 %>% vif()


# 4 偏回帰係数の信頼区間 --------------------------------------------------------

# モデル構築(再掲)
res1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)

# 信頼区間の算出
# --- 切片と偏回帰係数
res1 %>% confint(level = 0.95)


# 5 標準偏回帰係数の算出 --------------------------------------------------------


# データの標準化とデータフレーム化
scsdat <- csdat %>% scale() %>% as.data.frame()

# モデル構築
# --- モデル2： 標準偏回帰係数の算出
# --- モデル3： 質的変数も含めた重回帰分析
res2 <- lm(client_n ~ location + trainer_n, data = scsdat)

# 結果確認
res2 %>% summary()

# モデルサマリー
list(res2 = res2) %>% modelsummary(statistic = "{std.error}({statistic}){stars}")


# 6 質的変数を含む重回帰分析 ---------------------------------------------------


# データ確認
csdat %>%
  select(client_n, location, facility, area, trainer, training_flag, privilege_flag)

# モデル構築
res3 <- lm(client_n ~ location + facility + area + trainer +
           training_flag + privilege_flag, data = csdat)

# 結果確認
res3 %>% summary()

# モデルサマリー
list(res3 = res3) %>% modelsummary(statistic = "{std.error}({statistic}){stars}")


# 7 AICとBICによるモデル評価 ---------------------------------------------------

# モデル構築
# --- モデル1： 説明変数が量的変数のみの回帰モデル
# --- モデル3： 説明変数に質的変数も含めた回帰モデル
res1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)
res3 <- lm(client_n ~ location + facility + area + trainer + training_flag + privilege_flag, data = csdat)

# AICの算出
res1 %>% extractAIC()
res3 %>% extractAIC()

# BICの算出
res1 %>% extractAIC(k = log(30))
res3 %>% extractAIC(k = log(30))

# glance
res1 %>% glance()
res3 %>% glance()

# モデルサマリー
list(res1 = res1, res3 = res3) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 8 切片と編回帰係数の検定 ------------------------------------------------------

# t分布
# --- 切片のp値
pt(-0.6949209, 25) * 2

# 設備満足度の偏回帰係数のp値
(1 - pt(3.075611, 25)) * 2

# qtの算出
# --- 下側確率0.025を与えるqtの算出
# --- 上側確率0.025を与えるqtの算出
qt(0.025, 25)
qt(0.975, 25)


# 信頼区間の算出
# --- 信頼区間の下限
# --- 信頼区間の上限
21.640 - 2.059539 * 7.036
21.640 + 2.059539 * 7.036
