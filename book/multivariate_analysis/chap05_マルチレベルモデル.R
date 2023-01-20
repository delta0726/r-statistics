# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 5 マルチレベルモデル
# Theme   : さまざまな集団から得られたデータを分析したい
# Date    : 2022/12/18
# Page    : P115 - P137
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜概要＞
# - マルチレベルモデルとは、階層データに対して回帰モデルを適用する手法
#   --- グループごとに傾きや切片を定義することにより、グループ情報に基づく細やかな情報を入手することができる


# ＜目次＞
# 0 準備
# 1 データ概要とデータ構造
# 2 データの階層構造の確認
# 3 級内相関係数の確認
# 4 階層ごとの中心化
# 5 ランダム切片モデル
# 6 ランダム傾きモデル
# 7 集団レベルの変数を含むマルチレベルモデル
# 8 クロスレベルの交互作用項を含むマルチレベルモデル


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ICC)
library(lmerTest)
library(psych)
library(broom)


# データロード
df <- read_csv("csv/chap05/frog.csv")


# 1 データ概要とデータ構造 ------------------------------------------------------------

# ＜目的＞
# - ｢井の中の蛙｣で自己肯定感を高めた方が、学業的自己概念を維持しやすいかを確認する
#   --- 学業的自己概念が高いほど、テストの点数も良くなると考えられる

# ＜データ項目＞
# - 生徒： Student
# - クラス： Class
# - 学業的自己概念： Self_Concept
# - テスト得点： Test_score


# データ確認
df %>% print()
df %>% glimpse()

# 相関確認
# --- 全体的には学業的自己概念が高いほどテスト得点が高い傾向がありそう
df %>%
  select(Self_Concept, Test_score) %>%
  pairs.panels()


# 2 データの階層構造の確認 -----------------------------------------------------------

# ＜分析目的＞
# - 学業的自己概念に対する3つの効果について検討する
#   --- 1. 生徒個人の学業水準の効果
#   --- 2. ｢生徒個人の学業水準｣を一定にしたときの、｢クラスごとの学業水準｣の効果
#   --- 3. ｢生徒個人の学業水準｣と｢学級の学業水準｣のクロスレベルの交互作用効果


# ＜データ構造＞
# - レベル1：抽出単位は個人(生徒：Student)
# - レベル2：抽出単位は集団(Class：Class)


# カテゴリカル変数の確認
# --- 生徒IDは個人ごとに異なる
# --- クラスは40人ごとに30クラス（1200人）
df$Student %>% table()
df$Class %>% table()


# 3 級内相関係数の確認 ------------------------------------------------------------

# ＜ポイント＞
# - 階層性を持つデータでは観測値の独立性が満たされないことを想定（通常の回帰分析では観測値の独立性を前提とする）
# - グループ内でのデータの類似性を観測する指標としてICCがある（級内相関係数）
#   --- ICCが大きい程、マルチレベル分析を利用する必要性が高い
#   --- ICC = (集団の分散) / (集団の分散 + 個人の分散)


# 級内相関係数の算出
ICCest(as.factor(Class), Self_Concept, data = df, alpha = 0.05, CI.type = "Smith")


# 4 階層ごとの中心化 --------------------------------------------------------------

# データ加工
# --- Test_score_m  ： Test_scoreのClass平均値
# --- Test_score_cwc： 集団平均中心化
# --- Test_score_cgm： 全体平均中心化
df_center <-
  df %>%
    group_by(Class) %>%
    mutate(Test_score_m = mean(Test_score)) %>%
    ungroup() %>%
    mutate(Test_score_cwc = Test_score - Test_score_m,
           Test_score_cgm = Test_score - mean(Test_score),
           Test_score_cm = Test_score_m - mean(Test_score))

# 相関係数の算出
df_center %>%
  select(Test_score_m, Test_score_cwc, Test_score_cgm) %>%
  cor() %>%
  round(3)

# プロット確認
# --- 散布図行列
df_center %>%
  select(Test_score_m, Test_score_cwc, Test_score_cgm) %>%
  pairs.panels()


# 5 ランダム切片モデル ----------------------------------------------------------

# ＜ポイント＞
# - 切片がグループごとによって異なり、傾きはグループ間で等しいことを仮定したモデル
#   --- ICCが大きい程、切片のばらつきが大きくなる


# データ確認
df_center %>% print()

#ランダム切片モデル
model1.cwc <- lmer(Self_Concept ~ Test_score_cwc + (1 | Class), data = df_center, REML = FALSE)

# サマリー
model1.cwc %>% summary()

# 回帰係数
model1.cwc %>% coef()

# 効果
# --- 固定効果
# --- ランダム効果
model1.cwc %>% fixef()
model1.cwc %>% ranef()

# ランダム切片
model1.cwc %>% coef() %>% use_series(Class) %>% use_series(`(Intercept)`)

# 手動計算（ランダム切片）
fixed_effect <- model1.cwc %>% fixef() %>% .[1]
random_effect <- model1.cwc %>% ranef() %>% use_series(Class) %>% .[,1]
fixed_effect + random_effect


# 6 ランダム傾きモデル -----------------------------------------------------------

# ＜ポイント＞
# - ランダム傾きモデルは、グループごとに切片に加えて傾きも変化するモデル（切片固定ではない点に注意）


# データ確認
df_center %>% print()

#ランダム傾きモデル
model2.cwc <- lmer(Self_Concept ~ Test_score_cwc + (1 + Test_score_cwc | Class),
                   data = df_center, REML = FALSE)

# サマリー
model2.cwc %>% summary()

# 回帰係数
model2.cwc %>% coef()

# 効果
# --- 固定効果
# --- ランダム効果
model2.cwc %>% fixef()
model2.cwc %>% ranef()

# ランダム切片/ランダム傾き
total_effect <- model2.cwc %>% coef() %>% use_series(Class)

# 手動計算（ランダム切片）
fixed_effect <- model2.cwc %>% fixef()
random_effect <- model2.cwc %>% ranef() %>% use_series(Class)
total_effect_calc <- t(fixed_effect + t(random_effect))

# 検証
total_effect - total_effect_calc


# 7 集団レベルの変数を含むマルチレベルモデル ------------------------------------------

# 集団レベルの変数を含むマルチレベルモデル
model3.cgm <- lmer(Self_Concept ~ Test_score_cgm + Test_score_cm + (1 + Test_score_cgm | Class),
                   data = df_center,
                   REML = FALSE)

# サマリー
model3.cgm %>% summary()


# 8 クロスレベルの交互作用項を含むマルチレベルモデル -------------------------------------

# クロスレベルの交互作用項を含むマルチレベルモデル
model4.cgm <- lmer(Self_Concept ~ Test_score_cgm +
  Test_score_cm +
  Test_score_cgm * Test_score_cm +
  (1 + Test_score_cgm | Class), data = df_center, REML = FALSE)

# サマリー
model4.cgm %>% summary()

# モデル比較
anova(model3.cgm, model4.cgm)

# 信頼区間の算出
confint(model4.cgm, method = "Wald")
