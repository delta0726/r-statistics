# ***********************************************************************************************
# Title   : Rによる心理学研究法入門
# Chapter : 7 心理学における調査研究（1）
# Date    : 2023/1/16
# Page    : P147 - P171
# URL     : https://www.kitaohji.com/news/n41183.html
# ***********************************************************************************************


# ＜概要＞
# - 階層性のあるデータは｢観測データの独立性｣の仮定が満たされず｢集団特異性｣を持つと考えられる
#   --- 階層性のあるデータではマルチレベル分析が必要となる


# ＜統計的手法＞
# - マルチレベル分析
# - 尤度比検定


# ＜目次＞
# 0 準備
# 1 データ概要
# 2 スコア集計とクラスごとのプロット
# 3 級内相関係数の算出
# 4 集団平均センタリング
# 5 ランダム切片モデル
# 6 ランダム傾きモデル
# 7 尤度比検定によるモデル比較
# 8 説明変数が2つ以上のモデル


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(lme4)
library(broom)
library(ICC)


# データロード
df <- read_csv("csv/competition.csv")


# 1 データ概要 -------------------------------------------------------------------

# ＜データ内容＞
# - 大学受験の競争環境は人の動機付けやパフォーマンスにどのような影響を与えるのかを調査
# - 4つの学校でクラス別/男女別にアンケートを実施（階層性を持つデータ）

# ＜データ項目＞
# - school  ：学校
# - class   ：クラス
# - sex     ：性別
# - c1-c9   ：消耗型競争観
# - c10-c21 ：成長型競争観
# - m1-m5   ：内的調整
# - m6-m10  ：同一化的調整
# - m11-m15 ：取り入れ的調整
# - m16-m20 ：外的調整


# データ確認
df %>% print()
df %>% glimpse()

# カテゴリデータの確認
df %>% select(school, class, sex) %>% mutate_all(as.factor) %>% summary()
df %>% group_by(school, class, sex) %>% tally()


# 2 スコア集計とクラスごとのプロット -------------------------------------------------

# ＜ポイント＞
# - カテゴリスコアを算出することでデータ次元を圧縮する
# - クラスごとの回帰直線を確認する（マルチレベル分析を簡単に可視化したイメージが得られる）
# - 多くの学級で消耗型競争観(x)と内的調整(y)に負の相関があるが、正の相関を持つクラスもある
#   --- クラスの違いは重要な意味を持つことを示唆（マルチレベル分析の意義を確認）


# 尺度得点の算出
# --- 調査項目のカテゴリごとに平均スコアを算出
df_res <-
  df %>%
    mutate(shoumou = (c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9) / 9,
           seichou = (c10 + c11 + c12 + c13 + c14 + c15 + c16 + c17 + c18 + c19 + c20 + c21) / 12,
           naiteki = (m1 + m2 + m3 + m4 + m5) / 5,
           douitu = (m6 + m7 + m8 + m9 + m10) / 5,
           toriire = (m11 + m12 + m13 + m14 + m15) / 5,
           gaiteki = (m16 + m17 + m18 + m19 + m20) / 5)

# データ確認
df_res %>% glimpse()

# 散布図の確認
# --- クラス別に回帰直線を出力（naiteki ~ shoumou）
df_res %>%
  ggplot(aes(x = shoumou, y = naiteki)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~class)


# 3 級内相関係数の算出 ---------------------------------------------------------------

# ＜ポイント＞
# - 変数の得点の学級間の格差がどの程度であるかを確認するため級内相関係数を算出する
#   --- 級内相関係数の値が大きいほど、クラス間の得点格差が大きいことを意味する（マルチレベル分析の必要性が高まる）


# モデル構築
# --- (1 | class)で切片の値が変動することを示す
model1.naiteki <- lmer(naiteki ~ (1 | class), data = df_res)

# 結果確認
model1.naiteki %>% summary() %>% glimpse()

# 級内相関係数
var_in_class <- 0.04413
var_in_student <- 0.40736
var_in_class / (var_in_class + var_in_student)

# 級内相関係数
ICCest(x = factor(class), y = naiteki, data = df_res) %>%
  use_series(ICC)


# 4 集団平均センタリング --------------------------------------------------------------

# ＜ポイント＞
# - マルチレベル分析では分析データに対してセンタリング(中立化)を行う場合がある
# - 以下の2種類の方法があるが、それぞれによって解釈が異なる点に注意する
#   --- 集団平均センタリング
#   --- 全体平均センタリング


# センタリングの実行
# --- 集団平均センタリング
df_center <-
  df_res %>%
    group_by(class) %>%
    mutate(shoumou.cwc = shoumou - mean(shoumou),
           seichou.cwc = seichou - mean(seichou)) %>%
    ungroup()


# 5 ランダム切片モデル ----------------------------------------------------------------

# ＜ポイント＞
# - ランダム切片モデルとは切片のみが学級によって異なり、傾きは学級間で等しいことを仮定するモデル


# データ確認
df_center %>% select(naiteki, shoumou.cwc, class)

# 散布図
# --- 回帰係数を入れるとクラスごとに傾きが異なっているように見える
df_center %>%
  select(naiteki, shoumou.cwc, class) %>%
  ggplot(aes(x = shoumou.cwc, y = naiteki)) +
  geom_point() +
  #geom_smooth(method = "lm") +
  facet_wrap(~class)

# ランダム切片モデル
# --- (1 | class) ⇒ 切片のみが学級で異なることを意味する
model2.naiteki <- lmer(naiteki ~ shoumou.cwc + (1 | class), data = df_center)

# サマリー
model2.naiteki %>% summary()

# 回帰係数の確認
# --- クラスごとに切片のみが異なっている
model2.naiteki %>% coefficients()


# 6 ランダム傾きモデル ----------------------------------------------------------------

# ＜ポイント＞
# - ランダム傾きモデルとは切片と傾きの


# データ確認
df_center %>% select(naiteki, shoumou.cwc, class)

# 散布図
# --- 回帰係数を入れるとクラスごとに傾きが異なっているように見える
df_center %>%
  select(naiteki, shoumou.cwc, class) %>%
  ggplot(aes(x = shoumou.cwc, y = naiteki)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~class)

# ランダム傾きモデル
# --- (shoumou.cwc | class) ⇒ 傾きが変動する説明変数を入力
model3.naiteki <- lmer(naiteki ~ shoumou.cwc + (shoumou.cwc | class), data = df_center)

# サマリー
model3.naiteki %>% summary()

# 回帰係数の確認
# --- クラスごとに傾きのみが異なっている
model3.naiteki %>% coefficients()


# 7 尤度比検定によるモデル比較 -----------------------------------------------------------

# モデル2とモデル3の適合度の比較
anova(model2.naiteki, model3.naiteki)


# 8 説明変数が2つ以上のモデル ------------------------------------------------------------

# ランダム切片モデル（独立変数が2つ）
model4.naiteki <- lmer(naiteki ~ shoumou.cwc + seichou.cwc + (1 | class), data = df_center)

# ランダム傾きモデル（独立変数が2つ）
model5.naiteki <- lmer(naiteki ~ shoumou.cwc + seichou.cwc +
  (shoumou.cwc + seichou.cwc | class), data = df_center)

# モデルサマリー
model4.naiteki %>% summary()
model5.naiteki %>% summary()

# 回帰係数の確認
model4.naiteki %>% coefficients()
model5.naiteki %>% coefficients()

# モデル4とモデル5の適合度の比較
anova(model4.naiteki, model5.naiteki)
