# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 4 階層重回帰分析
# Theme   : 現象を説明・予測する統計モデルを作る(2)
# Date    : 2022/12/24
# Page    : P92 - P113
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜概要＞
# - 階層的重回帰分析によって交互作用効果を検討する（階層線形モデルとは別議論）
# - 交互作用効果が有意であるときに利用される単純傾斜分析と変数選択についても確認する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 階層的重回帰分析
# 3 重回帰分析における交互作用効果の検討
# 4 単純傾斜分析
# 5 重回帰分析における変数選択


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(conflicted)
library(broom)
library(modelsummary)
library(psych)
library(MASS)
library(StepReg)

# コンフリクト解消
conflict_prefer("select", "dplyr", quiet = TRUE)


# データの読み込み
df_sts <- read_csv("csv/chap04/stress.csv")
df_bsb <- read_csv("csv/chap04/baseball.csv")


# 1 データ確認 -------------------------------------------------------------------

# ＜データ概要＞
# - 教師の燃え尽き症候群の傾向を分析する

# ＜データ項目＞
# - ストレス： Stress
# - サポート： Support
# - バーンアウト1： Burnout1
# - バーンアウト2： Burnout2

# ＜分析目的＞
# - ストレスとバーンアウトの関係はサポートの度合いに影響するかを確認する

#データフレームの確認
df_sts %>% print()
df_sts %>% glimpse()


# 2 階層的重回帰分析 -----------------------------------------------------------

# 2-1 モデル構築 --------------------------------------------------

# ＜ポイント＞
# - モデルに説明変数を追加するプロセスにおける説明力の変化に着目する


# モデル構築
# --- ステップ1： 最も興味のある関係性をモデル化
# --- ステップ2： ステップ1に変数を追加
model_step1 <- lm(Burnout2 ~ Burnout1, data = df_sts)
model_step2 <- lm(Burnout2 ~ Burnout1 + Stress + Support, data = df_sts)

# 結果比較
list(model_step1 = model_step1, model_step2 = model_step2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 2-2 決定係数の増分に関する検定 -------------------------------------

# ＜ポイント＞
# - 説明変数の追加による決定係数の増分に関する検定を行う
#   --- 説明変数を追加的に投入すべきかどうかの意義を示す


# F値の計算パーツ
R2_1 <- model_step1 %>% glance() %>% pull(r.squared)
R2_2 <- model_step2 %>% glance() %>% pull(r.squared)
J_1  <- model_step1 %>% tidy() %>% filter(term != "(Intercept)") %>% nrow()
J_2  <- model_step2 %>% tidy() %>% filter(term != "(Intercept)") %>% nrow()
N    <- df_sts %>% nrow()
df1  <- 2
df2  <- nrow(df_sts) - 3 - 1

# F値の算出
f_stat <- ((R2_2 - R2_1) / (J_2 - J_1)) / ((1 - R2_2) / (N - J_2 - J_1))
f_stat %>% print()

# p値の算出
1 - pf(q = f_stat, df1 = df1, df2 = df2)

# 決定係数の増分の検定
# --- 検定結果は有意であり、決定係数の増加は0よりも有意に大きいことが示された
# --- Burnout1が投入されていても、モデル2で新たに変数を追加する意味があることを示唆
result <- anova(model_step1, model_step2)
result %>% print()
result %>% tidy()


# 2-3 AICとBICによるモデル比較 --------------------------------------

# ＜ポイント＞
# - AICやBICなどの情報量基準は水準が小さいほうが良いことを示す
#   --- モデルに変数を追加することをペナルティとしたうえで評価

# ＜変更点＞
# - stats::extractAIC()からstats::AIC()に変更


# AICの算出
# --- step1：219.317
# --- step2：216.772
model_step1 %>% AIC()
model_step2 %>% AIC()

# BICの算出
# --- step1：230.4283
# --- step2：235.2912
model_step1 %>% BIC()
model_step2 %>% BIC()


# 3 重回帰分析における交互作用効果の検討 -----------------------------------------------

# ＜ポイント＞
# - 交互作用効果は変数の積をモデルに投入することによって分析する
#   --- 元の変数との多重共線性を避けるため、中心化して積を算出する
#   --- 標準化回帰係数の場合はZスコア変換した項の積をとる（積のZスコアではない）


# データ準備
# --- 中心化
# --- Interaction項の作成
df_sts_2 <-
  df_sts %>%
    mutate(Interaction = Stress * Support)

df_sts_2_center <-
  df_sts %>%
    mutate(Stress = Stress - mean(Stress),
           Support = Support - mean(Support),
           Interaction = Stress * Support,
           Burnout1 = Burnout1 - mean(Burnout1))

# 相関係数の確認
# --- 中心化なし
# --- 中心化あり
df_sts_2 %>% select(Stress, Support, Interaction) %>% pairs.panels()
df_sts_2_center %>% select(Stress, Support, Interaction) %>% pairs.panels()

# モデル構築
# --- Interaction効果の検討
# --- 関数lmによる重回帰分析の実行
model_base <- lm(Burnout2 ~ Burnout1 + Stress + Support, data = df_sts_2)
model_interact <- lm(Burnout2 ~ Burnout1 + Stress + Support + Stress * Support, data = df_sts_2_center)

# モデルサマリー
list(model_base = model_base, model_interact = model_interact) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")

# 回帰係数の確認
model_base %>% tidy()
model_interact %>% tidy()

# 決定係数の増分の検定
anova(model_base, model_interact) %>% tidy()

# 標準化データの作成
df_sts_z <-
  df_sts %>%
    scale() %>%
    as.data.frame()

# モデル構築
# --- 標準偏回帰係数の算出
model_interact_z <- lm(Burnout2 ~ Burnout1 + Stress + Support + Stress * Support, data = df_sts_z)

# モデルサマリー
list(model_base = model_base,
     model_interact = model_interact,
     model_interact_z = model_interact_z) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")



# 4 単純傾斜分析 -----------------------------------------------------------------

df_sts_3 <-
  df_sts %>%
    mutate(Stress.c = Stress - mean(Stress),
           Support.c = Support - mean(Support),
           Interaction = Stress * Support,
           Interaction.c = Stress.c * Support.c,
           Burnout1.c = Burnout1 - mean(Burnout1),
           Support.h = Support.c - sd(Support.c),
           Support.l = Support.c - sd(Support.c)) %>%
    select(Burnout2, Burnout1.c, Stress.c, Support.h)


#単純傾斜分析（ソーシャル・Supportが多い場合のBurnoutに対するStress経験の効果）
res3.h <- lm(Burnout2 ~ Burnout1.c + Stress.c + Support.h + Stress.c * Support.h, data = df_sts)
summary(res3.h)

#単純傾斜分析（ソーシャル・Supportが少ない場合のBurnoutに対するStress経験の効果）
res3.l <- lm(Burnout2 ~ Burnout1.c + Stress.c + Support.l + Stress.c * Support.l, data = df_sts)
summary(res3.l)

#信頼区間の算出
confint(res3)


# 5 野球データの確認 -------------------------------------------------------------

df_bsb %>% print()
df_bsb %>% glimpse()


# 6 重回帰分析における変数選択 -----------------------------------------------------


# 6-1 MASS::stepAIC() ------------------------------------------

# ベースモデル
# --- 切片のみのモデル
base_model <- lm(annual_salary ~ 1, data = df_bsb)

# ステップワイズ法による変数選択
step_model_step1 <-
  stepAIC(base_model,
          direction = "both",
          keep = TRUE,
          scope = list(upper = ~strokes + hit + RBI + homerun + walk + pitch + strikeout + bating_ratio))

# 最終的な変数選択の結果
step_model_step1 %>% summary()


# 6-2 StepReg::stepwise() ------------------------------------------

step_model_step2 <-
  stepwise(formula = annual_salary ~ strokes + hit + RBI + homerun + walk + pitch + strikeout + bating_ratio,
           data = df_bsb,
           selection = "score",
           select = "AIC")

# 最終的な変数選択の結果
step_model_step2$`Coefficients of the Selected Variables`
step_model_step2$`Selected Varaibles`
step_model_step2$`Summary of Parameters`
step_model_step2$`Variables Type`

# 探索プロセス
step_model_step2$`Process of Selection`
