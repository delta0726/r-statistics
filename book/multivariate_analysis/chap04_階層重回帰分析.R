# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 4 階層重回帰分析
# Theme   : 現象を説明・予測する統計モデルを作る(2)
# Date    : 2022/12/18
# Page    : P92 - P113
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************



# ＜目次＞
# 0 準備
# 1 データ確認
# 2 階層定期重回帰分析
# 3 重回帰分析におけるInteraction効果の検討
# 4 単純傾斜分析
# 5 重回帰分析における変数選択


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(modelsummary)
library(psych)
library(MASS)
library(StepReg)


# データの読み込み
sts <- read_csv("csv/chap04/stress.csv")
bsb <- read_csv("csv/chap04/baseball.csv")


# 1 データ確認 -------------------------------------------------------------------

#データフレームの確認
sts %>% print()
sts %>% glimpse()



# 2 階層定期重回帰分析 -----------------------------------------------------------


# 2-1 モデル構築 --------------------------------------------------


# モデル構築
# --- ステップ1： 最も興味のある関係性をモデル化
# --- ステップ2： ステップ1に変数を追加
res1 <- lm(Burnout2 ~ Burnout1, data = sts)
res2 <- lm(Burnout2 ~ Burnout1 + Stress + Support, data = sts)

# 結果比較
list(res1 = res1, res2 = res2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 2-2 決定係数の増分に関する検定 -------------------------------------

# F値の計算パーツ
R2_1 <- res1 %>% glance() %>% pull(r.squared)
R2_2 <- res2 %>% glance() %>% pull(r.squared)
J_1 <- res1 %>% tidy() %>% filter(term != "(Intercept)") %>% nrow()
J_2 <- res2 %>% tidy() %>% filter(term != "(Intercept)") %>% nrow()
N <- sts %>% nrow()

# F値の算出
f_stat <- ((R2_2 - R2_1) / (J_2 - J_1)) / ((1 - R2_2) / (N - J_2 - J_1))
f_stat %>% print()

# p値の算出
1 - pf(q = f_stat, 2, 296)

# 決定係数の増分の検定
anova(res1, res2)


# 2-3 AICとBICによるモデル比較 --------------------------------------

#AICの算出
res1 %>% extractAIC()
res2 %>% extractAIC()

#BICの算出
res1 %>% extractAIC( k = log(300))
res2 %>% extractAIC(k = log(300))


# 3 重回帰分析における相互効果の検討 -----------------------------------------------

# データ準備
# --- 中心化
# --- Interaction項の作成
sts_2 <-
  sts %>%
    mutate(Stress.c = Stress - mean(Stress),
           Support.c = Support - mean(Support),
           Interaction = Stress * Support,
           Interaction.c = Stress.c * Support.c,
           Burnout1.c = Burnout1 - mean(Burnout1))

# 相関係数の確認
sts_2 %>% select(Stress, Support, Interaction) %>% cor()
sts_2 %>% select(Stress.c, Support.c, Interaction.c) %>% cor()

# 関数lmによる重回帰分析の実行（Interaction効果の検討）
res3 <- lm(Burnout2 ~ Burnout1.c + Stress.c +  Support.c + Stress.c * Support.c, data = sts_2)
summary(res3)

# 決定係数の増分の検定
anova(res2, res3)


# 標準化データの作成
z.sts <- sts %>% scale() %>% as.data.frame()

# 標準偏回帰係数の算出
res3.z <- lm(Burnout2 ~ Burnout1 + Stress + Support + Stress * Support, data = z.sts)

res3.z %>% summary()


# 4 単純傾斜分析 -----------------------------------------------------------------

sts_3 <-
  sts %>%
    mutate(Stress.c = Stress - mean(Stress),
           Support.c = Support - mean(Support),
           Interaction = Stress * Support,
           Interaction.c = Stress.c * Support.c,
           Burnout1.c = Burnout1 - mean(Burnout1),
           Support.h = Support.c - sd(Support.c),
           Support.l = Support.c - sd(Support.c)) %>%
    select(Burnout2, Burnout1.c, Stress.c, Support.h)


#単純傾斜分析（ソーシャル・Supportが多い場合のBurnoutに対するStress経験の効果）
res3.h <- lm(Burnout2 ~ Burnout1.c + Stress.c + Support.h + Stress.c * Support.h, data = sts)
summary(res3.h)

#単純傾斜分析（ソーシャル・Supportが少ない場合のBurnoutに対するStress経験の効果）
res3.l <- lm(Burnout2 ~ Burnout1.c + Stress.c + Support.l + Stress.c * Support.l, data = sts)
summary(res3.l)

#信頼区間の算出
confint(res3)


# 5 野球データの確認 -------------------------------------------------------------

bsb %>% print()
bsb %>% glimpse()


# 6 重回帰分析における変数選択 -----------------------------------------------------


# 6-1 MASS::stepAIC() ------------------------------------------

# ベースモデル
# --- 切片のみのモデル
base_model <- lm(annual_salary ~ 1, data = bsb)

# ステップワイズ法による変数選択
step_res1 <-
  stepAIC(base_model,
          direction = "both",
          keep = TRUE,
          scope = list(upper = ~strokes + hit + RBI + homerun + walk + pitch + strikeout + bating_ratio))

# 最終的な変数選択の結果
step_res1 %>% summary()


# 6-2 StepReg::stepwise() ------------------------------------------

step_res2 <-
  stepwise(formula = annual_salary ~ strokes + hit + RBI + homerun + walk + pitch + strikeout + bating_ratio,
           data = bsb,
           selection = "score",
           select = "AIC")

# 最終的な変数選択の結果
step_res2$`Coefficients of the Selected Variables`
step_res2$`Selected Varaibles`
step_res2$`Summary of Parameters`
step_res2$`Variables Type`

# 探索プロセス
step_res2$`Process of Selection`
