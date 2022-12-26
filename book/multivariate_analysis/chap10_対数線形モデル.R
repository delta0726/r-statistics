# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 10 対数線形モデル
# Theme   : クロス集計表をもっと丁寧に分析したい
# Date    : 2022/12/27
# Page    : P238 - P261
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜概要＞
# - 対数線形モデルは、分割表の各セルにおける期待値を対数変換して各属性の主効果と交互作用で説明するモデル
# - クロス集計表のような分析を多次元に拡大することを目指す


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 クロス集計表の作成
# 3 飽和モデルの分析
# 4 独立モデルの分析
# 5 最良モデルの探索
# 6 母数と期待度数


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(psych)
library(rpivotTable)


# データロード
bdat <-
  read_csv("csv/chap10/bicycle.csv") %>%
    mutate(Age = as.character(Age))


# 1 データ確認 -------------------------------------------------------------------

# ＜データ概要＞
# - AgeとSexごとに所有自転車のMakerを調査（集計値を収録）

# ＜データ項目＞
# - Age：Age
# - Sex：Sex
# - Maker：Maker
# - Freq：Freq

# ＜分析目的＞
# - ｢Age｣｢Sex｣｢Maker｣にどのような連関があるかを確認
#   --- いずれもカテゴリカル変数


# データ確認
bdat %>% print()
bdat %>% glimpse()
bdat$Freq %>% sum()


# 2 クロス集計表の作成 ----------------------------------------------------------

# ＜ポイント＞
# - クロス集計は｢2つの質的変数｣と｢1つの量的変数｣の連関を分析することができる
#   --- 次元数が多くなると使い勝手が悪い


# 頻度の集計
# --- 男性
bdat %>%
  filter(Sex == "M") %>%
  rpivotTable(rows = "Age", cols = "Maker", aggregatorName = "Sum", vals = "Freq")

# 頻度の集計
# --- 女性
bdat %>%
  filter(Sex == "F") %>%
  rpivotTable(rows = "Age", cols = "Maker", aggregatorName = "Sum", vals = "Freq")


# 3 飽和モデルの分析 ----------------------------------------------------------

# ＜ポイント＞
# - 飽和モデルは3変数の主効果と交互効果をすべて出力する


# モデル構築
# --- 3変数の主効果と交互効果をすべて出力（飽和モデル）
# --- 頻度データのため一般化線形モデルでポアソン分布を使う
fullmodel <- glm(Freq ~ Age * Sex * Maker, data = bdat, family = "poisson")

# 飽和モデルの出力
fullmodel %>% summary()


# 4 独立モデルの分析 ----------------------------------------------------------

# ＜ポイント＞
# - 主効果のみで構成される独立モデルは飽和モデルから乖離している


# モデル構築
# --- 3変数の主効果のみ（独立モデル）
idmodel <- glm(Freq ~ Age + Sex + Maker, data = bdat, family = "poisson")

# 独立モデルの出力
idmodel %>% summary()

# 独立モデルの分散分析表
anova(idmodel, fullmodel, test = "Chisq")

# モデル評価
# --- AICとBIC
fullmodel %>% AIC()
idmodel %>% AIC()
fullmodel %>% BIC()
idmodel %>% BIC()


# 5 最良モデルの探索 -----------------------------------------------------------

# ＜ポイント＞
# - 独立モデルに交互作用を含めたほうが当てはまりが良いモデルとなる


# モデル構築
# --- 特定の交互作用効果を含めたモデル
bestmodel <- glm(Freq ~ Age + Sex + Maker + (Age:Sex) + (Age:Maker), data = bdat, family = "poisson")

#関数glmによる提案モデルの出力
bestmodel %>% summary()

#飽和モデルと提案モデルの尤度比検定
anova(bestmodel, fullmodel, test = "Chisq")


# 6 母数と期待度数 ---------------------------------------------------------------

#最良モデルの期待Freq行列
xtabs(bestmodel$fitted.values ~ bdat$Age + bdat$Maker + bdat$Sex)


#飽和モデルのもとでの期待Freq行列
xtabs(fullmodel$fitted.values ~ bdat$Age + bdat$Maker + bdat$Sex)


#基準セルの設定
bdat$Age <- factor(bdat$Age, levels = c("30代", "20代", "40代"))
bdat$Sex <- factor(bdat$Sex, levels = c("M", "F"))
bdat$Maker <- factor(bdat$Maker, levels = c("ピロリロ", "コレナゴ", "デロンザ"))

