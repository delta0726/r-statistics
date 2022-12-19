# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 6 パス解析
# Theme   : 複雑な仮説を統計モデルとして表したい
# Date    : 2022/12/20
# Page    : P138 - P159
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 パス解析
# 3 サマリーの確認
# 4 モデルの評価指標
# 5 モデルの修正


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lavaan)


# データの読み込み
df <- read_csv("csv/chap06/failure.csv")


# 1 データ確認 --------------------------------------------------------------------

# ＜データ概要＞
# - 子供がテストで悪い点数を取った時の親の態度

# ＜データ項目＞
# - 叱責(Rebuke)
# - 励まし(Encouragement)
# - 失敗不安(Fear)
# - 学習意欲(Willingness)
# - 学業成績(Performance)


# データ確認
df %>% head()
df %>% glimpse()


# 2 パス解析 ------------------------------------------------------------------------

#モデルの記述
df.model <- "
 Fear ~ Rebuke
 Willingness ~ Encouragement
 Performance ~ Fear + Willingness
"

# 母数の推定
df.fit <- sem(df.model, data = df)

# 結果出力
df.fit %>% print()


# 3 サマリーの確認 -------------------------------------------------------------------

# サマリー
# --- 標準化推定値，決定係数
# --- 標準化推定値，決定係数，信頼区間
df.fit %>% summary(standardized = TRUE, rsquare = TRUE)
df.fit %>% summary(standardized = TRUE, rsquare = TRUE, ci = TRUE)


# 4 モデルの評価指標 -------------------------------------------------------------------

# サマリー
# --- 標準化推定値，決定係数，信頼区間，適合度指標
df.fit %>% summary(standardized = TRUE, rsquare = TRUE, ci = TRUE, fit.measures = TRUE)

# 適合度指標
# --- すべての統計量を出力
df.fit %>% fitmeasures()


# 5 モデルの修正 ------------------------------------------------------------------------

# 修正指標の出力
df.fit %>% modindices()

# 修正モデル
# --- 誤差変数の間に相関を仮定したモデルで分析
df.model2 <- "
 Fear ~ Rebuke
 Willingness ~ Encouragement
 Performance ~ Fear + Willingness
 Fear ~~ Willingness
"

# モデル構築
# --- 母数の推定
df.fit2 <- sem(df.model2, data = df)

# 結果出旅行
df.fit2 %>% summary(standardized = TRUE, rsquare = TRUE, ci = TRUE, fit.measures = TRUE)

# 標本共分散行列をもとにした分析
df.cov <- cov(df) #標本共分散行列の算出
df.cov.fit <- df.model2 %>% sem(sample.cov = df.cov, sample.nobs = 500) #母数の推定
summary(df.cov.fit, standardized = TRUE, rsquare = TRUE, ci = TRUE, fit.measures = TRUE) #結果の出力

