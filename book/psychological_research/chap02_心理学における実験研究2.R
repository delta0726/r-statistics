# ***********************************************************************************************
# Title   : Rによる心理学研究法入門
# Chapter : 2 心理学における実験研究
# Date    : 2023/1/13
# Page    : P35 - P54
# URL     : https://www.kitaohji.com/news/n41183.html
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ概要
# 2 γ係数とは
# 3 記述統計的分析
# 4 1要因の分散分析
# 5 共分散分析
# 6 多重比較


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(PResiduals)
library(effects)
library(multcomp)


# データロード
df <- read_csv("csv/explanation.csv")


# 1 データ概要 -------------------------------------------------------------------

# ＜データ概要＞
# -

# ＜データ項目＞
# -


# データ確認
df %>% print()
df %>% glimpse()


# 2 γ係数とは ------------------------------------------------------------------

# 行列の作成
gamma1 <- rbind(c(1, 0, 1), c(0, 1, 0), c(0, 0, 2))

# データ確認
gamma1 %>% print()

# γ係数の算出
gamma1 %>% GKGamma()


# 3 記述統計的分析 ---------------------------------------------------------------

attach(df) # データフレームの指定

# 度数分布表の算出
# --- 条件
# --- 既有知識
df$condition %>% table()
df$priorknowledge %>% table()

# 記述統計量の算出
# --- 平均の算出（γ係数）
# --- 標準偏差の算出（γ係数）
df$accuracy %>% mean()
df$accuracy %>% sd()

# 記述統計量の算出（グループ別
# --- 平均の算出（γ係数）
# --- 標準偏差の算出（γ係数）
df %>% group_by(condition) %>% summarise(accuracy = mean(accuracy))
df %>% group_by(condition) %>% summarise(accuracy = sd(accuracy))


# 4 1要因の分散分析 ----------------------------------------------------------------

# データ準備
df <- df %>% mutate(condition2 = as.factor(condition))

# 分散分析の実行
fit <- aov(accuracy ~ condition2, data = df)

# サマリー
fit %>% summary()

# バーレット検定（分散等質性の検証）
bartlett.test(accuracy ~ condition2)


# 5 共分散分析 ---------------------------------------------------------------------

# 共分散分析
fit2 <- aov(accuracy ~ condition2 + priorknowledge, data = df)

# サマリー
fit2 %>% summary()

# 共分散分析の仮定の検討
fit3 <- aov(accuracy ~ condition2 * priorknowledge)

# サマリー
fit3 %>% summary()

# 調整した平均の算出
effect("condition2", fit2)


# 6 多重比較 -------------------------------------------------------------------------

#多重比較
TukeyHSD(fit)

#ダミー変数をcontrastに格納
contrast <- rbind("dummy 1" = c(-1, 0, 1), "dummy 2" = c(0, -1, 1))

#ダミー変数の表示
contrast

# 多重比較の結果の出力
fit2 %>%
  glht(linfct = mcp(condition2 = contrast)) %>%
  summary()
