# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 9 潜在変数を伴うパス解析
# Theme   : 複雑な仮定を統計モデルとして表したい(2)
# Date    : 2023/1/5
# Page    : P209 - P235
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜パス解析の手順＞
# - 1： 仮説の設定
# - 2： モデルによる表現
# - 3： モデル評価
# - 4： モデル指標の解釈、結果の考察


# ＜目次＞
# 0 準備
# 1 データ確認(幸せ調査)
# 2 モデルの記述
# 3 モデリング
# 4 モデル評価
# 5 パス図の描画
# 6 母数の関数の定義を含むモデル


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(lavaan)
library(semPlot)
library(broom)


# データの読み込み
sws <- read_csv("csv/chap09/happiness.csv")


# 1 データ確認(幸せ調査) ----------------------------------------------------------

# ＜データ概要＞
# - ｢幸せ｣を定義するため質問事項と回答を250人からアンケートで収集

# ＜データ項目＞
# - E1-E3： 経済状況への肯定感
# - R1-R3： 人間関係の良好さ
# - M1-M3： 心の健康
# - H1-H3： 幸福感

# ＜分析目的＞
# - 4つの概念間の関係の確からしさをデータから確認し関係性の強さを解釈する
#   --- A君の考えた概念図は正しいか？（P211 図9.2）


# データフレームの確認
sws %>% print()
sws %>% glimpse()
sws %>% mutate_all(as.factor) %>% summary()


# 2 モデルの記述 ---------------------------------------------------------------

# モデルの記述
# --- 係数の固定
sws.model1 <- "
  f1 =~ 1 * E1 + E2 + E3
  f2 =~ 1 * R1 + R2 + R3
  f3 =~ 1 * M1 + M2 + M3
  f4 =~ 1 * H1 + H2 + H3
  f3 ~ f1 + f2
  f4 ~ f1 + f2 +f3
  f1 ~~ f2
"

# モデルの記述
# --- 分散の固定
sws.model2 <- "
f1 =~ E1 + E2 + E3
f2 =~ R1 + R2 + R3
f3 =~ 1 * M1 + M2 + M3
f4 =~ 1 * H1 + H2 + H3
f3 ~ f1 + f2
f4 ~ f1 + f2 + f3
f1 ~~ 1 * f1 + f2
f2 ~~ 1 * f2
"


# 3 モデリング --------------------------------------------------------------------

# モデル構築
# --- auto.var-外生変数の分散を推定対象とする（TRUE）,しない（FALSE）
sws.fit <- sws.model1 %>% lavaan(data = sws, auto.var = TRUE)

# 結果の出力
# --- fit.measures-適合度, standardizes-標準化推定値, ci-95%信頼区間を出力する（TRUE）
sws.fit %>% summary(fit.measures = TRUE, standardized = TRUE, ci = TRUE)


# 4 モデル評価 ---------------------------------------------------------------------

# すべての適合度指標を出力
sws.fit %>% fitmeasures()

#モデル適合に関する部分的評価の指標（残差行列）
residuals(sws.fit, type = "cor")

#モデル適合に関する部分的評価の指標（修正指標）
sws.fit %>% modindices()

#分散説明率の出力
sws.fit %>% lavInspect("rsquare")


# 5 パス図の描画 --------------------------------------------------------------------

#推定値付パス図の描画
sws.fit %>%
	semPaths(whatLabels = "std", layout = "tree2", curve = 1.2,
			 optimizeLatRes = TRUE, edge.color = "black", nCharNodes = 0,
			 edge.label.position = c(rep(0.4, 17), rep(0.5, 18)), edge.label.cex = 0.8)


# 6 母数の関数の定義を含むモデル ------------------------------------------------------

sws.model3 <- "
	f1 =~ 1 * E1 + E2 + E3
	f2 =~ 1 * R1 + R2 + R3
	f3 =~ 1 * M1 + M2 + M3
	f4 =~ 1 * H1 + H2 + H3
	f3 ~ f1 + a * f2
	f4 ~ f1 + b * f2 +c * f3
	f1 ~~ f2
	DRE  := b
	IDRE := a * c
	TTE  := b + a * c
"

# モデル構築
sws.fit <- sws.model3 %>% lavaan(data = sws, auto.var = TRUE)

# 結果確認
sws.fit %>% summary(fit.measures = TRUE, standardized = TRUE, ci = TRUE)
