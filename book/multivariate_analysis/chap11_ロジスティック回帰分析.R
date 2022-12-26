# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 11 ロジスティック回帰分析
# Theme   : カテゴリに所属する確率を説明/予測したい
# Date    : 2022/12/27
# Page    : P262 - P283
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 ロジスティック回帰モデルの構築
# 3 回帰係数と切片の解釈
# 4 モデル評価
# 5 その他の有益な評価指標


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ResourceSelection)
library(car)


# データロード
aks <- read_csv("csv/chap11/sneak_thief.csv")


# 1 データ確認 -------------------------------------------------------------------

# ＜データ概要＞
# - 空き巣の有無を含めた800戸の調査データ

# ＜データ項目＞
# - 空き巣：Sneak_thief
# - Absent_time：Absent_time
# - Conversation：Conversation
# - House_age：House_age
# - セキュリティ：Security
# - 飼い犬：Pet_dog

# ＜分析目的＞
# - 空き巣被害に対して各項目がどのように影響するかを分析する


# データ確認
aks %>% print()
aks %>% glimpse()


# 2 ロジスティック回帰モデルの構築 --------------------------------------------------

# モデル構築
aks.out <- glm(Sneak_thief ~ Absent_time + Conversation + House_age + Security + Pet_dog , family = "binomial", data = aks)

# サマリー
aks.out %>% summary()


# 3 回帰係数と切片の解釈 ------------------------------------------------------------


#係数・切片の指数変換値の算出と解釈
exp(aks.out$coefficients)

#係数・切片に関する信頼区間の算出
confint(aks.out, level = 0.95)
exp(confint(aks.out, level = 0.95))

#標準化係数の算出
LRAstdcoef <- function(glm.out, vname){ #glm.out-関数glmによる出力オブジェクト, vname-標準化する量的な説明変数名
	#vnameに指定された変数のみからなるデータフレーム
	subdat <- (glm.out[["data"]])[ , vname]
	#subdatの各変数の標準偏差
	SDs <- apply(subdat, 2, sd)
	#標準化前の推定値
	rawcoef <- (glm.out[["coefficients"]])[vname]
	#標準化後の推定値
	stdcoef <- rawcoef * SDs
	return(stdcoef)
}
LRAstdcoef(aks.out,c("Absent_time", "Conversation", "House_age"))


# 4 モデル評価 ------------------------------------------------------------------------

#Hosmer-Lemeshowの適合度検定
hoslem.test(x = aks.out$y, y = fitted(aks.out)) #x-目的変数の観測値, y-目的変数の予測値（確率）

#AICとBICの算出
extractAIC(aks.out) #AIC
extractAIC(aks.out, k = log(nrow(aks.out$data))) #BIC


# 5 その他の有益な評価指標 --------------------------------------------------------------

#独立変数群の有効性の確認
aks.out_null <- glm(Sneak_thief~1, family = "binomial", data = aks)
anova(aks.out_null, aks.out, test="Chisq")

#変数選択の実行
step(aks.out_null, direction = "both", scope = ( ~ Absent_time + Conversation + House_age + Security + Pet_dog ))

#多重共線性の確認
vif(aks.out)
