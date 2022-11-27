# ***********************************************************************************************
# Title   : 平均・分散から始める一般化線形モデル入門
# Chapter : 6 一般化線形モデル
# Theme   : 3 一般化線形モデルの推定
# Date    : 2022/11/27
# Page    : P227 - P242
# URL     : https://logics-of-blue.com/
# ***********************************************************************************************


# ＜概要＞
# - 最尤法を用いると正規分布以外でもパラメータ推定をすることが可能


# ＜目次＞
# 0 準備
# 1 ポアソン分布
# 2 ポアソン分布のグラフ
# 3 ポアソン回帰のパーツ
# 4 線形予測子に従ってlambdaを変化させる
# 5 最適化によるポアソン回帰のパラメータ推定
# 6 glmによる一般化線形モデルの推定
# 7 線形予測子から求めた予測結果
# 8 応答変数の予測値
# 9 線形予測子の係数の解釈


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(broom)


# 1 ポアソン分布 ---------------------------------------------------------------

# ＜ポイント＞
# - ポアソン分布は個数や回数といった離散値(整数)を表す確率分布
# - 推定すべきパラメータはλのみ
#   --- ポアソン分布においては期待値/分散ともにλと等しくなる


# 階乗の計算
# --- ポアソン分布のパーツ
factorial(3)

# ポアソン分布の公式
# --- λが5のポアソン分布で7が出る確率
lambda <- 5
lambda^7 * exp(-lambda) / factorial(7)

# 関数を使用
# --- dpoisで代用
dpois(lambda = 5, x = 7)


# 2 ポアソン分布のグラフ ----------------------------------------------------------

# 数列
x <- 0:30

# ポアソン分布のグラフ
lambda_2 <- data.frame(lambda = 2, x, y = dpois(lambda = 2, x))
lambda_5 <- data.frame(lambda = 5, x, y = dpois(lambda = 5, x))
lambda_7 <- data.frame(lambda = 7, x, y = dpois(lambda = 7, x))
lambda_10 <- data.frame(lambda = 10, x, y = dpois(lambda = 10, x))
lambda_20 <- data.frame(lambda = 20, x, y = dpois(lambda = 20, x))

lambda_2 %>%
  bind_rows(lambda_5) %>%
  bind_rows(lambda_7) %>%
  bind_rows(lambda_10) %>%
  bind_rows(lambda_20) %>%
  ggplot(aes(x = x, y = y, group = lambda)) +
  geom_line() +
  geom_point() +
  ylab("Probability") +
  labs(title = "Poisson Distribution")


# 3 ポアソン回帰のパーツ -----------------------------------------------------------

# データ作成
d6 <- data.frame(esa = c(1, 2, 3, 4),
                 neko = c(4, 10, 7, 14))

# 確認
d6 %>% print()


# 確率分布からの確率算出
# --- λ＝5のポアソン分布で4,10,7,14という値が出る確率
pPois <- dpois(lambda = 5, d6$neko)
pPois %>% print()

# 尤度
# --- λ＝5の時の尤度
likelihood <- pPois[1] * pPois[2] * pPois[3] * pPois[4]
likelihood %>% print()

# 対数尤度
# --- λ＝5の時の対数尤度
likelihood %>% log()
sum(log(dpois(lambda = 5, d6$neko)))


# 4 線形予測子に従ってlambdaを変化させる ---------------------------------------------

# λを変化させる
# --- 線形予測子(0.3 * x + 1)により変化すると想定
a <- 0.3
b <- 1
lambdaHat <- exp(a * d6$esa + b)

# 確認
lambdaHat %>% print()

# データが得られる確率
dpois(lambda = lambdaHat, d6$neko)

# 対数尤度
sum(log(dpois(lambda = lambdaHat, d6$neko)))


# 5 最適化によるポアソン回帰のパラメータ推定 ---------------------------------------------

# 関数定義
# --- 対数尤度の－1倍を計算する関数
calcLogLikPoisson <- function(para) {
  a <- para[1]
  b <- para[2]
  lambdaHat <- exp(a * d6$esa + b)
  logLik <- -1 * sum(log(dpois(lambda = lambdaHat, d6$neko)))
  return(logLik)
}

# 関数実行
# --- 線形予測子のパラメータを与える
calcLogLikPoisson(para = c(0.3, 1))

# 尤度を最大にする係数を推定する
result <- optim(c(0.3, 1), calcLogLikPoisson)

# 結果確認
# --- パラメータ
# --- 対数尤度(-1を掛けて考える)
result$par %>% print()
result$value %>% print()


# 6 glmによる一般化線形モデルの推定 ---------------------------------------------------

# データ確認
d6 %>% print()

# モデル構築
# --- glm関数による一般化線形モデルの推定
modelPoisson <- glm(neko ~ esa, family = "poisson", data = d6)

# 結果確認
modelPoisson %>% tidy()
modelPoisson %>% glance()


# 7 線形予測子から求めた予測結果 -------------------------------------------------------

# ＜ポイント＞
# - 線形予測子による予測結果はtype引数をlinkとしてpredict関数を実行する


# 係数の取得
modelPoisson$coef
seppen <- as.numeric(modelPoisson$coef[1])
katamuki <- as.numeric(modelPoisson$coef[2])

# 予測値の取得
# --- 手動計算
seppen + katamuki * 4

# 予測値の取得
# --- predict()による予測
predData1 <- data.frame(esa = 4)
modelPoisson %>% predict(predData1, type = "link")


# 8 応答変数の予測値 -----------------------------------------------------------------

# ＜ポイント＞
# - 応答変数はリンク関数の逆関数、すなわちexp()を適用した値となる
# - predict関数ではtype引数をresponseとして実行する


# 予測値の取得
# --- 手動計算
exp(seppen + katamuki * 4)

# 予測値の取得
# --- 関数の利用
predict(modelPoisson, predData1, type = "response")


# 9 線形予測子の係数の解釈 -----------------------------------------------------------------

# 予測値の取得
pred1 <- modelPoisson %>% predict(data.frame(esa = 1), type = "response")
pred2 <- modelPoisson %>% predict(data.frame(esa = 2), type = "response")

# 予測値の比
pred2 / pred1

# 回帰係数
exp(modelPoisson$coefficients[2])


# 10 一般化線形モデルによりグラフに引かれた線の意味 -------------------------------------------

# 予測用データの作成
predData2 <- tibble(esa = seq(0, 5, 0.1))
predData2 %>% print()

# 予測値の作成
predGurahu <- modelPoisson %>% predict(predData2, type = "response")

# プロット作成
plot(neko ~ esa, pch = 16, cex = 2, data = d6, main = "ポアソン回帰")
lines(predGurahu ~ predData2$esa, lwd = 2)


# 予測値の意味
pred1 <- modelPoisson %>% predict(data.frame(esa = 1), type = "response")
pred1

# 餌の量が1の時に、猫の個体数が7になる確率
dpois(lambda = pred1, 7)

# 餌の量が1の時に、猫の個体数が10になる確率
dpois(lambda = pred1, 10)
