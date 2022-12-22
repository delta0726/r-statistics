# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 3 重回帰分析
# Theme   : 現象を説明・予測する統計モデルを作る
# Date    : 2022/12/23
# Page    : P68 - P91
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜概要＞
# - 重回帰分析の構築方法と評価手順を学ぶ
# - モデル操作でどこに影響が出るのかをモデル比較を通して確認する


# ＜目次＞
# 0 準備
# 1 データ概要と分析目的の確認
# 2 重回帰モデルの構築
# 3 多重共線性のチェック
# 4 多重共線性問題の影響範囲
# 5 偏回帰係数の信頼区間
# 6 標準偏回帰係数の算出
# 7 質的変数を含む重回帰分析
# 8 AICとBICによるモデル評価
# 9 切片と編回帰係数の検定


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(modelsummary)
library(car)


# データロード
csdat <- read_csv("csv/chap03/customer_satisfaction.csv")


# 1 データ概要と分析目的の確認 ----------------------------------------------------

# ＜データ概要＞
# - フィットネスジムのアンケート調査

# ＜データ項目＞
# - 店舗番号： no
# - 顧客数： client_n
# - 立地満足度： location
# - 設備満足度： facility
# - 店舗面積満足度： area
# - トレーナー満足度： trainer
# - トレーナー数： trainer_n
# - 接客研修： training_flag
# - 入会特典： privilege_flag

# ＜分析目的＞
# - 顧客数を目的変数(Y)として、各変数がYに与える影響を重回帰分析により明らかにする

# ＜分析手順＞
# - 1. 回帰モデルの作成
# - 2. モデルにおける母数の推定
# - 3. モデル診断による評価
# - 4. 偏回帰係数の解釈


# データ確認
csdat %>% print()
csdat %>% glimpse()


# 2 重回帰モデルの構築 ----------------------------------------------------------

# モデル構築
res1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)

# モデルサマリー
res1 %>% summary()

# モデル比較
list(res1 = res1) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 3 多重共線性のチェック ----------------------------------------------------------

# ＜ポイント＞
# - 説明変数間の相関係数が高い場合に多重共線性問題が発生することがある
# - VIFにより多重共線性を生み出す回帰係数を判別することができる（事後分析）
#   --- 事前排除する場合は、高相関の変数の一方を前処理で排除する

# ＜多重共線性の影響＞
# - 該当する回帰係数と切片の標準誤差が高まる（t値も下がる）

# ＜VIFの解釈＞
# - 2未満であることが望ましく、10を超えると多重共線性が疑われる


# 説明変数間の相関係数
# --- 0.984
csdat %$% cor(trainer, trainer_n)

# モデル構築
# --- 相関の高い変数を含む
resm1 <- lm(client_n ~ trainer, data = csdat)
resm2 <- lm(client_n ~ trainer + trainer_n, data = csdat)

# モデル比較
# --- モデル2は標準誤差がとても大きくなっている
list(model1 = resm1, model2 = resm2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")

# VIFの算出
# --- 2変数以上ないとVIFは計算できない
# --- 10を超えると多重共線性が疑われる
resm2 %>% vif()

# 2で作成したモデルのVIF
# --- 問題なし
res1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)
res1 %>% vif()


# 4 多重共線性問題の影響範囲 ------------------------------------------------------

# ＜ポイント＞
# - 多重共線性は高相関の説明変数と切片のみに影響が出る
#   --- その他の説明変数には影響がでない
#   --- モデル全体の説明力(R2)にも影響はでない


# モデル構築
# --- 多重共線性のない変数(area)を追加
exp_md1 <- lm(client_n ~ area + trainer, data = csdat)
exp_md2 <- lm(client_n ~ area + trainer + trainer_n + area, data = csdat)

# モデル比較
# --- モデル2は標準誤差がとても大きくなっている
list(model1 = exp_md1, model2 = exp_md2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")

# VIF
exp_md1 %>% vif()
exp_md2 %>% vif()


# 5 偏回帰係数の信頼区間 --------------------------------------------------------

# ＜ポイント＞
# - 偏回帰係数の解釈では、回帰係数の｢水準/正負｣に加えて｢信頼区間｣の重要な情報となる


# モデル構築(再掲)
res1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)

# 信頼区間の算出
# --- 切片と偏回帰係数
res1 %>% confint(level = 0.95)

# モデルサマリー
list(model1 = res1) %>%
  modelsummary(estimate = "{estimate} [{conf.low}, {conf.high}]",
               statistic = "{std.error}({statistic}){stars}")


# 6 標準偏回帰係数の算出 --------------------------------------------------------

# ＜ポイント＞
# - 単位が異なる複数の説明変数を用いた場合、説明変数間の比較が難しくなる
#   --- 当該説明変数と目的変数の関係は直感的なものとなる
#   --- 各説明変数をZスコアに変換して回帰モデルを構築することで横比較しやすくなる
#   --- 標準偏回帰係数は｢-1 to +1｣で推移する

# ＜結果への影響＞
# - R2などの比率統計量は変わらないが、AICのように水準統計量は影響を受ける
# - 回帰係数の水準と標準誤差は異なるが、t値は同一となる


# データの標準化とデータフレーム化
scsdat <- csdat %>% scale() %>% as.data.frame()

# モデル構築
# --- モデル1： 標準偏回帰係数の算出
# --- モデル2： 通常の回帰係数の算出
res2_1 <- lm(client_n ~ location + trainer_n, data = scsdat)
res2_2 <- lm(client_n ~ location + trainer_n, data = csdat)

# モデルサマリー
list(model_1 = res2_1, model_2 = res2_2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 7 質的変数を含む重回帰分析 ---------------------------------------------------

# ＜ポイント＞
# - 質的変数はそのまま説明変数として加えることができる

# ＜モデル解釈＞
# - モデル1の場合、1は0よりも顧客数(client_n)を+8.014ほど増やす


# データ確認
# --- 質的変数が含まれる（2値データ）
csdat %>%
  select(client_n, location, facility, area, trainer, training_flag, privilege_flag)

# モデル構築
# --- モデル1： 質的変数を含む
# --- モデル2： 質的変数を含まない
# --- モデル3： 質的変数のみ
res3_1 <- lm(client_n ~ location + facility + area + trainer +
             training_flag + privilege_flag, data = csdat)
res3_2 <- lm(client_n ~ location + facility + area + trainer, data = csdat)
res3_3 <- lm(client_n ~ training_flag + privilege_flag, data = csdat)

# モデルサマリー
# --- 質的変数を加えたことで説明力が改善
# --- 質的変数のみのモデルでも一定の説明力があることを確認
list(model_1 = res3_1, model_2 = res3_2, model_3 = res3_3) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")


# 8 AICとBICによるモデル評価 ---------------------------------------------------

# ＜ポイント＞
# - 説明変数の異なるモデル適合度はAICやBICを用いて評価する
#   --- いずれも説明変数の数をペナルティとする（BICはサンプル数も影響を与える）
#   --- いずれも小さいほうが良好と評価
#   --- 参考資料によると、AICの方が使い勝手がよさそう

# ＜注意＞
# - AICやBICの計算定義は複数存在する
# - モデル間で比較する場合は差分を取るが、その値は一致する

# ＜参考＞
# AICとBICの違いは何か？統計解析での特徴比較
# - https://toukeier.hatenablog.com/entry/difference-between-AIC-and-BIC


# モデル構築
# --- モデル1： 説明変数が量的変数のみの回帰モデル
# --- モデル2： 説明変数に質的変数も含めた回帰モデル
res4_1 <- lm(client_n ~ location + facility + area + trainer, data = csdat)
res4_2 <- lm(client_n ~ location + facility + area + trainer + training_flag + privilege_flag, data = csdat)

# AICの算出
res4_1 %>% extractAIC()
res4_2 %>% extractAIC()
res4_1 %>% AIC()
res4_2 %>% AIC()

# BICの算出
res4_1 %>% extractAIC(k = log(nrow(csdat)))
res4_2 %>% extractAIC(k = log(nrow(csdat)))
res4_1 %>% BIC()
res4_2 %>% BIC()

# モデルサマリー
list(model_1 = res4_1, model_2 = res4_2) %>%
  modelsummary(statistic = "{std.error}({statistic}){stars}")

# ＜検証＞
# - AICの差分はフォーミュラが違っても一致するのか？
#   --- 一致する
extractAIC(res4_1)[2] - extractAIC(res4_2)[2]
AIC(res4_1) - AIC(res4_2)


# 9 切片と編回帰係数の検定 ------------------------------------------------------

# t分布
# --- 切片のp値
pt(-0.6949209, 25) * 2

# 設備満足度の偏回帰係数のp値
(1 - pt(3.075611, 25)) * 2

# qtの算出
# --- 下側確率0.025を与えるqtの算出
# --- 上側確率0.025を与えるqtの算出
qt(0.025, 25)
qt(0.975, 25)

# 信頼区間の算出
# --- 信頼区間の下限
# --- 信頼区間の上限
21.640 - 2.059539 * 7.036
21.640 + 2.059539 * 7.036
