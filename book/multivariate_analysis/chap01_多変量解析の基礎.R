# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 1 多変量解析の基礎
# Theme   : Rによる多変量データの基礎的な処理
# Date    : 2022/12/22
# Page    : P2 - P39
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜概要＞
# - 基本的な統計処理を復習する
# - 心理系分野で使用頻度の高い統計量や評価手法を学ぶ


# ＜目次＞
# 0 準備
# 1 データ概要とデータ確認
# 2 単変量データの基礎分析
# 3 t検定
# 4 多変量データの基礎分析
# 5 多変量データの関係性分析
# 6 カテゴリカル変数の関係性分析
# 7 偏相関係数
# 8 順序カテゴリカル変数の相関係数
# 9 独立な2群のt検定における効果量
# 10 対応のあるt検定における効果量
# 11 その他の効果量と信頼区間


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(gplots)
library(psych)
library(vcd)
library(MBESS)
library(polycor)


#データの読み込み
df <- read_csv("csv/chap01/zinji.csv")


# 1 データ概要とデータ確認 ----------------------------------------------------------

# ＜データ概要＞
# - 10万人の世界規模のソフトウェアメーカーの人事データ
#   --- 試用期間中により800人のデータを抜粋


# ＜データ項目＞
# - ID (ID)
# - 性別 (Sex)
# - 部署 (Department)
# - 年代 (Age)
# - 協調性 (Coordination)
# - 自己主張 (Own)
# - 技能 (Skill)
# - 知識 (Knowledge)
# - ストレス (Stress)
# - 総合 (Total)
# - 昨年総合 (Total_lastyear)


#データの確認
df %>% print()
df %>% glimpse()


# 2 単変量データの基礎分析 ----------------------------------------------------------

# ＜ポイント＞
# - 基本統計量(代表値/散布度)は単変量データの分析で最も良く用いられる


# ヒストグラム
# --- Stress
df %>% ggplot(aes(x = Stress)) + geom_histogram(bins = 20)

# 代表値の算出
# --- 平均値
# --- 中央値
# --- 最頻値
df$Stress %>% mean()
df$Stress %>% median()
df$Age %>% table() %>% .[which.max(.)]

# 散布度の算出
# --- 標準偏差(不偏分散の平方根)
# --- 分散
df$Stress %>% sd()
df$Stress %>% var()
df$Stress %>% var() %>% sqrt()

# 中央値からの平均偏差の算出
# --- ｢平方根｣ではなく、｢偏差の絶対値｣を用いている
df %$% abs(Stress - median(Stress)) %>% mean()


# 2 多変量データの群間比較 --------------------------------------------------------

# ＜ポイント＞
# - 単変量データはファセットを用いてカテゴリごとに分析することも多い
# - ボックスプロットは四分位データと中央値を表現したプロット(P9 図1.4)

# ヒストグラム
# --- 複数基準のファセットを使用
df %>%
  ggplot(aes(x = Coordination, group = Sex)) +
  geom_histogram() +
  facet_wrap(~Age + Sex)

# グループ集計
# --- 群間での分布比較
df %>%
  group_by(Sex) %>%
  summarise(Coordination = mean(Coordination))
df %>%
  group_by(Sex) %>%
  summarise(Coordination = sd(Coordination))

# 箱ヒゲ図
df %>% ggplot(aes(x = Skill)) + geom_boxplot()
df %>% ggplot(aes(x = Sex, y = Coordination)) + geom_boxplot()

# 基本統計量
# --- 四分位数も含めた要約統計量の算出
df$Skill %>% summary()


# 3 t検定 -----------------------------------------------------------------------

# ＜ポイント＞
# - t検定は、2つのカテゴリの母集団において平均値に差があるかを統計的に推測する


# データ確認
df %>% select(Sex, Coordination)
df %>% ggplot(aes(x = Sex, y = Coordination)) + geom_boxplot()

# 等分散のF検定
# --- CoordinationはSexの｢F｣と｢M｣で分散に差はない（帰無仮説）
# --- p値が0.05を上回っている（帰無仮説を棄却できない）
var.test(Coordination ~ Sex, data = df) %>% glance()

# 独立な2群のt検定（等分散を仮定）
# --- CoordinationはSexの｢F｣と｢M｣で平均値に差はない（帰無仮説）
t.test(Coordination ~ Sex, data = df, var.equal = TRUE) %>% glance()

# Welch法によるt検定
t.test(Coordination ~ Sex, data = df)

# 対応のあるt検定
score <- c(df$Total, df$Total_lastyear)
year <- c(rep("year_0", 800), rep("year_m1", 800))
t.test(score ~ year, paired = TRUE)

# 群ごとに信頼区間を描画
gplots::plotmeans(Coordination ~ Sex, data = df, p = 0.95, ylim = c(49, 54))

# 信頼区間の算出
df %>%
  filter(Sex == "F") %>%
  pull(Coordination) %>%
  t.test()
df %>%
  filter(Sex == "M") %>%
  pull(Coordination) %>%
  t.test()


# 4 多変量データの基礎分析 ----------------------------------------------------------

# ＜ポイント＞
# - dplyrによるデータ集計と基準化を確認


# データフレーム作成
df_temp <- df %>% select(Coordination, Own, Skill, Knowledge)

# 列集計
# --- 変数に対する基礎集計
df_temp %>% apply(2, mean)
df_temp %>% apply(2, sd)

# 行集計
# --- 観測対象に対する基礎集計
df_temp %>% apply(1, sum)
df_temp %>% apply(1, sd)

# 多変数の分布を群間で比較
# --- 平均
# --- 標準偏差
df %>%
  select(Sex, Coordination, Own, Skill, Knowledge) %>%
  group_by(Sex) %>%
  summarise_all(mean) %>%
  ungroup()

df %>%
  select(Sex, Coordination, Own, Skill, Knowledge) %>%
  group_by(Sex) %>%
  summarise_all(sd) %>%
  ungroup()

# データの標準化
# --- Zスコア
# --- 偏差値
zscore <- df$Knowledge %>% scale() %>% as.vector()
tscore <- zscore * 10 + 50
tibble(zscore = zscore, tscore = tscore)


# 5 多変量データの関係性分析 -------------------------------------------------------

# ＜ポイント＞
# - 2変量の関係性を分析する場合は散布図や相関係数を用いる
#   --- カテゴリカル情報でファセットを使ってプロットするのが効果的


# 散布図の描画
gino <- df$Skill
chisiki <- df$Knowledge
plot(gino, chisiki, xlab = "Skill", ylab = "Knowledge")

# 散布図行列の描画
df %>%
  select(Coordination, Own, Stress) %>%
  pairs.panels()

# 層別散布図の描画
df %>%
  ggplot(aes(x = Skill, y = Knowledge)) +
  geom_point() +
  facet_wrap(~Department + Age)

# 相関係数の算出
df %$% cor(Coordination, Stress)

# 多変量の相関関係
# --- 相関係数行列
# --- 分散共分散行列
df %>%
  select(Coordination, Own, Skill, Knowledge) %>%
  cor()
df %>%
  select(Coordination, Own, Skill, Knowledge) %>%
  cov()

# 相関係数の検定
# --- t値
# --- p値
result <- df %>%
  select(Coordination, Own, Skill, Knowledge) %>%
  corr.test()
result$t
result$p


# 6 カテゴリカル変数の関係性分析 -----------------------------------------------------

# ＜ポイント＞
# - カテゴリカル変数の関係性は｢連関｣と呼ばれる
# - クラメール連関係数のような定量的な分析も可能

# ＜参考＞
# クラメールの連関係数
# https://corvus-window.com/excel_cramers-v/


# データ確認
df %>% select(Department, Age)
df %>% select(Department, Age) %>% nrow()

# クロス集計表の作成
# --- カウント集計
# --- 比率集計
df %$% table(Department, Age)
df %$% table(Department, Age) %>% sum()

# クロス集計表の作成
# --- 比率集計
df %$% table(Department, Age) %>% prop.table()
df %$% table(Department, Age) %>%
  prop.table() %>%
  sum()

# クロス比率集計
# --- 行方向の割合
df %$% table(Department, Age) %>% prop.table(margin = 1)
df %$% table(Department, Age) %>%
  prop.table(margin = 1) %>%
  apply(1, sum)

# クロス比率集計
# --- 列方向の割合
df %$% table(Department, Age) %>% prop.table(margin = 2)
df %$% table(Department, Age) %>%
  prop.table(margin = 2) %>%
  apply(2, sum)

# 層別クロス集計表の作成
xtabs(~Department + Age + Sex, data = df)

# クラメールの連関係数
# --- vcd::assocstats()を用いる
df %$% table(Department, Age) %>% assocstats()
df %$% table(Department, Age) %>%
  assocstats() %>%
  use_series(cramer)

# クロス集計表に対するカイ二乗検定
df %$% table(Department, Age) %>% chisq.test()

# 残差分析
# --- クロス集計表の逸脱度を確認（1.96以上で5％水準で有意な逸脱）
# --- proficiencyとyoungは部署間の頻度で統計的に有意な偏りがある
df %$% table(Department, Age) %>% prop.table()
df %$% table(Department, Age) %>%
  chisq.test() %>%
  use_series(stdres)


# ＜参考1＞
# クラメールの連関係数の手動計算
statistics <- df %$% table(Department, Age) %>%
  chisq.test() %>%
  use_series(statistic)
n <- df %>% select(Department, Age) %>% nrow()
k <- df %>% select(Department, Age) %>% ncol()
sqrt(statistics / n * (k - 1))

# ＜参考2＞
# クロス集計表と連関係数の関係
# --- 完全連関
# --- 独立したクロス集計表
matrix(c(50, 0, 0, 50), ncol = 2) %>% assocstats()
matrix(c(10, 20, 100, 200), ncol = 2) %>% assocstats()


# 7 偏相関係数 -------------------------------------------------------------------

# ＜ポイント＞
# - 編相関係数とは相関分析対象の裏にある交絡を統制した相関係数


# データ確認
# --- いずれの変数も相関を持つ
df %>%
  select(Total, Total_lastyear, Coordination, Own, Skill, Knowledge) %>%
  pairs.panels()

# 通常の相関係数
df %$% cor(Total, Total_lastyear)

# 偏相関係数
# --- xは相関ペア、yが交絡変数（相関ペアをx, yに入れないように注意）
# --- TotalやTotal_lastyearは統制変数で指定されたもので構成されている
# --- 交絡を取り除くと相関が0.03とゼロ付近になるのは納得的
df %>%
  partial.r(x = c("Total", "Total_lastyear"),
            y = c("Coordination", "Own", "Skill", "Knowledge"))


# 8 順序カテゴリカル変数の相関係数 ---------------------------------------------------

# ＜ポイント＞
# - 順序カテゴリカルデータからピアソン相関係数を算出すると、本来よりも絶対値の小さい値が出力される


# カテゴリカル変数への変換
# --- Total： scat
# --- Skill： gcat
scat <-
  df %>%
    mutate(category = factor(ifelse(Total > mean(Total), 1, 0))) %>%
    pull(category)

gcat <-
  df %>%
    mutate(category = factor(ifelse(Skill >= quantile(Skill, 0.75), 2,
                                    ifelse(Skill < quantile(Skill, 0.25), 0, 1)))) %>%
    pull(category)


# データフレーム作成
# --- tibble()だと次のステップがエラーとなる
df_temp <- data.frame(Total = scat, Skill = gcat, Knowledge = df$Knowledge)
df_temp %>% as_tibble()

# 相関係数の算出
# --- 最尤法で相関行列の算出（ML引数をTRUE）
df_temp %>%
  hetcor(ML = TRUE) %>%
  use_series(correlations)


# 9 独立な2群のt検定における効果量 ----------------------------------------------------

# 関数定義
effectd1 <- function(x1, x2, clevel = 0.95) {
  # 標本サイズ
  n1 <- length(x1)
  n2 <- length(x2)

  # 平均値
  m1 <- mean(x1)
  m2 <- mean(x2)

  # 標本標準偏差
  s1 <- sqrt(mean((x1 - m1)^2))
  s2 <- sqrt(mean((x2 - m2)^2))

  # 母標準偏差の推定値の算出
  sast <- sqrt(((n1 * s1^2) + (n2 * s2^2)) / (n1 + n2 - 2))

  # 効果量の算出
  d <- (m1 - m2) / sast

  # 独立した2群のｔ検定の実行(等分散仮定)と自由度の算出
  rest <- t.test(x1, x2, paired = FALSE, var.equal = TRUE)

  # 効果量の信頼区間の算出
  resconf <- conf.limits.nct(t.value = rest$statistic, df = rest$parameter, conf.level = clevel)
  ll <- resconf$Lower.Limit * sqrt((n1 + n2) / (n1 * n2))
  ul <- resconf$Upper.Limit * sqrt((n1 + n2) / (n1 * n2))
  u3 <- pnorm(d, 0, 1)
  return(list = c(effect = d, clevel = clevel, ci_lower = ll,
                  ci_upper = ul, U3 = u3))
}

# データ準備
fdat <- df %>% filter(Sex == "F") %>% use_series(Coordination)
mdat <- df %>% filter(Sex == "M") %>% use_series(Coordination)

# 効果量の測定
effectd1(x1 = fdat, x2 = mdat, clevel = 0.95)


# 10 対応のあるt検定における効果量 ---------------------------------------------------

# 関数定義
effectd2 <- function(x1, x2, clevel = 0.95) {
  library(MBESS)

  # 標本サイズ
  n <- length(x1 - x2)

  # 差異の平均v.barの算出
  v.bar <- mean(x1 - x2)

  # 差異の不偏分散の平方根svの算出
  sv.p <- sd(x1 - x2)

  # 効果量の算出
  d.p <- v.bar / sv.p

  # 対応のあるｔ検定の実行と自由度の算出
  rest <- t.test(x1, x2, paired = TRUE)

  # 効果量の信頼区間の算出
  resconf <- conf.limits.nct(t.value = rest$statistic,
                             df = rest$parameter, conf.level = clevel)
  ll <- resconf$Lower.Limit / sqrt(n)
  ul <- resconf$Upper.Limit / sqrt(n)
  u3 <- pnorm(d.p, 0, 1)
  return(list = c(effect = d.p, clevel = clevel, ci_lower = ll,
                  ci_upper = ul, U3 = u3))
}


#対応のあるt検定における効果量の算出
effectd2(x1 = df$Total, x2 = df$Total_lastyear, clevel = 0.95)



# 11 その他の効果量と信頼区間 ------------------------------------------------------

effectv <- function(x, y, clevel = 0.95){
  library(vcd)
  library(MBESS)

  #クロス集計表の算出
  tmpcross <- table(x, y)

  #標本サイズの算出
  n <- sum(tmpcross)

  #集計表の行数と列数を算出
  size <- dim(tmpcross)

  #自由度を算出
  dof <- prod(size - 1)

  #カイ二乗値とクラメールVの算出
  resas <- assocstats(tmpcross)
  chi <- resas$chisq_tests["Pearson", "X^2"]
  v <- resas$cramer

  #カイ二乗値を所与としたときの非心度の上限値，下限値を算出
  resconf <- conf.limits.nc.chisq(Chi.Square = chi,
                                  df = dof, conf.level = clevel)

  if (resconf$Lower.Limit > 0) {
    #信頼区間の下限・上限の算出
    ll <- sqrt((dof + resconf$Lower.Limit) / ((min(size) - 1) * n))
    ul <- sqrt((dof + resconf$Upper.Limit) / ((min(size) - 1) * n))
    return(list = c(effect = v, chi = chi, clevel = clevel, ci_lower = ll, ci_upper = ul))
  } else if (resconf$Lower.Limit == 0){
    #信頼区間の下限を0に制約した上で上限を算出
    resconf <- conf.limits.nc.chisq(Chi.Square = chi,
                                    df = dof, conf.level = NULL, alpha.lower = 0, alpha.upper = (1 - clevel) / 2)
    ul <- sqrt((dof + resconf$Upper.Limit) / ((min(size) - 1) * n))
    return(list(c(effect = v, chi = chi, clevel = clevel, ci_lwoer = 0, ci_upper = ul)))
  }
}

#クラメールVに対する信頼区間の算出
effectv(df$Age, df$Department, clevel = .95)
