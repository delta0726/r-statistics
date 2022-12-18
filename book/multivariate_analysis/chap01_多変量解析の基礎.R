# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 1 多変量解析の基礎
# Theme   : Rによる多変量データの基礎的な処理
# Date    : 2022/12/15
# Page    : P2 - P39
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜目次＞
# 0 準備
# 1 単変量データの基礎分析
# 2 多変量データの群間比較
# 3 t検定
# 4 多変量データの基礎分析
# 5 多変量データの関係性分析
# 6 カテゴリカル変数の関係性分析
# 7 偏相関係数
# 9 効果量


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(gplots)
library(psych)
library(vcd)
library(MBESS)
library(polycor)


#データの読み込み
df <- read_csv("csv/chap01/zinji.csv")

#データの確認
df %>% print()
df %>% glimpse()


# 1 単変量データの基礎分析 ----------------------------------------------------------

# ヒストグラム
df %>% ggplot(aes(x = Stress)) + geom_histogram(bins = 20)

# 代表値の算出
# --- 平均値
# --- 中央値
# --- 最頻値
df$Stress %>% mean()
df$Stress %>% median()
df$Age %>% table() %>% .[which.max(.)]

# 散布度の算出
df$Stress %>% sd()
df$Stress %>% var()

# 中央値からの平均偏差の算出
df %$% abs(Stress - median(Stress)) %>% mean()


# 2 多変量データの群間比較 --------------------------------------------------------

# 群別にヒストグラムを描画
df %>%
  ggplot(aes(x = Coordination, group = Sex)) +
  geom_histogram() +
  facet_wrap(~Age + Sex)

# 群間での分布比較
df %>% group_by(Sex) %>% summarise(Coordination = mean(Coordination))
df %>% group_by(Sex) %>% summarise(Coordination = sd(Coordination))

# 箱ヒゲ図の描画
df %>% ggplot(aes(x = Skill)) + geom_boxplot()
df %>% ggplot(aes(x = Sex, y = Coordination)) + geom_boxplot()

# 四分位数も含めた要約統計量の算出
df$Skill %>% summary()


# 3 t検定 -----------------------------------------------------------------------

# 等分散のF検定
var.test(Coordination ~ Sex, data = df)

# 独立な2群のt検定（等分散を仮定）
t.test(Coordination ~ Sex, data = df, var.equal = TRUE)

# Welch法によるt検定
t.test(Coordination ~ Sex, data = df)

# 対応のあるt検定
score <- c(df$Total, df$Total_lastyear)
year <- c(rep("year_0", 800), rep("year_m1", 800))
t.test(score ~ year, paired = TRUE)

# 群ごとに信頼区間を描画
gplots::plotmeans(Coordination ~ Sex, data = df, p = 0.95, ylim = c(49, 54))

# 信頼区間の算出
df %>% filter(Sex == "F") %>% pull(Coordination) %>% t.test()
df %>% filter(Sex == "M") %>% pull(Coordination) %>% t.test()


# 4 多変量データの基礎分析 ----------------------------------------------------------

# データフレーム作成
df_temp <- df %>% select(Coordination, Own, Skill, Knowledge)

# 列集計
# --- 変数に対する基礎集計
df_temp %>% apply(2, mean)
df_temp %>% apply(2, sd)

# 行集計
# --- 観測対象に対する基礎集計
df_temp %>% apply( 1, sum)
df_temp %>% apply( 1, sd)

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

# 散布図の描画
gino <- df$Skill
chisiki <- df$Knowledge
plot(gino, chisiki, xlab = "Skill", ylab = "Knowledge")

# 散布図行列の描画
df %>% select(Coordination, Own, Stress) %>% pairs.panels()

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
df %>% select(Coordination, Own, Skill, Knowledge) %>% cor()
df %>% select(Coordination, Own, Skill, Knowledge) %>% cov()


# 相関係数の検定
# --- t値
# --- p値
result <- df %>% select(Coordination, Own, Skill, Knowledge) %>% corr.test()
result$t
result$p


# 6 カテゴリカル変数の関係性分析 -----------------------------------------------------

# ＜ポイント＞
# - カテゴリカル変数の関係性は｢連関｣と呼ばれる
# - クロス集計など集計分析に加えて、連関係数のような定量的な分析も可能


# クロス集計表の作成
# --- カウント集計
# --- 比率集計
df %$% table(Department, Age)
df %$% table(Department, Age) %>% prop.table()

# クロス比率集計
# --- 行方向の割合
# --- 列方向の割合
df %$% table(Department, Age) %>% prop.table(margin = 1)
df %$% table(Department, Age) %>% prop.table(margin = 2)

# 層別クロス集計表の作成
xtabs(~Department + Age + Sex, data=df)

# クラメールの連関係数
df %$% table(Department, Age) %>% assocstats()

# クロス集計表と連関係数の関係
# --- 完全連関
# --- 独立したクロス集計表
matrix(c(50, 0, 0, 50), ncol = 2) %>% assocstats()
matrix(c(10, 20, 100, 200), ncol = 2) %>% assocstats()

# クロス集計表に対するカイ二乗検定
df %$% table(Department, Age) %>% chisq.test()


# 7 偏相関係数 -------------------------------------------------------------------

# 通常の相関係数
df %$% cor(Total, Total_lastyear)

# 偏相関係数
# --- Coordination，Own，Skill，Knowledgeを統制した相関係数
df %>% partial.r(c("Total", "Total_lastyear"), c("Coordination", "Own", "Skill", "Knowledge"))


# 8 順序カテゴリカル変数の相関係数 ---------------------------------------------------

# カテゴリカル変数への変換
# --- Total
# --- Skill
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
df_temp <- data.frame(Total = scat, Skill = gcat, Knowledge=df$Knowledge)

# 最尤法で相関行列の算出
df_temp %>% hetcor(ML = TRUE)


# 9 効果量 -------------------------------------------------------------------------

#関数effectd1の読み込み
effectd1 <- function(x1, x2, clevel=0.95)
{
#各群の標本サイズの算出
n1 <- length(x1); n2 <- length(x2)
#各群の平均の算出
m1 <- mean(x1); m2 <- mean(x2)
#各群の標本標準偏差の算出
s1 <- sqrt(mean((x1-m1)^2))
s2 <- sqrt(mean((x2-m2)^2))
#母標準偏差の推定値の算出
sast <- sqrt(((n1*s1^2)+(n2*s2^2))/(n1+n2-2))
#効果量の算出
d <- (m1-m2)/sast
#独立した2群のｔ検定の実行(等分散仮定)と自由度の算出
rest <- t.test(x1, x2, paired=FALSE, var.equal=TRUE)
#効果量の信頼区間の算出
resconf <- conf.limits.nct(t.value=rest$statistic,
df=rest$parameter, conf.level=clevel)
ll <- resconf$Lower.Limit*sqrt((n1+n2)/(n1*n2))
ul <- resconf$Upper.Limit*sqrt((n1+n2)/(n1*n2))
u3 <- pnorm(d, 0, 1)
return(list=c(効果量=d, 信頼水準=clevel, 区間下限=ll,
区間上限=ul, U3=u3))
}

#独立な２群のt検定に対応した効果量の算出
#事前に関数effectd1をRに読み込んでおく
fdat <- df$Coordination[df$Sex=="F"]
mdat <- df$Coordination[df$Sex=="M"]
effectd1(fdat, mdat, clevel=0.95)

#関数effectd2の読みこみ
#本関数は欠測値には対応していません。
effectd2 <- function(x1, x2, clevel=0.95)
{
library(MBESS)
#標本サイズの算出
n <- length(x1-x2)
#差異の平均v.barの算出
v.bar <- mean(x1-x2)
#差異の不偏分散の平方根svの算出
sv.p <- sd(x1-x2)
#効果量の算出
d.p <- v.bar/sv.p
#対応のあるｔ検定の実行と自由度の算出
rest <- t.test(x1, x2, paired=TRUE)
#効果量の信頼区間の算出
resconf <- conf.limits.nct(t.value=rest$statistic,
df=rest$parameter, conf.level=clevel)
ll <- resconf$Lower.Limit/sqrt(n)
ul <- resconf$Upper.Limit/sqrt(n)
u3 <- pnorm(d.p, 0, 1)
return(list=c(効果量=d.p, 信頼水準=clevel, 区間下限=ll,
区間上限=ul, U3=u3))
}

#対応のあるt検定における効果量の算出
effectd2(df$Total, df$昨年Total, clevel=0.95)


#相関係数の信頼区間の算出
corkekka2 <- corr.test(df[, kjs], alpha=0.05)
print(corkekka2, short=FALSE)


effectv <- function(x, y, clevel=0.95)
{
library(vcd)
library(MBESS)
#クロス集計表の算出
tmpcross <- table(x, y)
#標本サイズの算出
n <- sum(tmpcross)
#集計表の行数と列数を算出
size <- dim(tmpcross)
#自由度を算出
dof <- prod(size-1)
#カイ二乗値とクラメールVの算出
resas <- assocstats(tmpcross)
chi <- resas$chisq_tests["Pearson", "X^2"]
v <- resas$cramer
#カイ二乗値を所与としたときの非心度の上限値，下限値を算出
resconf <- conf.limits.nc.chisq(Chi.Square=chi,
df=dof, conf.level=clevel)

if(resconf$Lower.Limit>0)#下限値λLがを超える領域に入った場合
{
#信頼区間の下限・上限の算出
ll <- sqrt((dof+resconf$Lower.Limit)/((min(size)-1)*n))
ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
return(list=c(効果量V=v, カイ二乗値=chi, 信頼水準=clevel,
区間下限=ll, 区間上限=ul))
}else if(resconf$Lower.Limit==0) #下限値λlが負値になった場合
{
#信頼区間の下限を0に制約した上で上限を算出
resconf <- conf.limits.nc.chisq(Chi.Square=chi,
df=dof, conf.level=NULL, alpha.lower=0, alpha.upper=(1-clevel)/2)
ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
return(list=list(
"下限値λLが負値になったので信頼区間の下限値を0にしました。",
c(効果量V=v, カイ二乗値=chi, 信頼水準=clevel, 区間下限=0,
区間上限=ul)))
}
}

#クラメールVに対する信頼区間の算出
effectv(df$Age, df$Department, clevel=.95)


