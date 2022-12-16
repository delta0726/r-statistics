# ***********************************************************************************************
# Title   : Rによる多変量解析入門
# Chapter : 2 データハンドリング
# Theme   : アンケートデータとID-POSデータのハンドリング
# Date    : 2022/12/17
# Page    : P41 - P65
# URL     : https://www.ohmsha.co.jp/book/9784274222368/
# ***********************************************************************************************


# ＜概要＞
# - 基本的なデータフレーム操作を確認
#   --- インプリケーションが少ないので一部省略


# ＜目次＞
# 0 準備
# 1 データフレームの複数列の平均
# 2 度数分布表とファクターオブジェクト
# 3 データフレームの条件抽出
# 4 欠損値の処理
# 5 データのソート
# 6 データフレームの結合
# 7 数値の置換
# 8 固定長データの操作


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(psych)


#データの読み込み
df <- read_csv("csv/chap02/zinji.csv")

#データの確認
df %>% print()
df %>% glimpse()


# 1 データフレームの複数列の平均 ----------------------------------------------------

# データフレームに変数を追加する
# --- 2つの列の平均値
df %>%
  mutate(Total_mean = rowMeans(select(., Total, Total_lastyear), na.rm = TRUE)) %>%
  glimpse()


# 2 度数分布表とファクターオブジェクト -----------------------------------------------

# データ作成
score <- c(1, 5, 2, 10, 8, 2, 1, 4, 3, 3)

# 度数分布表
score %>% table()


# factor型への変換と度数分布表
fscore <- score %>% factor(levels = seq(0, 10, 1))

# 構造の確認
fscore %>% str()

# 度数分布表
fscore %>% table()


# 3 データフレームの条件抽出 ------------------------------------------------------

# 一致抽出
# --- 男性(M)のみを抽出
df %>% filter(Sex == "M")
df %>% filter(Sex != "F")

# 不等号抽出
df %>% filter(Coordination < 50)
df %>% filter(Coordination <= 50)
df %>% filter(Coordination > 50)
df %>% filter(Coordination >= 50)


# 理論和や理論積による抽出
df %>% filter(Sex == "M" | Age == "proficiency")
df %>% filter(Sex == "M" & Age == "proficiency" & Skill >= 50)
df %>% filter(Sex == "M" & Age %in% c("middle", "proficiency"))


# 4 欠損値の処理 -------------------------------------------------------------------

# 欠損データの読み込み
# --- 欠損箇所が空白の場合
kesson <- read_csv("csv/chap02/na_data.csv")
kesson

# 欠測データの読み込み2
# --- 欠損箇所が数値の場合
# --- データ読み込み関数は引数でNAと認識する文字列や数値を指定することができる
kesson2 <- read_csv("csv/chap02/na_data2.csv")
kesson3 <- read_csv("csv/chap02/na_data2.csv", na = c("999", "9999"),)
kesson2
kesson3

# NAを含む行を削除
kesson3 %>% drop_na()

# NAを含む行を削除
# --- NAのない行番号を取得
# --- 上記の行のみを抽出
cind <- kesson3 %>% complete.cases()
kesson3[cind, ]


# 5 データのソート ------------------------------------------------------------------

# データ並び替え
score <- c(1, 5, 2, 10, 8, 2, 1, 4, 3, 3)
score %>% sort(decreasing = FALSE)

# データロード
sdat <- read.csv("csv/chap02/sort.csv")
sdat

# 列データの並び替え
posi <- sdat$Coordination %>% order()

# データフレームの並び替え
# --- 昇順
# --- 降順
sdat %>% arrange(Coordination)
sdat %>% arrange(desc(Coordination))

# データフレームの複数列のソート
sdat %>% arrange(Coordination, Total)


# 6 データフレームの結合 -------------------------------------------------------------

# マージするデータ
# --- 行数8
# --- 行数4
datA <- read_csv("csv/chap02/marge_a.csv")
datB <- read_csv("csv/chap02/marge_b.csv")

# データ確認
datA %>% print()
datB %>% print()

# 行方向に結合
datA %>% bind_rows(datB)

# データフレーム結合
# --- 共通レコードのみ抽出
# --- 結合元のデータを基準に抽出
datA %>% inner_join(datB, by = "ID")
datA %>% left_join(datB, by = "ID")


# 7 数値の置換 ---------------------------------------------------------------------

# 置換を行うデータ
vec <- c(2, 3, 4, 5, 1, 2, 3, 1, 2)

# 行列作成
tmat <- vec %>% matrix(ncol = 3)
tmat

# 関数whichによる置換対象要素の座標の取得
loc2 <- which(tmat == 2, arr.ind = TRUE)
loc4 <- which(tmat == 4, arr.ind = TRUE)
loc2


# 数値の置換
tmatc <- tmat #変換前のデータ行列のコピーを作成する
tmatc[loc4] <- 2 #tmatで4の座標を選択し，2を代入
tmatc[loc2] <- 4 #tmatで2の座標を選択し，4を代入
tmatc
