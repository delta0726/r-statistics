# ***********************************************************************************************
# Title   : Rによる心理学研究法入門
# Chapter : 1 心理学における実践研究
# Date    : 2023/1/11
# Page    : P13 - P34
# URL     : https://www.kitaohji.com/news/n41183.html
# ***********************************************************************************************


# ＜概要＞
# - 基本的なデータ処理に加えて｢信頼性分析｣｢平均の差の検定(t検定)｣を確認する


# ＜目次＞
# 0 準備
# 1 データ概要
# 2 基本統計量の確認
# 3 α係数と尺度得点の算出
# 4 データフレームの行方向の集計
# 5 ヒストグラムの作成
# 6 尺度得点の基本統計量
# 7 変化量についての独立な2群のt検定


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(psych)

# データロード
df <- read_csv("csv/crithin.csv")


# 1 データ概要 -------------------------------------------------------------------

# ＜データ概要＞
# - 批判的思考態度尺度のアンケート結果
#   --- 同じ人に2回実施しており、列で"a"と"b"に分けて管理されている

# ＜データ項目＞
# - name： アンケート対象者のイニシャル
# - class： 合同ゼミの受講生(1)/非受講生(0)
# - r1-r5： 論理的思考への自覚
# - t1-t5： 探求心
# - k1-k5： 客観性
# - s1-s3： 証拠の重視


# データ確認
df %>% print()
df %>% glimpse()

# データ分布
# --- 各データ項目が離散値か文字列なのでカテゴリカル変数に変換して集計
df %>% mutate_all(as.factor) %>% summary()


# 2 基本統計量の確認 --------------------------------------------------------------

# 基本統計量の算出
df %>% describe()

# 基本統計量の算出
# --- classごと（1, 2）
df %>% describeBy(.$class)


# 3 α係数と尺度得点の算出 ----------------------------------------------------------

# α係数の算出
# --- 論理的思考への自覚(1回目)
# --- 探究心(1回目)
# --- 客観性(1回目)
# --- 証拠の重視(1回目)
# --- 18項目合計(1回目)
# --- 論理的思考への自覚(2回目)
# --- 探究心(2回目)
# --- 客観性(2回目)
# --- 証拠の重視(2回目)
# --- 18項目合計(2回目)
df %>% select(matches("r[1-5]a")) %>% alpha()
df %>% select(matches("t[1-5]a")) %>% alpha()
df %>% select(matches("k[1-5]a")) %>% alpha()
df %>% select(matches("s[1-3]a")) %>% alpha()
df %>% select(matches("(r|t|k|s)[1-5]a")) %>% alpha()
df %>% select(matches("r[1-5]b")) %>% alpha()
df %>% select(matches("t[1-5]b")) %>% alpha()
df %>% select(matches("k[1-5]b")) %>% alpha()
df %>% select(matches("s[1-3]b")) %>% alpha()
df %>% select(matches("s[1-3]b")) %>% alpha()
df %>% select(matches("(r|t|k|s)[1-5]b")) %>% alpha()


# 4 データフレームの行方向の集計 ----------------------------------------------

# 得点集計
# --- 尺度得点の算出
# --- 差得点の算出
df_mod <-
  df %>%
    mutate(ronri.a = rowMeans(select(., matches("r[1-5]a"))),
           tankyu.a = rowMeans(select(., matches("t[1-5]a"))),
           kyakkan.a = rowMeans(select(., matches("k[1-5]a"))),
           shoko.a = rowMeans(select(., matches("s[1-5]a"))),
           taido.a = rowMeans(select(., matches("(r|t|k|s)[1-5]a"))),
           ronri.b = rowMeans(select(., matches("r[1-5]b"))),
           tankyu.b = rowMeans(select(., matches("t[1-5]b"))),
           kyakkan.b = rowMeans(select(., matches("k[1-5]b"))),
           shoko.b = rowMeans(select(., matches("s[1-5]b"))),
           taido.b = rowMeans(select(., matches("(r|t|k|s)[1-5]b")))) %>%
    mutate(ronri.diff = ronri.b - ronri.a,
           tankyu.diff = tankyu.b - tankyu.a,
           kyakkan.diff = kyakkan.b - kyakkan.a,
           shoko.diff = shoko.b - shoko.a,
           taido.diff = taido.b - taido.a) %>%
    mutate(class= factor(class))

# データ確認
df_mod %>% glimpse()


# 5 ヒストグラムの作成 --------------------------------------------------------

df_mod %>%
  select(name, class, taido.a, taido.b) %>%
  pivot_longer(-c("name", "class"), names_to = "type", values_to = "score") %>%
  ggplot(aes(x = score)) +
  geom_histogram() +
  facet_wrap(~type + class, scales = "free_y", dir = "v")


# 6 尺度得点の基本統計量 -------------------------------------------------------

# データ確認
df_mod %>% select(ronri.a1:taido.diff)

# 基本統計量
df_mod %>% select(ronri.a1:taido.diff) %>% describe()
df_mod %>% select(class, ronri.a1:taido.diff) %>% describeBy(class)


# 7 変化量についての独立な2群のt検定 ---------------------------------------------

# 独立な2群のt検定
df_mod %$% t.test(ronri.diff ~ class, var.equal = TRUE)
df_mod %$% t.test(tankyu.diff ~ class, var.equal = TRUE)
df_mod %$% t.test(kyakkan.diff ~ class, var.equal = TRUE)
df_mod %$% t.test(shoko.diff ~ class, var.equal = TRUE)
df_mod %$% t.test(taido.diff ~ class, var.equal = TRUE)
