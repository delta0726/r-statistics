# ----------------------------------------------------- #
# ソフトマックス関数のプロット
# ----------------------------------------------------- #

# メモリとグラフのクリア
rm(list=ls())
graphics.off()

# 描画のためのライブラリ読み込み
library(tidyverse)

# 逆温度betaの値
beta_plot <- c(2.0, 1, 5) 

# ソフトマックス関数の定義
smax <- function(x, beta) {
  return(1/(1 + exp( - beta * x)))
}

# 描画のためのウィンドウの生成
#x11()

# プロット
g <-
  data.frame(X = c(-4, 4)) %>%
    ggplot(aes(x = X)) +
    mapply(function(beta, lt) stat_function(fun = smax, args = beta, linetype = lt, size = 1.5),
    beta_plot, c(1,2,3)) +
  ylab('Probability of Action-A P(a = A)') +
  xlab('Q(A) - Q(B)') +
  theme_bw(base_size = 12)

print(g)

# 保存するときは以下をコメントアウト 
# (作業ディレクトリにディレクトリ./figs/を作る必要がある)
# ggsave(file = paste0("./figs/softmax.eps"), g) 
