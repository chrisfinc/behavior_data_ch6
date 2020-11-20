library(tidyverse)

directory <- "~/study_programming/behavior_data_ch6/data/"
simulation_id <- "Qlearning_group_comparison"
csv_simulation_data <- paste0(directory, "simulation_data", simulation_id, ".csv")
csv_param <- paste0(directory, "trueparam_", simulation_id, ".csv")

n_group <- 2 # グループ数
n_subgroup <- 20 # 各グループの参加数
n_trial <- 100 # 一人の参加者あたりの試行数 

set.seed(4)

# 健常群と疾患群のパラメタ
alphaL_mean <- c(0.3, 0.5) # 学習率(疾患群の学習率は高い)
beta_mean <- c(2.0, 2.0) # 逆温度

alpha_sigma <- c(0.1, 0.1) # 学習率
beta_sigma <- c(0.5, 0.5) # 逆温度

# 報酬確率の系列
pr <- list()
interval <- 50 # 同じ報酬確率が続く試行数
padv <- 0.7 # 良い選択肢の報酬確率
pr[[1]] <- rep(rep(c(padv, 1-padv), n_trial/interval/2), each=interval)
pr[[2]] <- rep(rep(c(1-padv, padv), n_trial/interval/2), each=interval)

df_param <- data.frame()
df_simulation_data <- data.frame()

for (idxGroup in 1:n_group) {
  # 学習率, 逆温度の真値の設定
  alphaL <- rnorm(n = n_subgroup, mean = alphaL_mean[idxGroup], sd = alpha_sigma[idxGroup])
  alphaL <- pmax(pmin(alphaL, 1.0), 0.01) # 0.01以上1.0以下におさめる
  alphaF <- numeric(n_subgroup) # 標準的Q学習モデルではalphaF = 0
  beta <- rnorm(n = n_subgroup, mean = beta_mean[idxGroup], sd = beta_sigma[idxGroup])
  beta <- pmax(pmin(beta, 10.0), 0.01) # 0.01以上10.0以下におさめる

  # パラメータの真値を記録
  df_tmp_param <- data.frame(group = as.factor(idxGroup), subject = as.factor(1:n_subgroup),
                             alphaL = alphaL, alphaF = alphaF, beta = beta)
  df_param <- rbind(df_param, df_tmp_param)

  for (idxSub in 1:n_subgroup) {
    # 変数の初期化
    Q <- matrix(numeric(2 * n_trial), nrow = 2, ncol = n_trial) # 行動価値関数
    c <- numeric(n_trial)
    r <- numeric(n_trial)
    pA <- numeric(n_trial)

    for (t in 1:n_trial) {
      # ソフトマックス関数
      pA[t] <- 1/(1 + exp(-(beta[idxSub] * (Q[1, t] - Q[2, t]))))

      if (runif(1, 0, 1) < pA[t]) {
        # Aを選択
        c[t] <- 1
        r[t] <- as.numeric(runif(1, 0, 1) < pr[[1]][t])
      } else {
        # Bを選択
        c[t] <- 2
        r[t] <- as.numeric(runif(1, 0, 1) < pr[[2]][t])
      }

      # 行動価値の更新
      if (t < n_trial) {
        delta <- r[t] - Q[c[t], t]
        Q[c[t], t+1] <- Q[c[t],t] + alphaL[idxSub] * delta
        Q[3-c[t], t+1] <- (1-alphaF[idxSub]) * Q[3-c[t],t]
      }
    }

    # データフレームに格納する
    df_tmp_sim <- data.frame(group = as.factor(idxGroup),
                             subject = idxSub,
                             trial = 1:n_trial,
                             choice = c,
                             reward = r,
                             Q1 = Q[1,], Q2 = Q[2,],
                             pA = pA)
    df_simulation_data <- rbind(df_simulation_data, df_tmp_sim)
  }
}

write.table(df_simulation_data , file = csv_simulation_data,
            quote = FALSE, sep = ",",row.names = FALSE)

write.table(df_param, file = csv_param,
            quote = FALSE, sep = ",",row.names = FALSE)
