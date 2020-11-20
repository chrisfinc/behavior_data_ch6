library(tidyverse)
require(rstan)

base_directory <- "~/study_programming/behavior_data_ch6/"
simulation_id <- "Qlearning_group_comparison"
csv_simulation_data <- paste0(base_directory, "data/", "simulation_data", simulation_id, ".csv")
data <- read.table(csv_simulation_data, header = T, sep = ",")

# Data information
nSubject <- 40; nGroup <- 2; nTrial <- 100

# DataList for Stan code
dataList = list(N = nSubject,
                G = rep(c(1,2), times = c(20,20)), # 参加者ごとのグループのインデックス
                nGroup = nGroup,
                flgCommon_alpha = 0, # 1のとき，alphaの集団レベル分布はグループ間で共通
                flgCommon_beta = 1,  # 1のとき，alphaの集団レベル分布はグループ間で共通
                T = nTrial,
                c = matrix(data$choice, nSubject, nTrial, byrow = T),
                r = matrix(data$reward, nSubject, nTrial , byrow = T),
                WBICmode = 0)

# Stan code compile
modelfile <- 'smodel_qlearning_multiple_group.stan'
smodel <- rstan::stan_model(file = paste0(base_directory, modelfile))

# initize chains
initsList <- vector("list", 1)
initsList[[1]] <- list(alpha = runif(nSubject, 0.3, 0.6),
                       beta = runif(nSubject, 0.5, 2.0),
                       mu_p_alpha = runif(nGroup, -0.1, 0.1),
                       sigma_p_alpha = runif(nGroup, 0.5, 1),
                       mu_p_beta = runif(nGroup, -3, -2),
                       sigma_p_beta = runif(nGroup, 0.4, 0.9),
                       eta_alpha = runif(nSubject, -0.2, 0.2),
                       eta_beta = runif(nSubject, -0.2, 0.2))

# Bayse estimation
stanFit <- rstan::sampling(object = smodel,
                           data = dataList,
                           chains = 1,
                           pars = c('mu_p_alpha',
                                    'sigma_p_alpha',
                                    'mu_p_beta',
                                    'sigma_p_beta',
                                    'alpha_p',
                                    'beta_p',
                                    'log_lik',
                                    'alpha_diff',
                                    'beta_diff'),
                           iter = 5000,
                           warmup = 1000,
                           thin = 1,
                           init = initsList)

# Plot
rstan::stan_plot(stanFit,
                 point_est="mean",
                 show_density="T",
                 ci_level = 0.95,
                 pars = c("alpha_p",'alpha_diff'))
