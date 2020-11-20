# Library
library(tidyverse) 
library(Rsolnp)

# Data
directory <- "~/study_programming/behavior_data_ch6/data/"
simulation_id <- "Qlearning_group_comparison"
csv_simulation_data <- paste0(directory, "simulation_data", simulation_id, ".csv")
data <- read.table(csv_simulation_data, header = T, sep = ",")

# Functions
func_ML <- function(modelfunc, param, gpmap, idxGroup, data) {
  return(modelfunc(param, gpmap, idxGroup, data)$negll)
}

fit_models_SSML <- function(data, modelfunc, nParamList) {
  paramest <- list()
  fvalmin = Inf

  # ラグランジュ未定乗数法による最適化を5回実施
  for (idxrun in 1:5) {
    initparam <- runif(nParamList, 0, 1.0)
    
    # https://cran.r-project.org/web/packages/Rsolnp/Rsolnp.pdf
    res <- solnp(initparam,
                 fun = func_ML,
                 modelfunc = modelfunc,
                 control = list(trace = 0),
                 gpmap = matrix(1:nParamList, nrow = 1),
                 idxGroup = 1,
                 data = data)
    nll <- res$values[length(res$values)]
    
    # 5回のうちベストの結果を記録
    if (nll < fvalmin) {
      paramest <- res$par
      fvalmin <- nll
    }
  }

  return(data.frame(ll = fvalmin, param = t(paramest)))
}

paramfitSSML <- function(modelfunc, data, nParamList) {
  df_model_result <- data %>%
    dplyr::group_by(group, subject) %>%
    dplyr::do(fit_models_SSML(., modelfunc, nParamList))
  return(df_model_result)
}

func_qlearning <- function(param, gpmap, idxGroup = 1, data) {
  alpha <- param[gpmap[idxGroup, 1]]
  beta <- param[gpmap[idxGroup, 2]]
  c <- data$choice
  r <- data$reward
  T <- length(c)
  pA <- numeric(T)
  Q <- matrix(numeric(2*T), nrow=2, ncol=T) # 行動価値観数
  ll <- 0 # Log likelihood
  
  for (t in 1:T) {
    pA[t] <- 1/(1 + exp(-beta * (Q[1, t] - Q[2, t])))
    pA[t] <- max(min(pA[t], 0.9999), 0.0001)
    ll <- ll + (c[t] == 1) * log(pA[t]) +  (c[t] == 2) * log(1 - pA[t])
    
    # update values 
    if (t < T) {
      Q[c[t], t+1] <- Q[c[t], t] + alpha * (r[t] - Q[c[t], t])
      # for unchosen option
      Q[3-c[t], t+1] <- Q[3-c[t], t]
    }
  }
  
  return(list(negll = -ll, Q = Q, pA = pA))
}

# Model fitting
set.seed(1)
resultSSML <- paramfitSSML(func_qlearning, data, nParamList = 2)

# t-test (learning rate)
x1 <- resultSSML$param.1[resultSSML$group==1]
x2 <- resultSSML$param.1[resultSSML$group==2]
t.test(x1, x2)
