
library('dplyr')
library('ggplot2')
ep = c(0.0001, 0.0005, 0.01, 0.02, 0.05, 0.1)
#gdf = numeric(length(ep))
  
logistic_gdf <- function(n = 10000, eps_vector = c(0.0001, 0.0005, 0.01, 0.02, 0.05, 0.1), num_samples = 10){
set.seed(100)
x1 = rnorm(n, 10, 5)
x2 = rnorm(n, 8, 2)
x3 = rnorm(n, 3, 6)
x4 = rnorm(n, 20, 1)
y = sample(c(rep(0,ceiling(n/2)),rep(1,ceiling(n/2))), n, replace = F)
gdf_list = list()
  for (i in 1:length(eps_vector)){
      gdf = numeric(num_samples)
      df = data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
      
      # Logistic Regression Model for True Model
      logit = glm(y ~ ., data = df, family = 'binomial')
      preds = predict(logit, select(df, -y))
      preds = exp(preds) / (1 + exp(preds))
      preds_true_hat = list()
       
      # Flip the weighted coin here for true probabilities num_samples times
      for (flip in 1:length(gdf)){
        y_true = rbinom(n, 1, prob = preds)
        y_true_df = data.frame(y = y_true, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
        logit_ytrue = glm(y ~ ., data = y_true_df, family = 'binomial')
        log_odds = predict(logit_ytrue, select(y_true_df, -y))
        preds_true_hat[[flip]] = exp(log_odds) / (1 + exp(log_odds))
      } 
      
      # Perturb true probabilities 
      epsilon = eps_vector[i]
      eps_norm = rnorm(n,0,1)
      pos_or_neg = sample(c(-1,1), n, replace = TRUE)
      preds_pert = preds + epsilon * eps_norm
      preds_pert_hat = list()
    
      # Flip the weighted coin here for perturbed probabilities num_samples times
      for (flip in 1:length(gdf)){
        y_pert = rbinom(n, 1, prob = preds_pert)
        df_pert = data.frame(y = y_pert, x1 = x1, x2 = x2, x3 = x3, x4 = x4)
        logit_pert = glm(y ~ ., data = df_pert, family = 'binomial') # This is a big question mark
        log_odds = predict(logit_pert, select(df_pert, -y))
        preds_pert_hat[[flip]] = exp(log_odds) / (1 + exp(log_odds))
        gdf[flip] = sum(((preds_pert_hat[[flip]] - preds_true_hat[[flip]]))/((preds_pert - preds)))
      }
      gdf_list[[i]] = gdf
  }
  #ye_hat = abs(preds_pert_hat[[1]] - preds_true_hat[[1]])
  #ye_true = abs(preds_pert - preds)
  #mod = lm(preds_pert_hat[[1]] ~ preds_pert)
  
  gdf_means = sapply(gdf_list, mean)
  #gdf_plot = plot(x = eps_vector, y = gdf_means, xlab = "Epsilon",y = "Mean GDF")
  return(list(gdfgdf_list, gdf_means))
}

# If epsilon is ever greater than 0.1, it seems to throw warnings
ret = logistic_gdf(n = 5000, ep = c(0.001, 0.005, 0.01, 0.02, 0.05), num_samples = 10)

samples = c(1000,5000,10000,20000,50000)
eps01 = numeric(length(samples))
eps02 = numeric(length(samples))
for (i in 1:length(samples)){
  eps01[i] = logistic_gdf(n = samples[i], ep = 0.01, num_samples = 10)[[2]] / samples[i]
  eps02[i] = logistic_gdf(n = samples[i], ep = 0.02, num_samples = 10)[[2]] / samples[i]
}


