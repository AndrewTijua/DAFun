library(tidyverse)
library(data.table)
library(HDInterval)

library(modeest)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)
options(mc.cores = 1)

library(rstanarm)
library(bayesplot)
library(coda)

library(caret)

library(FactoMineR)
library(factoextra)

hd_data <- fread("data/heart.csv")

factor_variables <-
  c("cp", "fbs", "restecg", "exang", "thal", "sex")

hd_data[, (factor_variables) := lapply(.SD, factor), .SDcols = factor_variables]

hd_glm <-
  glm(data = hd_data, target ~ ., family = binomial(link = "logit"))

summary(hd_glm)

p_resp <- predict(hd_glm, hd_data, type = "response")
acc_glm <- (nrow(hd_data) - sum(abs(round(p_resp) - hd_data$target)))/nrow(hd_data)
table(round(p_resp), hd_data$target)

X <- model.matrix(target ~ ., data = hd_data)

stan_lasso <- stan_model(file = 'bayes_logreg_lasso.stan')

stan_lasso_data <- 
  list(
    N = nrow(hd_data),
    P = ncol(X),
    y = hd_data$target,
    X = X,
    lambda = 1e2
  )

stan_lasso_samples <- sampling(stan_lasso, data = stan_lasso_data, chains = 7, control = list(adapt_delta = 0.8), iter = 2e3)
stan_lasso_samples
stan_lasso_samples_sm <- extract(stan_lasso_samples)$beta
colnames(stan_lasso_samples_sm) <- colnames(X)
stan_MAP_est <- apply(stan_lasso_samples_sm, 2, mlv, method = "grenander", p = 2)
stan_MAP_est

stan_lasso_p <- plogis(X %*% stan_M_est)
acc_stan <- (nrow(hd_data) - sum(abs(round(stan_lasso_p) - hd_data$target)))/nrow(hd_data)
table(round(stan_lasso_p), hd_data$target)

stanarm_cl <- stan_glm(target ~ ., data = as.data.frame(hd_data), family = binomial(link = "logit"))
stanarm_cl

acc_stanarm <- (nrow(hd_data) - sum(abs(round(fitted(stanarm_cl)) - hd_data$target)))/nrow(hd_data)
table(round(fitted(stanarm_cl)), hd_data$target)


tc <- trainControl(method = "cv", number = 10)
caret_model <- train(as.factor(target) ~ ., data = hd_data, method = "parRF", trControl = tc)
caret_model
