library(tidyverse)
library(coda)
library(rstan)
library(bayesplot)
library(ggplot2)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)
#options(mc.cores = 1)

q4_2019 <-
  read.csv("data/bb_q4_2019.csv", fileEncoding = "UTF-8-BOM")
q4_2019 <- arrange(q4_2019, desc(days))
q4_2019_ls <- filter(q4_2019, days > 1e2)
q4_2019_c <-
  select(q4_2019_ls,-c(model, failures, days, count, size))
#q4_2019_mc <- mutate_if(q4_2019, is.numeric, scale, scale=FALSE)

eb_beta_afr <-
  fitdistrplus::fitdist((filter(q4_2019_c, afr > 0))$afr, "beta")
mle <- eb_beta_afr$estimate

q4_2019_matrix <-
  model.matrix(afr ~ ., q4_2019_c)

stan_data <-
  list(
    failed = q4_2019_ls$failures,
    total = q4_2019_ls$days,
    N = nrow(q4_2019_ls),
    P = ncol(q4_2019_matrix),
    X = q4_2019_matrix,
    fr_hp = mle
  )

hfr_model <- stan_model(file = "hier_bin.stan")

hfr_model_samples <- sampling(
  hfr_model,
  data = stan_data,
  chains = 7,
  control = list(adapt_delta = 0.9),
  iter = 4000
)

hfr_model_samples

smps <- extract(hfr_model_samples)

ggfr <-
  matrix(ncol = nrow(filter(q4_2019, days < 1e5, days > 1e2)), nrow = 7 * 40000)
for (i in 1:nrow(filter(q4_2019, days < 1e5, days > 1e2))) {
  ggfr[, i] = smps$y_pp[, 18-i]
}
ggfr <- as.data.frame(ggfr)
names(ggfr) <- filter(q4_2019, days < 1e5, days > 1e2)$model
ggfrlong <- reshape2::melt(ggfr)

names(ggfrlong) <- c("model", "posterior.predictive")

histplots <- ggplot(data = ggfrlong) +
  theme_minimal() +
  geom_histogram(aes(x = posterior.predictive, color = model, fill = model), position = "identity", alpha = 0.3, binwidth = 1) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  coord_cartesian(xlim = c(0,100)) +
  labs(title = "Histogram for Low-Data", y = "Occurence in 280,000 draws", x = "Failures in 100,000 drive days")
histplots


ggfr <-
  matrix(ncol = nrow(filter(q4_2019, days > 1e5)), nrow = 7 * 40000)
for (i in 1:nrow(filter(q4_2019, days > 1e5))) {
  ggfr[, i] = smps$y_pp[, i]
}
ggfr <- as.data.frame(ggfr)
names(ggfr) <- filter(q4_2019, days > 1e5)$model
ggfrlong <- reshape2::melt(ggfr)

names(ggfrlong) <- c("model", "posterior.predictive")

histplots <- ggplot(data = ggfrlong) +
  theme_minimal() +
  geom_histogram(aes(x = posterior.predictive, color = model, fill = model), position = "identity", alpha = 0.3, binwidth = 1) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  labs(title = "Histogram for High-Data", y = "Occurence in 280,000 draws", x = "Failures in 100,000 drive days")
histplots



#####
stan_data_fe <-
  list(
    y = q4_2019_ls$failures,
    N = nrow(q4_2019_ls),
    P = ncol(q4_2019_matrix),
    X = q4_2019_matrix,
    days = q4_2019_ls$days,
    daycount = 1e6
  )

fe_model <- stan_model(file = "main.stan")

fe_model_samples <- sampling(
  fe_model,
  data = stan_data_fe,
  chains = 7,
  control = list(adapt_delta = 0.9),
  iter = 6000
)

fe_model_samples
#####
