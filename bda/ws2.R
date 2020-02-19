library(rjags)

set.seed(1)

data <- rpois(100, 1.39)

post <- function(theta, data) {
  lh <- prod(dpois(data, theta))
  prior <- dgamma(theta, shape = 3, rate = 1)
  post <- prior * lh
  return(post)
}

pmean <- 1.39

n.gen <- 1e5
n.sample <- 1e4

prop.dist.gen <- rexp(n.gen, pmean)
importance.weights <- numeric(n.gen)

for (i in 1:n.gen) {
  importance.weights[i] <-
    post(theta = prop.dist.gen[i], data = data) / dexp(prop.dist.gen[i], pmean)
}

norm.weights <- importance.weights / sum(importance.weights)

samples <-
  sample(prop.dist.gen, n.sample, replace = TRUE, prob = norm.weights)

mean(samples)
sd(samples)
quantile(samples, probs = c(0.025, 0.975))



a = 1 / 2
b = 1e-3

model_string <- "model{

# Likelihood
for (i in 1:N){
y[i] ~ dpois(theta)
}

# Prior
theta~dgamma(a, b)
}"

model = jags.model(
  textConnection(model_string),
  n.chains = 5,
  data = list(
    y = data,
    N = length(data),
    a = a,
    b = b
  )
)

update(model, 1000, progress.bar = "none")

# Running the model
samp = coda.samples(
  model,
  variable.names = c("theta"),
  n.iter = 20000,
  progress.bar = "none"
)

summary(samp)
plot(samp)




d_data <- c(rep(1, 15), rep(0, 5))

alpha = 9.2
beta = 13.8
alphaw = 12
betaw = 3

model_string <- "model{

# Likelihood
for (i in 1:N){
d[i] ~ dbinom(theta, 1)
}

# Prior
theta~dbeta(alpha[r], beta[r])
r ~ dcat(p)

P.crit<-step(theta-0.6)
}"

model = jags.model(
  textConnection(model_string),
  n.chains = 5,
  data = list(
    d = d_data,
    N = length(d_data),
    alpha = c(alpha, alphaw),
    beta = c(beta, betaw),
    p = c(0.95,0.05)
  )
)

update(model, 1000, progress.bar = "none")

# Running the model
samp = coda.samples(
  model,
  variable.names = c("theta", "P.crit"),
  n.iter = 20000,
  progress.bar = "none"
)

summary(samp)
plot(samp)
#autocorr.plot(samp)
gelman.plot(samp)

p_better <- 1 - pbinom(24, size = 40, prob = 0.6433)
p_better
