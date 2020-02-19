data {
  int<lower=0> N;
  int<lower=0> P;
  int<lower=0> y[N];
  int<lower=0> count[N];
  vector<lower=0>[N] days;
  matrix[N,P] X;
}

transformed data{
  vector<lower=0>[N] failures = to_vector(y);
  vector<lower=0>[N] fail_per_time = failures ./ days;
}

parameters {
  real<lower=0, upper=1> mean_fail_rate[N];
  vector[P] beta;
  real<lower=0> sigma;
}

transformed parameters{
}

model {
  fail_per_time ~ normal(X*beta, sigma);
  y ~ binomial(count, mean_fail_rate);
}
