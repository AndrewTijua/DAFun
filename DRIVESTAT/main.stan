data {
  int<lower=0> N;
  int<lower=0> P;
  int<lower=0> y[N];
  //int<lower=0> count[N];
  vector<lower=0>[N] days;
  matrix[N,P] X;
  int<lower=1> daycount;
}

transformed data{
  vector<lower=0>[N] failures = to_vector(y);
  vector<lower=0>[N] fail_per_time = failures ./ (days / daycount);
}

parameters {
  //real<lower=0, upper=1> mean_fail_rate[N];
  vector[P] beta;
  //vector<lower=0>[N] sigma;
  real<lower=0> sigma;
}

transformed parameters{
}

model {
  sigma ~ inv_gamma(0.01, 0.01);
  beta ~ normal(0, 10000);
  fail_per_time ~ normal(X*beta, sigma);
  //y ~ binomial(count, mean_fail_rate);
}

generated quantities{
  real fail_post[N] = normal_rng(X*beta, sigma);
}
