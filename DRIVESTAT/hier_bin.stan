data {
  int<lower=0> N;
  int<lower=0> P;
  int<lower=0> failed[N];
  int<lower=0> total[N];
  matrix[N,P] X;
  vector<lower=0>[2] fr_hp;
}

parameters {
  vector<lower=0,upper=1>[N] failure_rate;
  vector<lower=0,upper=1000>[N] alpha;
  vector<lower=0,upper=1000>[N] beta;
  vector<lower=-1000, upper=1000>[N] a;
  vector<lower=-1000, upper=1000>[N] b;
  vector<lower=0>[N] sigma;
}

model {
  failed ~ binomial(total, failure_rate);
  failure_rate ~ beta(alpha, beta);
  alpha ~ normal(a, sigma[1]);
  beta ~ normal(b, sigma[2]);
  sigma ~ inv_gamma(fr_hp[1], fr_hp[2]);
}

generated quantities{
  int<lower=0> y_pp[N] = binomial_rng(100000, failure_rate);
}
