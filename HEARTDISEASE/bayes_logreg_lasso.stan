data {
  int<lower=0> N; //matrix rows
  int<lower=0> P; //matrix columns
  matrix[N, P] X; //model matrix
  int<lower=0, upper=1>y[N]; //target variable
  real lambda; //penaliser
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[P] beta;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for(i in 1:P){
   target += -lambda * fabs(beta[i]);
  }
  //beta ~ double_exponential(0, lambda);
  y ~ bernoulli_logit(X * beta);
}

