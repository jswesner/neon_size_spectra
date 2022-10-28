
data {
  int<lower=1> N;
  int<lower=0> c1;
  int<lower=0> n_ij1;
}

parameters {
  real<lower=0, upper=1> q_ij;
}

// transformed data {
//   real<lower = 0> n_ij1;
//   n_ij1 ~ poisson(1000);
// }

model {
  c1 ~ binomial(n_ij1, q_ij);
  q_ij ~ beta(5,5);
  n_ij1 ~ poisson(1000);
}

