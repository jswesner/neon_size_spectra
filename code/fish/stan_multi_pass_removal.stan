//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int<lower=0> y[N];
  int<lower=0> Nsites[N];
  int<lower=0> Npass[N];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector q[Npass];
  vector r[Nsite,Npass];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for(site in 1:Nsites){
    for(pass in 1:Npass){
      y[site,pass] ~ binomial(q[pass], r[site,pass]);
      r[site,pass+1] = r[site,pass] - y[site,pass];
    }
  }
  qfirst ~ beta(4,4);
  q[1] = qfirst;
  eta_log ~ unif(0,10);
  eta = exp(eta_log);
  qother ~ beta(4,4);
  for(pass in 2:Npass){
    q[pass] = qother*(eta/(eta+pass-1));
  }
  for(site in 1:Nsites){
    m_log[site] ~ dunif(0, 10);
    m[site] = exp(m_log[site]);
    n[site] ~ poisson(m[site]);
    r[site,1] = n[site];
  }
}

