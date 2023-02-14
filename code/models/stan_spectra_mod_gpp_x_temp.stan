functions{
  real paretocounts_lpdf(real x, real lambda, real xmin, real xmax, real counts){
    if(lambda != -1)
    return(counts*(log((lambda+1) / ( xmax^(lambda+1) - xmin^(lambda+1))) + lambda*log(x)));
    else
    return(counts*(log(log(xmin) - log(xmax)) + lambda*log(x)));
  }
}
    
data {
	int<lower=0> N;
	real x[N];
	real xmin[N];
	real xmax[N];
	real counts[N];
	real mat_s[N];
	real gpp_s[N];
	int<lower = 1> n_years;
	int<lower = 1, upper = n_years> year[N];
	int<lower = 1> n_sites;
	int<lower = 1, upper = n_sites> site[N];
	int<lower = 1> n_samples;
	int<lower = 1, upper = n_samples> sample[N];
	
}

parameters {
	real beta_mat;
	real beta_gpp;
	real beta_gpp_mat;
	real a;
	real alpha_raw_year[n_years];
	real alpha_raw_site[n_sites];
	real alpha_raw_sample[n_samples];
  real<lower=0> sigma_year;
  real<lower=0> sigma_site;
  real<lower=0> sigma_sample;
}



model {
	// likelihood
	for (i in 1:N){
	  x[i] ~ paretocounts(a + sigma_year*alpha_raw_year[year[i]] + sigma_site*alpha_raw_site[site[i]] + 
	  sigma_sample*alpha_raw_sample[sample[i]] + beta_mat*mat_s[i] + beta_gpp*gpp_s[i] + beta_gpp_mat*gpp_s[i]*mat_s[i], xmin[i], xmax[i], counts[i]); 
	  }
	  
	//priors
  target += normal_lpdf(a|-1.5, 0.2);
  alpha_raw_year ~ std_normal();
  alpha_raw_site ~ std_normal();
  alpha_raw_sample ~ std_normal();
  
  //hyperpriors
  target += exponential_lpdf(sigma_year|7); 
	target += normal_lpdf(beta_mat|0, 0.1);
	target += normal_lpdf(beta_gpp|0, 0.1);
	target += normal_lpdf(beta_gpp_mat|0, 0.1);
	target += exponential_lpdf(sigma_site|7);
	target += exponential_lpdf(sigma_site|7);
	
}

