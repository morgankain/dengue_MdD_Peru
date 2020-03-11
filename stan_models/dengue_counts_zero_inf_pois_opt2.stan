
data {

  int<lower=0> N;               // number of observations of Dengue
  int<lower=0> K_den;
  int<lower=0> K_obs;
  int<lower=0> K_mos_theta;
  int<lower=0> K_mos_lambda;
  int<lower=0> y[N];            // Dengue case counts
  matrix[N, K_den] x;
  matrix[N, K_mos_theta] z_theta;
  matrix[N, K_mos_lambda] z_lambda;
  matrix[N, K_obs] x_obs;
  vector[N] offset;

  int<lower=0> N_pred;
  matrix[N_pred, K_den] x_pred;
  matrix[N_pred, K_mos_theta] z_theta_pred;
  matrix[N_pred, K_mos_lambda] z_lambda_pred;
  matrix[N_pred, K_obs] x_obs_pred;
  vector[N_pred] offset_pred;

}

parameters {

  real alpha_theta;        
  real alpha_lambda; 
  vector[K_den] beta_theta;   
  vector[K_den] beta_lambda;
  vector[K_obs] beta_obs;      
  vector[K_mos_theta] beta_theta_mos; 
  vector[K_mos_lambda] beta_lambda_mos; 

}

transformed parameters {

  vector[N] theta;
  vector[N] lambda_log;

 // vector[N_pred] theta_pred;
 // vector[N_pred] lambda_log_pred;
    
  theta      = inv_logit(alpha_theta + x * beta_theta + z_theta * beta_theta_mos);

  lambda_log = alpha_lambda + x * beta_lambda + z_lambda * beta_lambda_mos + x_obs * beta_obs + offset; 

 // theta_pred = inv_logit(alpha_theta + x_pred * beta_theta + z_theta_pred * beta_theta_mos);
 // lambda_log_pred = alpha_lambda + x_pred * beta_lambda + z_lambda_pred * beta_lambda_mos + x_obs_pred * beta_obs + offset_pred;

}

model {

// priors. Break up the beta's according to some other independent data sources...

// intercept
   alpha_theta ~ normal(0, 3) ;
   alpha_lambda ~ normal(0, 3) ;

// slopes
   beta_theta ~ normal(0, 3) ;
   beta_lambda ~ normal(0, 3) ;
   beta_obs ~ normal(0, 3) ;
   beta_theta_mos ~ normal(0, 3) ;    
   beta_lambda_mos ~ normal(0, 3) ; 

// modify the likelihood
   for(n in 1:N) {
    
if (y[n] == 0) {
     
      target += log_sum_exp(bernoulli_lpmf(1 | theta[n]),                 
                             bernoulli_lpmf(0 | theta[n])
                           + poisson_log_lpmf(y[n] | lambda_log[n]));

    } else {

      target += bernoulli_lpmf(0 | theta[n])                              
                  + poisson_log_lpmf(y[n] | lambda_log[n]);

    }
  }
}

generated quantities{


}

